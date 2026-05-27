#' Build an interactive epidemiological spot map for India
#'
#' @param data Path to a CSV/Excel/TSV file, or a data.frame.
#' @param state_shp Optional custom state boundary file path.
#' @param district_shp Optional custom district boundary file path.
#' @param lat_col Latitude column name. Auto-detected when NULL.
#' @param lon_col Longitude column name. Auto-detected when NULL.
#' @param outcome_col Outcome column name. Auto-detected when NULL.
#' @param case_value Value representing a case. Auto-detected when NULL.
#' @param count_cutoff District count threshold for zoom level (default 2).
#' @param margin_deg Padding in degrees around bounding box (default 1.0).
#' @param cluster_color Hex colour for dot-density clusters.
#' @param case_color Hex colour for case pins.
#' @param control_color Hex colour for control pins.
#' @param output Path to save the HTML file. If NULL, returns the leaflet widget.
#' @return A leaflet widget (invisibly if saved to file).
#' @export
#' @examples
#' \dontrun{
#' spot_map("cases.csv", output = "map.html")
#'
#' # Or with a data.frame
#' spot_map(my_df, lat_col = "lat", lon_col = "lon",
#'          outcome_col = "status", case_value = "case")
#' }
spot_map <- function(data,
                     state_shp = NULL,
                     district_shp = NULL,
                     lat_col = NULL,
                     lon_col = NULL,
                     outcome_col = NULL,
                     case_value = NULL,
                     count_cutoff = 2L,
                     margin_deg = 1.0,
                     cluster_color = "#E85252",
                     case_color = "#D55757",
                     control_color = "#7676E7",
                     output = NULL) {

  # 1. Load data
  if (is.character(data)) {
    df <- load_data(data)
  } else if (is.data.frame(data)) {
    df <- data
  } else {
    stop("data must be a file path (character) or a data.frame.", call. = FALSE)
  }

  # 2. Detect columns
  ll <- detect_lat_lon(df, lat_col, lon_col)
  lat_col <- ll$lat
  lon_col <- ll$lon

  oc <- detect_outcome(df, outcome_col, case_value)
  outcome_col <- oc$outcome_col
  case_value <- oc$case_value

  df[["_outcome_norm"]] <- tolower(trimws(as.character(df[[outcome_col]])))
  df[["_outcome_norm"]][df[["_outcome_norm"]] %in% c("na", "nan")] <- NA

  # 3. Load boundaries
  bnd <- load_boundaries(state_shp, district_shp)
  states <- bnd$states
  districts <- bnd$districts
  state_name_col <- bnd$state_name_col
  district_name_col <- bnd$district_name_col

  # 4. Spatial join
  points_joined <- spatial_join(df, lat_col, lon_col, states, districts,
                                 state_name_col, district_name_col)

  # 5. Split cases / controls
  is_case <- points_joined[["_outcome_norm"]] == case_value
  is_case[is.na(is_case)] <- FALSE
  points_cases <- points_joined[is_case, ]
  points_controls <- points_joined[!is_case, ]

  if (nrow(points_cases) == 0)
    stop("No case points found with outcome value '", case_value, "'.",
         call. = FALSE)

  # 6. Determine mode + crop
  mode_info <- determine_mode(points_cases, district_name_col, state_name_col,
                               count_cutoff)
  mode <- mode_info$mode
  bounds <- mode_info$bounds

  india_outline <- build_india_outline(states)
  affected_states <- states[states[[state_name_col]] %in% mode_info$unique_states, ]
  affected_districts <- districts[districts[[district_name_col]] %in%
                                    mode_info$affected_districts, ]

  india_sub <- crop_geodataframe(india_outline, bounds, margin_deg)
  states_sub <- crop_geodataframe(affected_states, bounds, margin_deg)
  districts_sub <- crop_geodataframe(affected_districts, bounds, margin_deg)

  # 7. Build leaflet map
  coords_cases <- sf::st_coordinates(points_cases)
  center_lat <- mean(coords_cases[, 2], na.rm = TRUE)
  center_lon <- mean(coords_cases[, 1], na.rm = TRUE)

  m <- leaflet::leaflet(
    width = "100%", height = "100%",
    options = leaflet::leafletOptions(zoomSnap = 0.1, zoomDelta = 0.1)
  ) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::setView(lng = center_lon, lat = center_lat, zoom = 5)

  # 8. Boundary layers
  m <- m |>
    leaflet::addGeoJSON(
      geojson = sf_to_geojson(india_sub),
      weight = 1, color = "#000000", fillOpacity = 0.0, opacity = 0.5,
      group = "India Border"
    ) |>
    leaflet::addGeoJSON(
      geojson = sf_to_geojson(states_sub),
      weight = 1.5, color = "#4B0082", fillOpacity = 0.05, opacity = 0.7,
      group = "Affected States"
    ) |>
    leaflet::addGeoJSON(
      geojson = sf_to_geojson(districts_sub),
      weight = 1, color = "#000000", fillOpacity = 0.01, opacity = 1.0,
      group = "Affected Districts"
    )

  # 9. Auto-zoom
  n_states_uniq <- length(mode_info$unique_states)
  n_dist_uniq <- length(mode_info$affected_districts)

  if (n_states_uniq > 1) {
    target_bounds <- if (nrow(affected_states) > 0) sf::st_bbox(affected_states) else bounds
  } else if (n_states_uniq == 1) {
    if (n_dist_uniq > 1) {
      target_bounds <- if (nrow(affected_states) > 0) sf::st_bbox(affected_states) else bounds
    } else {
      target_bounds <- if (nrow(affected_districts) > 0) sf::st_bbox(affected_districts) else bounds
    }
  } else {
    target_bounds <- bounds
  }

  if (all(is.finite(target_bounds))) {
    span_x <- target_bounds["xmax"] - target_bounds["xmin"]
    span_y <- target_bounds["ymax"] - target_bounds["ymin"]
    buf <- 0.05
    m <- m |> leaflet::fitBounds(
      lng1 = target_bounds["xmin"] - span_x * buf,
      lat1 = target_bounds["ymin"] - span_y * buf,
      lng2 = target_bounds["xmax"] + span_x * buf,
      lat2 = target_bounds["ymax"] + span_y * buf
    )
  }

  # 10. Marker layers — dot density (clustered)
  case_coords <- sf::st_coordinates(points_cases)
  case_popups <- paste0(
    "<b>Type:</b> Case<br>",
    "<b>State:</b> ", points_cases[[state_name_col]], "<br>",
    "<b>District:</b> ", points_cases[[district_name_col]]
  )

  m <- m |>
    leaflet::addMarkers(
      lng = case_coords[, 1], lat = case_coords[, 2],
      popup = case_popups,
      group = "Dot Density Layer",
      clusterOptions = leaflet::markerClusterOptions(
        disableClusteringAtZoom = 15,
        spiderfyOnMaxZoom = TRUE,
        showCoverageOnHover = FALSE,
        maxClusterRadius = 60,
        singleMarkerMode = TRUE
      )
    )

  # Case pins
  m <- m |>
    leaflet::addCircleMarkers(
      lng = case_coords[, 1], lat = case_coords[, 2],
      popup = case_popups,
      radius = 5, color = case_color, fillColor = case_color,
      fillOpacity = 0.8, weight = 1,
      group = "Spot Map - Cases"
    )

  # Control pins
  if (nrow(points_controls) > 0) {
    ctrl_coords <- sf::st_coordinates(points_controls)
    ctrl_popups <- paste0(
      "<b>Type:</b> Control<br>",
      "<b>State:</b> ", points_controls[[state_name_col]], "<br>",
      "<b>District:</b> ", points_controls[[district_name_col]]
    )
    m <- m |>
      leaflet::addCircleMarkers(
        lng = ctrl_coords[, 1], lat = ctrl_coords[, 2],
        popup = ctrl_popups,
        radius = 5, color = control_color, fillColor = control_color,
        fillOpacity = 0.8, weight = 1,
        group = "Spot Map - Controls"
      )
  } else {
    # Add empty group so JS doesn't break
    m <- m |> leaflet::addCircleMarkers(
      lng = numeric(0), lat = numeric(0),
      group = "Spot Map - Controls"
    )
  }

  # 11. Hide pin layers by default (dots mode is default)
  m <- m |>
    leaflet::hideGroup("Spot Map - Cases") |>
    leaflet::hideGroup("Spot Map - Controls")

  # 12. Sidebar HTML + JS
  sidebar <- build_sidebar_html(
    n_cases = nrow(points_cases),
    n_controls = nrow(points_controls),
    mode = mode,
    cluster_color = cluster_color,
    case_color = case_color,
    control_color = control_color
  )

  sidebar_js <- build_sidebar_js(cluster_color, case_color, control_color)

  # Full-page styling so the map fills the browser window
  fullpage_css <- htmltools::tags$style(htmltools::HTML("
    html, body { margin: 0; padding: 0; width: 100%; height: 100%; overflow: hidden; }
    #htmlwidget_container { width: 100%; height: 100%; }
    .leaflet.html-widget { width: 100% !important; height: 100% !important; }
  "))

  m <- m |>
    htmlwidgets::prependContent(fullpage_css) |>
    htmlwidgets::prependContent(sidebar) |>
    htmlwidgets::onRender(sidebar_js)

  # 13. Save or return
  if (!is.null(output)) {
    out_path <- normalizePath(output, mustWork = FALSE)
    # Try selfcontained first; fall back to non-selfcontained if pandoc missing
    saved <- tryCatch({
      htmlwidgets::saveWidget(m, file = out_path, selfcontained = TRUE)
      TRUE
    }, error = function(e) {
      if (grepl("pandoc", conditionMessage(e), ignore.case = TRUE)) {
        # Save with external dependencies in a lib folder
        htmlwidgets::saveWidget(m, file = out_path, selfcontained = FALSE)
        TRUE
      } else {
        stop(e)
      }
    })
    message("Map saved to: ", output)
    invisible(m)
  } else {
    m
  }
}


#' Convert an sf object to GeoJSON string
#' @keywords internal
sf_to_geojson <- function(sf_obj) {
  if (is.null(sf_obj) || nrow(sf_obj) == 0) return("{}")
  tmp <- tempfile(fileext = ".geojson")
  sf::st_write(sf_obj, tmp, driver = "GeoJSON", quiet = TRUE)
  paste(readLines(tmp, warn = FALSE), collapse = "\n")
}
