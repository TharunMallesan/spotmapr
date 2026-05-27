#' Load state and district boundary GeoDataFrames
#'
#' Uses bundled FlatGeobuf files by default. Pass custom paths to override.
#'
#' @param state_shp Optional custom state boundary file path.
#' @param district_shp Optional custom district boundary file path.
#' @return A named list with `states`, `districts`, `state_name_col`,
#'   `district_name_col`.
#' @keywords internal
load_boundaries <- function(state_shp = NULL, district_shp = NULL) {
  state_path <- state_shp %||%
    system.file("extdata", "state_boundary_lite.fgb", package = "spotmapr")
  district_path <- district_shp %||%
    system.file("extdata", "district_boundary_lite.fgb", package = "spotmapr")

  states <- .ensure_wgs84(sf::st_read(state_path, quiet = TRUE))
  districts <- .ensure_wgs84(sf::st_read(district_path, quiet = TRUE))

  state_candidates <- c("STATE", "STATE_UT", "ST_NM", "STATE_NAME",
                         "STNAME", "NAME")
  district_candidates <- c("DISTRICT", "DIST_NEW", "DISTRICT_N",
                            "DT_NAME", "DIST_ROMAN", "dtname", "NAME")

  state_name_col <- .find_name_col(states, state_candidates, "state")
  district_name_col <- .find_name_col(districts, district_candidates, "district")

  list(states = states, districts = districts,
       state_name_col = state_name_col,
       district_name_col = district_name_col)
}


.find_name_col <- function(sf_obj, candidates, label) {
  cols <- names(sf_obj)
  hit <- intersect(candidates, cols)
  if (length(hit) == 0)
    stop("No ", label, " name column found. Columns present: ",
         paste(cols, collapse = ", "), call. = FALSE)
  hit[1]
}


.ensure_wgs84 <- function(sf_obj) {
  if (is.na(sf::st_crs(sf_obj))) {
    sf_obj <- sf::st_set_crs(sf_obj, 4326)
  } else if (sf::st_crs(sf_obj)$epsg != 4326) {
    sf_obj <- sf::st_transform(sf_obj, 4326)
  }
  sf_obj <- sf::st_make_valid(sf_obj)
  sf_obj
}


#' Build an outline of India from state boundaries
#' @keywords internal
build_india_outline <- function(states) {
  geom <- sf::st_union(states)
  sf::st_sf(geometry = geom, crs = sf::st_crs(states))
}


#' Spatial join: attach state and district names to each point
#' @keywords internal
spatial_join <- function(df, lat_col, lon_col, states, districts,
                         state_name_col, district_name_col) {
  points <- sf::st_as_sf(df,
                          coords = c(lon_col, lat_col),
                          crs = 4326,
                          remove = FALSE)

  # Join with districts
  joined_d <- sf::st_join(points,
                           districts[, c(district_name_col, "geometry")],
                           join = sf::st_within,
                           left = TRUE)

  # Join with states
  joined_s <- sf::st_join(points,
                           states[, c(state_name_col, "geometry")],
                           join = sf::st_within,
                           left = TRUE)

  # If the join introduced a duplicate column (e.g., NAME from both),

  # use the suffixed version
  result <- points
  dcol <- if (paste0(district_name_col, ".y") %in% names(joined_d)) {
    paste0(district_name_col, ".y")
  } else {
    district_name_col
  }
  scol <- if (paste0(state_name_col, ".y") %in% names(joined_s)) {
    paste0(state_name_col, ".y")
  } else {
    state_name_col
  }

  result[[district_name_col]] <- joined_d[[dcol]]
  result[[state_name_col]] <- joined_s[[scol]]
  result
}


#' Determine map mode: "districts", "states", or "india"
#' @keywords internal
determine_mode <- function(points_cases, district_name_col, state_name_col,
                           count_cutoff = 2L) {
  affected_districts <- unique(stats::na.omit(points_cases[[district_name_col]]))
  unique_states <- unique(stats::na.omit(points_cases[[state_name_col]]))

  n_dist <- length(affected_districts)
  n_states <- length(unique_states)

  if (n_dist > 0 && n_dist <= count_cutoff) {
    mode <- "districts"
  } else if (n_states > count_cutoff) {
    mode <- "india"
  } else {
    mode <- "states"
  }

  bounds <- sf::st_bbox(points_cases)
  list(mode = mode, affected_districts = affected_districts,
       unique_states = unique_states, bounds = bounds)
}


#' Crop an sf object to a bounding box with margin
#' @keywords internal
crop_geodataframe <- function(sf_obj, bounds, margin = 1.0) {
  if (is.null(sf_obj) || nrow(sf_obj) == 0) return(sf_obj)
  if (any(!is.finite(bounds))) return(sf_obj)

  crop_box <- sf::st_bbox(c(
    xmin = bounds["xmin"] - margin,
    ymin = bounds["ymin"] - margin,
    xmax = bounds["xmax"] + margin,
    ymax = bounds["ymax"] + margin
  ), crs = sf::st_crs(sf_obj))

  cropped <- tryCatch(
    sf::st_crop(sf_obj, crop_box),
    error = function(e) sf_obj
  )
  if (nrow(cropped) == 0) sf_obj else cropped
}
