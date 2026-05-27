#' Build the sidebar HTML/CSS/JS for the SpotMap
#' @keywords internal
build_sidebar_html <- function(n_cases, n_controls, mode,
                                cluster_color, case_color, control_color) {
  htmltools::HTML(sprintf('
<style>
#sidebar-toggle-btn {
    position: fixed;
    top: 14px;
    right: 14px;
    z-index: 10000;
    width: 42px;
    height: 42px;
    background: #fff;
    border-radius: 8px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.18);
    display: flex;
    align-items: center;
    justify-content: center;
    cursor: pointer;
    user-select: none;
    transition: all 0.15s ease;
}
#sidebar-toggle-btn:hover { background: #f5f7fa; transform: scale(1.05); }
#sidebar-toggle-btn span {
    display: block;
    width: 22px;
    height: 2.5px;
    background: #2c3e50;
    margin: 3px 0;
    border-radius: 2px;
}

#map-sidebar {
    position: fixed;
    top: 14px;
    right: 14px;
    bottom: 14px;
    width: 300px;
    z-index: 9999;
    background: #ffffff;
    border-radius: 12px;
    box-shadow: 0 4px 16px rgba(0,0,0,0.15);
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
    font-size: 13px;
    color: #2c3e50;
    overflow-y: auto;
    transform: translateX(115%%);
    transition: transform 0.3s cubic-bezier(0.4, 0, 0.2, 1);
}
#map-sidebar.open { transform: translateX(0); }

#map-sidebar::-webkit-scrollbar { width: 6px; }
#map-sidebar::-webkit-scrollbar-thumb { background: #ccc; border-radius: 3px; }

.sidebar-header {
    padding: 16px 18px 12px;
    border-bottom: 1px solid #eef0f3;
    background: linear-gradient(135deg, #667eea 0%%, #764ba2 100%%);
    border-radius: 12px 12px 0 0;
    color: #fff;
}
.sidebar-header h2 { margin: 0; font-size: 16px; font-weight: 600; letter-spacing: 0.3px; }
.sidebar-header .stat-row {
    margin-top: 8px;
    display: flex;
    gap: 12px;
    font-size: 11px;
    opacity: 0.95;
}
.sidebar-header .stat-row b { font-size: 14px; display: block; }

.sidebar-section {
    padding: 14px 18px;
    border-bottom: 1px solid #eef0f3;
}
.sidebar-section h4 {
    margin: 0 0 10px 0;
    font-size: 11px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.8px;
    color: #6b7280;
}

.seg-control {
    display: flex;
    background: #f3f4f6;
    border-radius: 8px;
    padding: 3px;
    gap: 3px;
}
.seg-control label {
    flex: 1;
    text-align: center;
    padding: 7px 8px;
    border-radius: 6px;
    cursor: pointer;
    font-size: 12px;
    font-weight: 500;
    transition: all 0.15s ease;
}
.seg-control input { display: none; }
.seg-control input:checked + span {
    background: #fff;
    box-shadow: 0 1px 3px rgba(0,0,0,0.12);
    color: #2c3e50;
    font-weight: 600;
}
.seg-control span {
    display: block;
    padding: 7px 8px;
    border-radius: 6px;
    color: #6b7280;
    transition: all 0.15s ease;
}
.seg-control label:hover span { color: #2c3e50; }

.swatch-row {
    display: flex;
    gap: 8px;
    align-items: center;
    margin: 6px 0;
}
.swatch-row label { flex: 1; font-size: 12px; color: #4b5563; }
.swatch-row input[type="color"] {
    width: 38px;
    height: 28px;
    border: 1px solid #d1d5db;
    border-radius: 6px;
    cursor: pointer;
    padding: 2px;
    background: #fff;
}

input[type="range"] { width: 100%%; accent-color: #667eea; }
.slider-row { display: flex; align-items: center; gap: 10px; }
.slider-value {
    min-width: 32px;
    font-size: 12px;
    font-weight: 600;
    color: #667eea;
    text-align: right;
}

.btn {
    display: block;
    width: 100%%;
    padding: 9px 12px;
    margin: 6px 0;
    background: #667eea;
    color: #fff;
    border: none;
    border-radius: 8px;
    cursor: pointer;
    font-size: 12px;
    font-weight: 600;
    text-align: center;
    text-decoration: none;
    transition: all 0.15s ease;
}
.btn:hover { background: #5568d3; transform: translateY(-1px); }
.btn.secondary { background: #fff; color: #667eea; border: 1.5px solid #667eea; }
.btn.secondary:hover { background: #f0f3ff; }

.spot-filter { display: none; margin-top: 10px; }
.spot-filter.show { display: block; }

#map-legend {
    position: absolute;
    top: 16px;
    left: 60px;
    z-index: 1000;
    background: #fff;
    padding: 10px 14px;
    border-radius: 8px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.15);
    font-size: 12px;
    font-family: -apple-system, BlinkMacSystemFont, sans-serif;
    min-width: 110px;
    display: none;
}
#map-legend h4 {
    margin: 0 0 8px 0; font-size: 11px; font-weight: 700;
    text-transform: uppercase; letter-spacing: 0.6px; color: #6b7280;
    text-align: center;
}
.legend-item {
    display: flex; align-items: center; margin-bottom: 5px;
    font-size: 12px; color: #2c3e50;
}
.legend-icon {
    width: 14px; height: 14px; border-radius: 50%%;
    margin-right: 9px; border: 1px solid rgba(0,0,0,0.15);
}

@media print {
    #map-sidebar, #sidebar-toggle-btn { display: none !important; }
    #map-legend { display: block !important; position: absolute; top: 10px; left: 10px; }
    .leaflet-control-zoom { display: none !important; }
}
</style>

<div id="sidebar-toggle-btn" title="Map options">
  <div><span></span><span></span><span></span></div>
</div>

<div id="map-legend">
    <h4>Legend</h4>
    <div class="legend-item" id="legend-case-item">
        <span class="legend-icon" id="legend-icon-case" style="background-color:%s;"></span>
        <span>Case</span>
    </div>
    <div class="legend-item" id="legend-control-item" style="display:none;">
        <span class="legend-icon" id="legend-icon-control" style="background-color:%s;"></span>
        <span>Control</span>
    </div>
</div>

<div id="map-sidebar">
  <div class="sidebar-header">
    <h2>SpotMap Controls</h2>
    <div class="stat-row">
      <div><b>%d</b>Cases</div>
      <div><b>%d</b>Controls</div>
      <div><b>%s</b>Zoom</div>
    </div>
  </div>

  <div class="sidebar-section">
    <h4>Display Mode</h4>
    <div class="seg-control">
      <label>
        <input type="radio" name="markerMode" value="dots" checked>
        <span>Dot Density</span>
      </label>
      <label>
        <input type="radio" name="markerMode" value="pins">
        <span>Spot Pins</span>
      </label>
    </div>
    <div class="spot-filter" id="spotFilterBox">
      <h4 style="margin-top:12px;">Show</h4>
      <div class="seg-control">
        <label>
          <input type="radio" name="spotFilterMode" value="cases" checked>
          <span>Cases Only</span>
        </label>
        <label>
          <input type="radio" name="spotFilterMode" value="both">
          <span>Cases &amp; Controls</span>
        </label>
      </div>
    </div>
  </div>

  <div class="sidebar-section" id="clusterColorSection">
    <h4>Cluster Colour</h4>
    <div class="swatch-row">
      <label>Pick a colour</label>
      <input type="color" id="clusterCustomColor" value="%s">
    </div>
  </div>

  <div class="sidebar-section" id="pinColorSection" style="display:none;">
    <h4>Pin Colours</h4>
    <div class="swatch-row">
      <label>Cases</label>
      <input type="color" id="caseColorPicker" value="%s">
    </div>
    <div class="swatch-row">
      <label>Controls</label>
      <input type="color" id="controlColorPicker" value="%s">
    </div>
    <h4 style="margin-top:14px;">Pin Size</h4>
    <div class="slider-row">
      <input type="range" id="pinSizeSlider" min="0.5" max="2.0" step="0.1" value="1.0">
      <span class="slider-value" id="pinSizeValue">1.0x</span>
    </div>
  </div>

  <div class="sidebar-section">
    <h4>Export Map</h4>
    <a class="btn secondary" id="downloadPrintLink">Print / Save PDF</a>
  </div>
</div>
',
    case_color, control_color,
    n_cases, n_controls, mode,
    cluster_color,
    case_color, control_color
  ))
}


#' Build the sidebar JavaScript for layer toggling
#' @keywords internal
build_sidebar_js <- function(cluster_color, case_color, control_color) {
  sprintf('
function(el, x) {
  var mapObj = this;

  // Move legend inside map container
  var legendDiv = document.getElementById("map-legend");
  if (legendDiv) mapObj.getContainer().appendChild(legendDiv);

  var sidebar = document.getElementById("map-sidebar");
  var toggleBtn = document.getElementById("sidebar-toggle-btn");
  var sidebarOpen = true;
  sidebar.classList.add("open");

  toggleBtn.addEventListener("click", function() {
    sidebarOpen = !sidebarOpen;
    sidebar.classList.toggle("open", sidebarOpen);
  });

  window.caseColor    = "%s";
  window.controlColor = "%s";
  window.clusterBaseColor = "%s";
  window.pinScale = 1.0;

  // Identify layers by group name
  var dotsLayer = null;
  var pinsCasesLayer = null;
  var pinsControlsLayer = null;

  mapObj.eachLayer(function(layer) {
    if (layer.options && layer.options.group === "Dot Density Layer") dotsLayer = layer;
    if (layer.options && layer.options.group === "Spot Map - Cases") pinsCasesLayer = layer;
    if (layer.options && layer.options.group === "Spot Map - Controls") pinsControlsLayer = layer;
  });

  function updateLegend() {
    var legendBox = document.getElementById("map-legend");
    var isPins = document.querySelector("input[name=\\"markerMode\\"]:checked").value === "pins";
    legendBox.style.display = isPins ? "block" : "none";
    if (!isPins) return;
    document.getElementById("legend-icon-case").style.backgroundColor = window.caseColor;
    document.getElementById("legend-icon-control").style.backgroundColor = window.controlColor;
    var isBoth = document.querySelector("input[name=\\"spotFilterMode\\"]:checked").value === "both";
    document.getElementById("legend-control-item").style.display = isBoth ? "flex" : "none";
  }

  function makePinIcon(colorHex) {
    var scale = window.pinScale || 1.0;
    var baseW = 18, baseH = 24;
    var html =
      "<div style=\\"position:relative;width:"+baseW+"px;height:"+baseH+"px;transform:scale("+scale+");transform-origin:50%% 100%%;\\">" +
        "<div style=\\"position:absolute;left:3px;top:6px;width:12px;height:12px;border-radius:50%% 50%% 50%% 0;background:"+colorHex+";transform:rotate(-45deg);box-shadow:0 0 2px rgba(0,0,0,0.5);\\"></div>" +
        "<div style=\\"position:absolute;left:6.5px;top:9.5px;width:5px;height:5px;border-radius:50%%;background:white;opacity:0.9;\\"></div>" +
      "</div>";
    return L.divIcon({ html: html, className: "", iconSize: [baseW, baseH], iconAnchor: [baseW/2, baseH] });
  }

  function redrawPins() {
    if (pinsCasesLayer) {
      mapObj.eachLayer(function(layer) {
        if (layer.options && layer.options.group === "Spot Map - Cases") {
          layer.eachLayer(function(marker) {
            if (marker.setIcon) marker.setIcon(makePinIcon(window.caseColor));
          });
        }
      });
    }
    if (pinsControlsLayer) {
      mapObj.eachLayer(function(layer) {
        if (layer.options && layer.options.group === "Spot Map - Controls") {
          layer.eachLayer(function(marker) {
            if (marker.setIcon) marker.setIcon(makePinIcon(window.controlColor));
          });
        }
      });
    }
    updateLegend();
  }

  function applyLayerLogic() {
    var mode = document.querySelector("input[name=\\"markerMode\\"]:checked").value;
    var filter = document.querySelector("input[name=\\"spotFilterMode\\"]:checked").value;

    document.getElementById("spotFilterBox").classList.toggle("show", mode === "pins");
    document.getElementById("clusterColorSection").style.display = mode === "dots" ? "block" : "none";
    document.getElementById("pinColorSection").style.display = mode === "pins" ? "block" : "none";

    var groups = mapObj.layerControl ? mapObj.layerControl._layers : null;

    if (mode === "dots") {
      mapObj.eachLayer(function(l) {
        if (l.options && l.options.group === "Dot Density Layer" && !mapObj.hasLayer(l)) mapObj.addLayer(l);
        if (l.options && l.options.group === "Spot Map - Cases" && mapObj.hasLayer(l)) mapObj.removeLayer(l);
        if (l.options && l.options.group === "Spot Map - Controls" && mapObj.hasLayer(l)) mapObj.removeLayer(l);
      });
    } else {
      mapObj.eachLayer(function(l) {
        if (l.options && l.options.group === "Dot Density Layer" && mapObj.hasLayer(l)) mapObj.removeLayer(l);
      });
      mapObj.eachLayer(function(l) {
        if (l.options && l.options.group === "Spot Map - Cases" && !mapObj.hasLayer(l)) mapObj.addLayer(l);
      });
      if (filter === "both") {
        mapObj.eachLayer(function(l) {
          if (l.options && l.options.group === "Spot Map - Controls" && !mapObj.hasLayer(l)) mapObj.addLayer(l);
        });
      } else {
        mapObj.eachLayer(function(l) {
          if (l.options && l.options.group === "Spot Map - Controls" && mapObj.hasLayer(l)) mapObj.removeLayer(l);
        });
      }
      redrawPins();
    }
    updateLegend();
  }

  document.querySelectorAll("input[type=radio]").forEach(function(r) {
    r.addEventListener("change", applyLayerLogic);
  });

  var custClust = document.getElementById("clusterCustomColor");
  if (custClust) {
    custClust.addEventListener("input", function() {
      window.clusterBaseColor = custClust.value;
    });
  }

  document.getElementById("caseColorPicker").addEventListener("input", function(e) {
    window.caseColor = e.target.value;
    redrawPins();
  });
  document.getElementById("controlColorPicker").addEventListener("input", function(e) {
    window.controlColor = e.target.value;
    redrawPins();
  });

  var sizeSlider = document.getElementById("pinSizeSlider");
  var sizeValue = document.getElementById("pinSizeValue");
  sizeSlider.addEventListener("input", function(e) {
    window.pinScale = parseFloat(e.target.value);
    sizeValue.textContent = window.pinScale.toFixed(1) + "x";
    redrawPins();
  });

  document.getElementById("downloadPrintLink").addEventListener("click", function() { window.print(); });

  applyLayerLogic();
  redrawPins();
}
', case_color, control_color, cluster_color)
}
