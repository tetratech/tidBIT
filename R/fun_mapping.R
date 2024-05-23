# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Jon Harcum, 2024-04-11
#' @title Map Base Layer
#'
#' @description Initializes a Leaflet map with CartoDB Positron tiles (no
#'   labels) and a scale bar for basic mapping needs.
#'
#' @details Create a Basic Leaflet Map
#'
#' Initializes a leaflet map object with a base layer using CartoDB's Positron
#' (no labels) tiles and adds a scale bar. This function serves as the starting
#' point for creating maps, providing a clean, minimalistic base layer onto
#' which other geographic elements can be added, such as markers, polygons, or
#' circles. The function utilizes the `leaflet` package to create the map, and
#' `CartoDB.PositronNoLabels` tiles are chosen for their clear, uncluttered
#' appearance.
#'
#' @return A leaflet map object with CartoDB's Positron base tiles and a scale
#'   bar. The map is ready to be further customized by adding additional layers
#'   or controls.
#'
#' @importFrom leaflet leaflet addProviderTiles addScaleBar providerTileOptions
#' @importFrom dplyr %>% filter select
#'
#' @examples
#' # Create a basic leaflet map with a base layer and a scale bar
#' map <- map_base()
#' # Display the map in an R environment capable of rendering HTML widgets (e.g.
#' # , RStudio Viewer, Shiny app, or R Markdown document)
#' map
#'
#' @export
#'
#' @seealso \code{\link[leaflet]{addTiles}}, for adding other types of tile layers.
#' \code{\link[leaflet]{addScaleBar}}, for details on the scale bar added.
map_base <- function() {

  leaflet() %>%
    addProviderTiles(
      provider = "CartoDB.PositronNoLabels",
      options = providerTileOptions(noWrap = TRUE)
    ) %>%
    addScaleBar()

} # end ~ function: map_base



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Jon Harcum, 2024-04-11
#' @title Add Colored Polygons to a Leaflet Map
#'
#' @description Adds colored polygons to a Leaflet map based on a given `sf`
#'   object and attribute for coloring.
#'
#' @details This function enhances a Leaflet map by adding colored polygons
#'   derived from an `sf` object. The color of each polygon can be dynamically
#'   set based on an attribute within the `sf` object. A default color will be
#'   applied if no specific coloring attribute is specified. Additionally, if a
#'   column name for coloring and a color palette are provided, the function
#'   uses these to color the polygons. Otherwise, a default viridis color
#'   palette is applied. This function is particularly useful for visualizing
#'   spatial data with varying attributes (e.g., regions of interest, zoning
#'   areas) directly on a map.
#'
#' @param P1 The Leaflet map object to which polygons will be added. If NULL, a
#'   new Leaflet map will be created.
#' @param sf_obj An `sf` object containing MULTIPOLYGON geometries.
#' @param obj_name The name of the attribute in `sf_obj` to use for popup labels.
#' @param col_name The name of the attribute in `sf_obj` used for coloring
#'   polygons. Optional.
#' @param col_palette A vector of colors for the different values of `col_name`.
#'   If not provided, a default palette will be used.
#' @param legend_title A character string to be used for the legend title.
#'   Defaults to "Polygon Legend".
#'
#' @return A Leaflet map object with the added polygons and potentially a
#'   legend, ready for further use or display.
#'
#' @importFrom dplyr %>% filter select
#' @importFrom leaflet addPolygons addLegend highlightOptions labelOptions
#' @importFrom scales viridis_pal
#'
#' @examples
#' \dontrun{
#' # Assuming `segments` is an sf object with MULTIPOLYGON geometries
#' map <- map_base()
#' map <- map_polygon(P1 = map, sf_obj = segments, obj_name = "region"
#'                  , col_name = "zone", col_palette = c("red", "green", "blue")
#'                  , legend_title = "Zones")
#' # Display the map
#' map
#' }
#'
#' @export
#'
#' @seealso \code{\link[leaflet]{addPolygons}}, for adding polygons to a Leaflet map.
#' \code{\link[scales]{viridis_pal}}, for details on the default color palette used.
#'
#'
map_polygon <- function(P1 = NULL
                        , sf_obj
                        , obj_name
                        , col_name = NULL
                        , col_palette = NULL
                        , legend_title = "Polygon Legend") {

  if (is.null(P1)) {
    P1 <- map_base()
  }

  if (!is.null(col_name) && !is.null(col_palette)) {
      colors <- col_palette[as.factor(sf_obj[[col_name]])]
  } else if (!is.null(col_name) && is.null(col_palette)) {
      unique_vals <- sort(unique(sf_obj[[col_name]]))
      col_palette <- scales::viridis_pal()(length(unique_vals))
      colors <- col_palette[as.factor(sf_obj[[col_name]])]
  } else {
    colors <- "blue"
  }

  P1 <- P1 %>% leaflet::addPolygons(
    data = sf_obj,
    fillColor = ~colors,
    fillOpacity = 0.25,
    color = "black",
    weight = 0.5,
    opacity = 1,
    label = ~sf_obj[[obj_name]],
    labelOptions = labelOptions(noHide = FALSE, direction = "auto"),
    highlightOptions = highlightOptions(
      weight = 1,
      color = "white",
      fillOpacity = 0.5
    )
  )

  # Add Legend for Polygons
  if (!is.null(col_name) ) {
    P1 <- P1 %>% leaflet::addLegend(position = "bottomright",
                                    title = legend_title,
                                    colors = col_palette,
                                    labels = levels(as.factor(sf_obj[[col_name]])),
                                    opacity = 1)
  }

  return(P1)

}   # end ~ function: map_polygon


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#' @title Add Colored Points to a Leaflet Map
#'
#' @description Adds colored points to a Leaflet map based on a given `sf`
#'   object and attribute for coloring.
#'
#' @details This function enriches a Leaflet map by overlaying colored points
#'   derived from an `sf` object containing POINT geometries. Points can be
#'   dynamically colored based on a specified attribute within the `sf` object,
#'   enabling the visualization of categorical or numerical data. The function
#'   supports logical, factor, and numeric data types for the coloring
#'   attribute. If a specific coloring attribute and color palette are provided,
#'   these are used to determine the colors of the points. Otherwise, a default
#'   color or a reversed viridis color palette is applied, ensuring a visually
#'   distinct and informative map. Sorting is applied for logical coloring
#'   attributes to ensure proper layering of points.
#'
#' @param P1 The Leaflet map object to which points will be added; if NULL, a
#'   new Leaflet map is created.
#' @param sf_obj An `sf` object containing POINT geometries.
#' @param obj_name The name of the attribute in `sf_obj` for popup labels.
#' @param col_name The name of the attribute in `sf_obj` used for coloring
#'   points; optional.
#' @param col_palette A vector of colors for the different values of `col_name`;
#'   if not provided, a default palette is used.
#' @param legend_title A character string for the legend title; defaults to
#'   "Point Legend".
#'
#' @return A Leaflet map object with the added colored points and optionally a
#'   legend, ready for further use or display.
#'
#' @importFrom leaflet addCircleMarkers labelOptions addLegend
#' @importFrom dplyr %>% filter select arrange
#' @importFrom rlang sym
#' @importFrom scales viridis_pal
#'
#' @examples
#' \dontrun{
#' # Assuming `stations` is an sf object with POINT geometries
#' map <- map_base()
#' map <- map_points(P1 = map, sf_obj = stations, obj_name = "name", col_name = "status",
#'                    col_palette = c("green", "red"), legend_title = "Station Status")
#' # Display the map
#' map
#' }
#'
#' @export
#'
#' @seealso \code{\link[leaflet]{addCircleMarkers}}, for adding points to a Leaflet map.
#' \code{\link[scales]{viridis_pal}}, for details on the default color palette used.
#'
#'
map_points <- function(P1 = NULL
                       , sf_obj
                       , obj_name
                       , col_name = NULL
                       , col_palette = NULL
                       , legend_title = "Point Legend") {
  if (is.null(P1)) {
    P1 <- map_base()
  }

  # sort sf_obj in col_name order is logical
  if (!is.null(col_name) && is.logical(sf_obj[[col_name]])) {
    sf_obj <- sf_obj %>%
      arrange(!!sym(col_name))
  }

  if (!is.null(col_name) && !is.null(col_palette)) {
      colors <- col_palette[as.factor(sf_obj[[col_name]])]
  } else if (!is.null(col_name) && is.null(col_palette)) {
      unique_vals <- sort(unique(sf_obj[[col_name]]))
      col_palette <- viridis_pal()(length(unique_vals)+1)
      col_palette <- rev(col_palette)[-1]
      colors <- col_palette[as.factor(sf_obj[[col_name]])]
  } else {
    colors <- "red"
  }

  P1 <- P1 %>% addCircleMarkers(
    data = sf_obj,
    color = ~colors,
    fillColor = ~colors,
    fillOpacity = 0.35,
    weight = 2,
    radius = 5,
    label = ~sf_obj[[obj_name]],
    labelOptions = labelOptions(noHide = FALSE, direction = 'auto'),
    popup = ~paste(obj_name, sf_obj[[obj_name]])
  )

  # Add Legend for Points
  if (!is.null(col_name) ) {
    P1 <- P1 %>% addLegend(position = "bottomright",
                                    title = legend_title,
                                    colors = col_palette,
                                    labels = levels(as.factor(sf_obj[[col_name]])),
                                    opacity = 1)
  }

  return(P1)

}  # end ~ function: map_points
