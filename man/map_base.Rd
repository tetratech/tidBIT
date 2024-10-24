% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_mapping.R
\name{map_base}
\alias{map_base}
\title{Map Base Layer}
\usage{
map_base()
}
\value{
A leaflet map object with CartoDB's Positron base tiles and a scale
  bar. The map is ready to be further customized by adding additional layers
  or controls.
}
\description{
Initializes a Leaflet map with CartoDB Positron tiles (no
  labels) and a scale bar for basic mapping needs.
}
\details{
Create a Basic Leaflet Map

Initializes a leaflet map object with a base layer using CartoDB's Positron
(no labels) tiles and adds a scale bar. This function serves as the starting
point for creating maps, providing a clean, minimalistic base layer onto
which other geographic elements can be added, such as markers, polygons, or
circles. The function utilizes the `leaflet` package to create the map, and
`CartoDB.PositronNoLabels` tiles are chosen for their clear, uncluttered
appearance.
}
\examples{
# Create a basic leaflet map with a base layer and a scale bar
map <- map_base()
# Display the map in an R environment capable of rendering HTML widgets (e.g.
# , RStudio Viewer, Shiny app, or R Markdown document)
map

}
\seealso{
\code{\link[leaflet]{addTiles}}, for adding other types of tile layers.
\code{\link[leaflet]{addScaleBar}}, for details on the scale bar added.
}
