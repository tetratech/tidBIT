 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#' @title Find adjoining polygons 
#' 
#' @description Identifies and returns the names of polygons that are adjacent
#'   to a set of primary polygons within a given `sf` object. This function is
#'   useful for spatial analyses that require understanding the relationships
#'   and connectivity between different spatial features, such as identifying
#'   bordering regions or zones.
#'
#' @details The function splits the input `sf` object into two groups based on
#'   the presence of the primary polygons specified. It then uses the
#'   `st_touches` function from the `sf` package to determine which polygons in
#'   the first group touch those in the second group. The result is a vector of
#'   names of the touching (adjoining) polygons. This can be particularly useful
#'   in ecological studies, urban planning, and geographical analyses where
#'   adjacency effects are of interest.
#'
#' @param sf_poly An `sf` object containing polygon geometries.
#' @param prim_poly A vector of names or identifiers for the primary polygons
#'   within `sf_poly` to which adjoining polygons are to be found.
#' @param col_name The name of the column in `sf_poly` that contains the names
#'   or identifiers of the polygons.
#'
#' @return Returns a vector containing the names or identifiers of polygons that
#'   adjoin the primary polygons specified in `prim_poly`.
#'
#' @examples
#' # Assuming `sf_poly` is an `sf` object with a column "id" for polygon identifiers:
#' primary_polygons <- c("poly1", "poly2")
#' adjoining_polygons <- find_adj_polygon(sf_poly = sf_poly, prim_poly = primary_polygons, col_name = "id")
#' print(adjoining_polygons)
#'
#' @export
#'
#' @importFrom sf st_touches st_drop_geometry
#' 
#' @seealso \code{\link[sf]{st_touches}}, for the function used to detect touching polygons.
#' 
#' 
find_adj_polygon <- function(sf_poly=NA, prim_poly=NA, col_name=NA) {
  
  # Split the sf object into two parts based on the selected polygons
  split_list <- split(sf_poly, sf_poly[[col_name]] %in% prim_poly)
  
  # Find polygons touching the selected polygons
  touching_polygons <- st_touches(split_list[["FALSE"]], split_list[["TRUE"]])
  
  # Create a matrix of logical values indicating touching polygons
  touching_matrix <- as.matrix(touching_polygons)
  touching_vector <- apply(touching_matrix, 1, any)
  
  # Extract the names of polygons touching the selected polygons
  touching_polygons_names <- st_drop_geometry(split_list[["FALSE"]])[touching_vector, col_name]
  
  # Print the resulting vector of touching polygon names
  print(touching_polygons_names)
  
  return(touching_polygons_names)
  
} # end ~ function: find_adj_polygon





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#' @title Find intersection of Two Touching Polygons
#'
#' @description Computes the geometric intersection of two touching polygons
#'   from a single `sf` layer. This function is particularly useful for
#'   identifying shared boundaries or overlap areas between adjacent polygons.
#'   The function first transforms the coordinate reference system (CRS) of the
#'   polygons to UTM (Universal Transverse Mercator), assuming the Mid-Atlantic
#'   region for the UTM zone. After computing the intersection in the UTM CRS to
#'   maintain accuracy, the resulting geometry is transformed back to WGS84
#'   latitude and longitude coordinates for compatibility with most mapping
#'   applications.
#'
#' @param sf_poly An `sf` object containing at least two polygons from which the
#'   intersection is to be found. It is assumed that the two polygons are
#'   touching or overlapping.
#' @param col_name The name of the attribute in `sf_poly` which uniquely
#'   identifies the polygons. This parameter is currently not utilized within
#'   the function but is reserved for future enhancements that might require
#'   attribute-based operations.
#'
#' @return An `sf` object representing the geometric intersection of the first
#'   two polygons in `sf_poly`, with coordinates in WGS84 CRS. If the polygons
#'   do not intersect, the function returns an empty `sf` object.
#'
#' @export
#' 
#' @importFrom sf st_transform st_intersection
#'
#' @examples
#' # Assuming `sf_poly` is an sf object containing at least two polygons
#' intersection <- find_intersection(sf_poly)
#' # View the resulting intersection geometry
#' plot(intersection)
#'
#' @note The function currently transforms the CRS to UTM zone 18N, which is
#'   suitable for the Mid-Atlantic region of the USA. Adjustments may be
#'   required for other regions.
#'
#' @seealso \code{\link[sf]{st_transform}}, for CRS transformations.
#' \code{\link[sf]{st_intersection}}, for computing geometric intersections.
#' 
#' 
find_intersection <- function(sf_poly=NA, col_name=NA) {
  
  # transform sf_poly to UTM (assumes Mid-Atlantic region)
  sf_poly_utm <- st_transform(sf_poly, crs = "+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs")
  
  # identify the common boundary
  common_boundary_utm <- st_intersection(sf_poly_utm[1, ], sf_poly_utm[2, ])
  
  # transform common_boundary_utm to lat/lng WGS84
  common_boundary <- st_transform(common_boundary_utm, crs = "+proj=longlat +datum=WGS84")

  return(common_boundary)
  
} # end ~ function: find_intersection






# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#' @title Create buffer around the a line
#' 
#' @description Creates a buffer zone of a specified distance around a line
#'   geometry contained within an `sf` object. This function is useful for
#'   spatial analysis tasks that require identifying areas within a certain
#'   range of a line feature, such as roads, rivers, or utility lines. The
#'   buffer distance is specified in kilometers and is converted internally to
#'   meters as `sf`'s `st_buffer` function requires distance in SI units.
#'
#' @param sf_obj An `sf` object containing LINESTRING geometries for which the
#'   buffer will be created.
#' @param col_name The name of the attribute in `sf_obj` which could be used to
#'   specify or differentiate line features for future enhancements. Currently,
#'   this parameter is included for compatibility and future development but is
#'   not utilized in the function's operation.
#' @param buffer_km The distance (in kilometers) for the buffer zone around each
#'   line feature. Specifying the distance allows for the creation of buffer
#'   zones that are tailored to specific analysis requirements.
#'
#' @return An `sf` object representing the buffer zones around the line features
#'   specified in `sf_obj`. The geometries of the buffer zones are in the same
#'   CRS as the input `sf` object and are of POLYGON or MULTIPOLYGON type,
#'   depending on the geometry of the buffer created.
#'
#' @export
#'
#' @importFrom sf st_buffer
#' 
#' @examples
#' # Assuming `lines` is an sf object containing LINESTRING geometries
#' buffer_zones <- create_buffer(sf_obj = lines, buffer_km = 1)
#' # Visualize the buffer zones
#' plot(buffer_zones)
#'
#' @note The buffer distance is internally converted to meters; the function
#'   assumes planar projection. For geographic projections, consider the
#'   implications of this conversion and the accuracy of the buffer in
#'   geographic terms.
#'
#' @seealso \code{\link[sf]{st_buffer}}, for creating buffer zones around geometries.
#' 
#' 


create_buffer <- function(sf_obj=NA, col_name=NA, buffer_km=NA) {
  
  # buffer the common boundary
  buffer_zone <- st_buffer(sf_obj, dist = buffer_km * 1000)  
  
  return(buffer_zone)
  
} # end ~ function: create_buffer




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#' @title Spatially join points to polygons
#' 
#' @description Spatially Join Points to Polygons with Out of Bounds Handling
#'
#' @description Performs a spatial join between point and polygon layers,
#'   extending functionality to handle "out of bounds" points by identifying the
#'   nearest polygon and calculating the distance to it. This function is
#'   particularly useful for datasets where some points may not fall within any
#'   of the specified polygons, and there's a need to associate these points
#'   with their closest polygon along with the proximity information.
#'
#' @param sf_obj_point An `sf` object containing POINT geometries that will be
#'   spatially joined to polygons.
#' @param sf_obj_poly An `sf` object containing POLYGON or MULTIPOLYGON
#'   geometries to which points will be joined.
#' @param col_name The name of the attribute in `sf_obj_poly` used to perform
#'   the join and to populate the "near" and "near_km" columns for out of bounds
#'   points.
#'
#' @return An `sf` object of the points layer with additional columns: 
#' - A column named after `col_name` containing the attribute values from the joined
#' polygons, or `NA` if the point is out of bounds.
#' - A column (`col_name_near`) indicating the nearest polygon's attribute value for
#' out of bounds points.
#' - A column (`col_name_near_km`) indicating the distance in kilometers to the
#' nearest polygon for out of bounds points.
#' 
#' The geometry of the returned `sf` object is the same as the input points layer.
#'
#' @export
#'
#' @examples
#' # Assuming `points` and `polygons` are sf objects with appropriate geometries
#' sf_joined <- st_join_oob(sf_obj_point = points, sf_obj_poly = polygons, col_name = "region")
#' # This will add the region attribute to points, and for those outside any region,
#' # it provides the nearest region and the distance to it.
#'
#' @note This function requires that both `sf_obj_point` and `sf_obj_poly` have 
#' been properly projected and share the same CRS. Distances are calculated based 
#' on the CRS's unit, then converted to kilometers.
#'
#' @importFrom sf st_join st_nearest_feature st_distance
#' @importFrom dplyr mutate select left_join row_number
#' @importFrom magrittr %>%
#'
#' @seealso \code{\link[sf]{st_join}}, for the basic spatial join operation.
#' \code{\link[sf]{st_nearest_feature}}, and \code{\link[sf]{st_distance}} for nearest 
#' feature identification and distance calculations.
#' 
#' 
st_join_oob <- function(sf_obj_point, sf_obj_poly, col_name) {
  
  col_name_near <- paste0(col_name, "_near")
  col_name_near_km <- paste0(col_name, "_near_km")
  row_id <- paste0("row_", paste0(sample(letters,10), collapse = ""))
  
  # Perform spatial join
  sf_obj_point <- st_join(sf_obj_point, sf_obj_poly[, col_name]) %>%
    mutate(!!row_id := row_number())
  
  # Filter sf_obj_point with NA in sf_obj_poly
  sf_obj_point_na <- sf_obj_point[is.na(sf_obj_point[[col_name]]), ] %>%
    select(!!row_id)
  
  sf_obj_point_na[[col_name_near]] <- NA
  sf_obj_point_na[[col_name_near_km]] <- NA
  
  # Find nearest polygon for each point not in a polygon
  nearest_indices <- st_nearest_feature(sf_obj_point_na, sf_obj_poly)
  nearest_polygons <- sf_obj_poly[nearest_indices, ]
  
  # Extract polygon column and distance for each point not in a polygon
  nearest_col <- nearest_polygons[[col_name]]
  nearest_col_km <- as.numeric(st_distance(sf_obj_point_na, nearest_polygons, by_element = TRUE))/1000
  
  # Assign the polygon and distance values to the corresponding columns in the point layer
  sf_obj_point_na[[col_name_near]] <- nearest_col
  sf_obj_point_na[[col_name_near_km]] <- nearest_col_km
  
  # Drop geometry information from point layer
  sf_obj_point_na <- st_drop_geometry(sf_obj_point_na)
  
  sf_obj_point <- left_join(
    sf_obj_point,
    sf_obj_point_na,
    row_id) %>%
    select(-!!row_id)
  
  return(sf_obj_point)  
  
} # end ~ function: st_join_woob


