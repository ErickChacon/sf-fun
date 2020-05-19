#' @title Create an extended bbox
#'
#' @description
#' \code{st_buffer_box} extent the size of a \code{bbox} object.
#'
#' @details
#' details.
#'
#' @param box A \code{bbox} object or a named vector that includes \code{xmin},
#' \code{xmax}, \code{ymin} and \code{ymax}.
#' @param extent The extent to be added to \code{bbox}.
#'
#' @return An extended \code{bbox} object.
#'
#' @author Erick A. Chacón-Montalván
#'
#' @examples
#'
#' buffer_bbox(box, extent = 10)
#'
#' @export
st_buffer.bbox <- function(box, dist) {
    dist <- rep_len(dist, 2)
    box["xmin"] <- box["xmin"] - dist[1]
    box["xmax"] <- box["xmax"] + dist[1]
    box["ymin"] <- box["ymin"] - dist[2]
    box["ymax"] <- box["ymax"] + dist[2]
    return(box)
}

#' @title Obtain centroid of bounding box
#'
#' @description
#' \code{st_centroid} provides the centroid of a \code{bbox} object.
#'
#' @details
#' Uses an auxiliary coordinate system in meter.
#'
#' @param box .
#'
#' @return A \code{sfc} object with the centroid.
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @examples
#'
#' st_centroid(box)
#'
#' @export
st_centroid.bbox <- function(box, crs_m = 3857) {
    crs <- sf::st_crs(box)
    box_m <- sf::st_transform(sf::st_as_sfc(box), crs = crs_m)
    sf::st_transform(sf::st_centroid(box_m), crs = crs)
}


# this is an example
inside_box <- function(data, box = c(xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL),
                        aux_condition = TRUE) {
  index <- with(data,
                which(longitud > box["xmin"] & longitud < box["xmax"] &
                      latitud > box["ymin"] & latitud < box["ymax"] &
                      aux_condition))
  return(index)
}

seq_window <- function(var = 1:10, by = 3, buffer = 1) {
 seq <- unique(c(seq(min(var, na.rm = T) - buffer, max(var, na.rm = T) + buffer, by),
                 max(var, na.rm = T) + buffer))
  return(seq)
}

st_add_coordinates <- function (x, names = c("longitud", "latitud")) {
  x <- x %>%
    dplyr::bind_cols(., setNames(as.data.frame(sf::st_coordinates(.)), names))
  return(x)
}

st_as_points.bbox <- function(box) {
    st_as_sf(expand.grid(x = box[c("xmin", "xmax")], y = box[c("ymin", "ymax")]),
             coords = c("x", "y"), crs = st_crs(box))
}
