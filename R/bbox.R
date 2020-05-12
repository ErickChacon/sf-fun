st_add_coordinates <- function (x, names = c("longitud", "latitud")) {
  x <- x %>%
    dplyr::bind_cols(., setNames(as.data.frame(sf::st_coordinates(.)), names))
  return(x)
}

#' @title Create an extended bbox
#'
#' @description
#' \code{st_buffer_box} extent the size of a \code{bbox} object.
#'
#' @details
#' details.
#'
#' @param box A \code{bbox} object.
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
st_buffer.bbox <- function(box = c(xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL),
                           dist) {

    box["xmin"] <- box["xmin"] - dist
    box["xmax"] <- box["xmax"] + dist
    box["ymin"] <- box["ymin"] - dist
    box["ymax"] <- box["ymax"] + dist
    return(box)
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
