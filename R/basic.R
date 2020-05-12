
#' @title Create point geometry with CRS
#'
#' @description
#' \code{point_crs} Creates a point geometry with associated CRS.
#'
#' @param x A vector of coordinates.
#' @param crs Associated coordinate reference sytem.
#'
#' @return A \code{sfc} object with associated \code{crs}.
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @examples
#'
#' point_crs(c(0, 0), crs = 4326)
#'
#' @export
point_crs <- function(x, crs) {
    sf::st_sfc(sf::st_point(x), crs = crs)
}

