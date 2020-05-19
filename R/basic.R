
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

#' @title Ensure union provide unique multipolygon
#'
#' @description
#' \code{st_union_unique} helps to keep only the main multipolygon after using
#' \code{st_union}.
#'
#' @details
#' It simple collects the first polygon for each element of the list.
#'
#' @param x A \code{sfc} object obtained after using \code{st_union}.
#'
#' @return A \code{sfc} object with unique multipolygon.
#'
#' @author Erick A. Chacon-Montalvan
#'
#' @examples
#'
#' 
#'
#' @export
st_union_unique <- function (x) {
    uni <- st_multipolygon(lapply(x[[1]], function(y) y[1]))
    uni <- st_sfc(uni, crs = st_crs(x))
}
