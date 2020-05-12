
#' @title Convert distance to latlon degrees
#'
#' @description
#' \code{dist2degrees} converts a distance (e.g. in meters) to degrees for a
#' particular lonlat position.
#'
#' @details
#' The conversion uses a auxiliary coordinate system \code{crs_m} which is 3857 by
#' default.
#'
#' @param dist Value representing the distance in meters. It can also be a
#' \code{units} object.
#'
#' @return A vector representing the equivalent of the distance in longitude and
#' latitude degrees respectively.
#'
#' @author Erick A. Chacón-Montalván
#'
#' @examples
#'
#' dist2degrees(1000, c(0, 0), crs = 4326, crs_m = 3857)
#' dist2degrees(set_units(10, "km"), c(0, 0), crs = 4326, crs_m = 3857)
#' dist2degrees(set_units(10, "km"), c(20, 20), crs = 4326, crs_m = 3857)
#'
#' @export
dist2degrees <- function(dist, point = c(-80, 0), crs = 4326, crs_m = 3857) {
    dist <- units::drop_units(units::set_units(dist, "m"))
    point <- as.numeric(st_point_trans(point, crs, crs_m))
    pp <- rbind(c(point[1] - dist / 2, point[2] - dist / 2),
                c(point[1] + dist / 2, point[2] + dist / 2))
    pp <- t(apply(pp, 1, st_point_trans, crs = crs_m, crs_trans = crs))
    c(pp[2, ] - pp[1, ])
    # pp
}

st_point_trans <- function(x, crs, crs_trans) {
    sf::st_coordinates(sf::st_transform(point_crs(x, crs), crs = crs_trans))
}
