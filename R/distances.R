
#' @title Convert distance to latlon degrees
#'
#' @description
#' \code{dist2degrees} converts a distance (e.g. in meters) to degrees for a
#' particular lonlat \code{position}.
#'
#' @details
#' The distance in degrees in computed with respective to a particular lanlat
#' \code{position}. The conversion uses a auxiliary coordinate system (\code{crs_m})
#' in meters which is 3857 by default.
#'
#' @param dist Value representing the distance in meters. It can also be a
#' \code{units} object.
#' @param position A vector with longitude and latitude coordinates respectively. It
#' can also be a sfc object.
#' @param crs Coordinate reference system for position.
#' @param crs_m Auxiliary coordinate reference system in units.
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
dist2degrees <- function(dist, position = point_crs(c(0, 0), crs = 4326), crs_m = 3857) {
    dist <- units::drop_units(units::set_units(dist, "m"))

    crs <- sf::st_crs(position)
    position <- sf::st_coordinates(sf::st_transform(position, crs_m))
    pp <- rbind(c(position[1, 1] - dist / 2, position[1, 2] - dist / 2),
                c(position[1, 1] + dist / 2, position[1, 2] + dist / 2))

    ff <- function(x) sf::st_coordinates(sf::st_transform(point_crs(x, crs_m), crs = crs))
    pp <- t(apply(pp, 1, ff))
    return(c(pp[2, ] - pp[1, ]))
}

dist2degrees <- function(dist, position = point_crs(c(0, 0), crs = 4326), crs_m = 3857) {
    dist <- units::drop_units(units::set_units(dist, "m"))
    crs <- sf::st_crs(position)
    position <- sf::st_coordinates(sf::st_transform(position, crs_m))
    pp <- rbind(c(position[1, 1] - dist / 2, position[1, 2] - dist / 2),
                c(position[1, 1] + dist / 2, position[1, 2] + dist / 2))

    ff <- function(x) sf::st_coordinates(sf::st_transform(point_crs(x, crs_m), crs = crs))
    pp <- t(apply(pp, 1, ff))
    return(c(pp[2, ] - pp[1, ]))
}
