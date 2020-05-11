st_add_coordinates <- function (x, names = c("longitud", "latitud")) {
  x <- x %>%
    dplyr::bind_cols(., setNames(as.data.frame(sf::st_coordinates(.)), names))
  return(x)
}

buffer_box <- function (box = c(xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL),
                        extent = 10) {
  box["xmin"] <- box["xmin"] - extent
  box["xmax"] <- box["xmax"] + extent
  box["ymin"] <- box["ymin"] - extent
  box["ymax"] <- box["ymax"] + extent
  return(box)
}

inside_box <- function (data, box = c(xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL),
                        aux_condition = TRUE) {
  index <- with(data,
                which(longitud > box["xmin"] & longitud < box["xmax"] &
                      latitud > box["ymin"] & latitud < box["ymax"] &
                      aux_condition))
  return(index)
}

seq_window <- function (var = 1:10, by = 3, buffer = 1) {
 seq <- unique(c(seq(min(var, na.rm = T) - buffer, max(var, na.rm = T) + buffer, by),
                 max(var, na.rm = T) + buffer))
  return(seq)
}
