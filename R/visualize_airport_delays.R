#' Visualize mean arrival delay by destination airport
#'
#' Plots the mean arrival delay for each destination airport of flights
#' departing New York City in 2013, positioned by longitude and latitude.
#'
#' @return A ggplot object.
#' @export
#' @importFrom rlang .data
visualize_airport_delays <- function(){
  delays <- nycflights13::flights |>
    dplyr::group_by(dest = .data$dest) |>
    dplyr::summarise(mean_delay = mean(.data$arr_delay, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::inner_join(nycflights13::airports, by = c("dest" = "faa"))

  ggplot2::ggplot(delays, ggplot2::aes(x = .data$lon, y = .data$lat,
                                       colour = .data$mean_delay)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_colour_gradient2(low = "blue", mid = "grey80",
                                    high = "red", midpoint = 0) +
    ggplot2::labs(x = "Longitude", y = "Latitude",
                  colour = "Mean arrival\ndelay (min)",
                  title = "Mean arrival delay by destination airport") +
    ggplot2::theme_minimal()
}
