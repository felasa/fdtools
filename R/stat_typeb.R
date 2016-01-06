StatTypeb <- ggproto("StatTypeb", Stat,
                     compute_group = function(data, scales, epsilon = diff(range(data$x))/(length(data$x)*3)) {
                     n <- length(data$x)
                     delta_x <- diff(data$x)
                     delta_y <- diff(data$y)
                     angle <- atan(delta_y/delta_x)
                     x_plus <- epsilon * cos(angle)
                     y_plus <- epsilon * sin(angle)
                     
                     plus <- data.frame(x = data$x[-n] + x_plus, y = data$y[-n] + y_plus)
                     minus <- data.frame(xend = data$x[-1] - x_plus, yend = data$y[-1] - y_plus)                     
                     cbind(minus, plus)            
                     },                     
                     required_aes = c("x", "y")
)

stat_typeb <- function(mapping = NULL, data = NULL, geom = "segment",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatTypeb, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
