calcula_intervalo <- function(evento, t_inicio, t_evento, t_fin, scale=1) {
  time <- ifelse(evento, as.numeric(t_evento - t_inicio),
                 as.numeric(t_fin - t_inicio))
  time <- replace(time, is.infinite(time), NA)
  time / scale
}