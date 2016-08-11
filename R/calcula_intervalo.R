calcula_intervalo <- function(evento, t_inicio, t_evento, t_fin, scale=1, unit="days") {
  time <- ifelse(evento, as.numeric(difftime(t_evento, t_inicio, units=unit)),
                 as.numeric(difftime(t_fin, t_inicio, units=unit)))
  time / scale
}