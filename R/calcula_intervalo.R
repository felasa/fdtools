calcula_intervalo <- function(evento, t_inicio, t_evento, t_fin) {
  ifelse(evento, as.numeric(t_evento - t_inicio),
         as.numeric(t_fin - t_inicio))
}