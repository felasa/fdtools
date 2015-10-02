fdsurv <- function(muerte, fecha_inicial, fecha_evento, fecha_muerte) {
  require(survival)
  tiempos <- calcula_intervalo(muerte, fecha_inicial, fecha_evento, fecha_muerte)
  su <- Surv(tiempos, muerte)
  su
}