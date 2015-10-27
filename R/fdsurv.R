fdsurv <- function(muerte, fecha_inicial, fecha_evento, fecha_muerte, scale=1, ...) {
  require(survival)
  tiempos <- calcula_intervalo(muerte, fecha_inicial, fecha_evento, fecha_muerte, scale=scale)
  su <- Surv(tiempos, muerte, ...)
  su
}