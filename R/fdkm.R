StatStepribbon <- 
  ggproto("stepribbon", Stat, 
          compute_group = function(data, scales, direction = "hv",
                                   yvars = c( "ymin", "ymax" ), ...) {
            stairstepn( data = data, direction = direction, yvars = yvars )
          },
          required_aes = c( "x", "ymin", "ymax" ))

stat_stepribbon <- function( mapping = NULL, data = NULL, 
                             geom = "ribbon", position = "identity" ) {  
  ggplot2::layer(stat = "stepribbon", mapping = mapping, 
                 data = data, geom = geom, position = position)
}

stairstepn <- function( data, direction="hv", yvars="y" ) {
  direction <- match.arg( direction, c( "hv", "vh" ) )
  data <- as.data.frame( data )[ order( data$x ), ]
  n <- nrow( data )
  
  if ( direction == "vh" ) {
    xs <- rep( 1:n, each = 2 )[ -2 * n ]
    ys <- c( 1, rep( 2:n, each = 2 ) )
  } else {
    ys <- rep( 1:n, each = 2 )[ -2 * n ]
    xs <- c( 1, rep( 2:n, each = 2))
  }
  
  data.frame(
    x = data$x[ xs ]
    , data[ ys, yvars, drop=FALSE ]
    , data[ xs, setdiff( names( data ), c( "x", yvars ) ), drop=FALSE ]
  ) 
}

fdkm <- function(fit = survfit_obj, ...) {
  require(survival)
  require(ggplot2)
  if (class(fit) != "survfit") stop("Fit must be survfit object")
}