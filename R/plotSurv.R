plotSurv <- function(survobj,                      
                     CI = TRUE,                     
                     legend.text = NULL, 
                     lsize = NULL, 
                     csize = NULL,
                     title = "Survival",
                     xlab = "Time",
                     ylab = "Probability",
                     table.text.size = 4,
                     element.text.size = 17,
                     returnPlots = FALSE,
                     pval = TRUE,
                     rho=0,
                     makeTable = TRUE,
                     ggargsCurve = NULL,
                     ggargsTable = NULL,
                     medians = TRUE, 
                     ...) {
  require(ggplot2)
  require(survival)
  require(gridExtra)
  p <- NULL
  num.cat = ifelse(is.null(survobj$strata), 1 , length(survobj$strata) )
  n <- survobj$strata
  
  categorias <- "Survival"
  ##SI HAY ESTRATOS
  if (num.cat > 1) {
    
    #Extraemos nombre de los grupos del objeto
    categorias <- sapply(names(survobj$strata), function(x) substr(x,regexpr("=[^=]*$", x)+1,10000L), USE.NAMES=FALSE)
    if (!is.null(legend.text)) categorias <- legend.text
    categorias <- factor(categorias, levels = categorias)
    
    ## Se agrega tiempo 0 a la cruva de sobrevida pues no necesariamente estan incluidos en el objeto
    start.df <- data.frame(time = rep(0, num.cat),
                           n.risk = survobj$n, 
                           Survival = rep(1,num.cat), 
                           cens = rep(FALSE,num.cat), 
                           upper = rep(1,num.cat), 
                           lower = rep(1, num.cat), grupo = categorias)
    
    ## A que grupo pertence cada fila de la curva
    categorias.rep <- rep(categorias, n)
    #num.cat=length(survobj$strata)>1
  ## SI NO hay distintos grupos
  } else {
    categorias.rep <- rep("Survival", length(survobj$time))
    start.df <- data.frame(time = 0,
               n.risk = survobj$n, 
               Survival = 1, 
               cens = FALSE, 
               upper = 1, 
               lower = 1, grupo="Survival")    
  }
  
  # data frame for plotting
  df <- data.frame(time = survobj$time, 
                   n.risk = survobj$n.risk, 
                   Survival = survobj$surv, 
                   cens = survobj$n.censor != 0, 
                   upper = survobj$upper, 
                   lower = survobj$lower, grupo = categorias.rep )
  
  df <- rbind(start.df,df)
  cens <- which(df$cens)
  
  #specify range for aligning table with curves
  xrange = range(df$time)
  
  #we build the curve plot
  q <- ggplot(data = df, aes(x = time, y = Survival, ymin = lower, ymax = upper, color = grupo)) 
  
  #Add extra stuff to curve?
  if (!is.null(ggargsCurve)) eval(parse(text=paste("q <- q +", ggargsCurve)))
  
  q <- q +
    geom_step(size = 1) +         
    scale_shape_discrete(guide = FALSE)+ 
    scale_fill_discrete(guide = FALSE) + 
    theme(legend.title = element_blank()) + 
    ylab(ylab) + ggtitle(title)
  
  if (length(cens)>0) q <- q + geom_point(data = df[cens,], 
                                          aes(x = time, y = Survival, color = grupo), 
                                          shape = 3, size = 3, alpha = 0.8)
  
  # add confidence intervals
  if (CI) q <- q + geom_ribbon(alpha = 0.1, colour = NA, aes(fill = grupo), stat = "stepribbon")
  if (medians) {
    if (num.cat > 1) {
    medianas <- summary(survobj)$table[,"median"]    
    } else {
      medianas <- summary(survobj)$table[5]
    }
    q <- q + geom_vline(xintercept = medianas, linetype = 2)
  }
  
  # add pvalue
  if (pval && num.cat > 1) {
    sd1 <- survival::survdiff(eval(survobj$call$formula),
                              data = eval(survobj$call$data), rho = rho)
    p1 <- stats::pchisq(sd1$chisq,
                        length(sd1$n) - 1,
                        lower.tail=FALSE)
    p1txt <- ifelse(p1 < 0.0001,
                    "p < 0.0001",
                    paste(" p =", signif(p1, 3))
    )
    q <- q + annotate("text",
                        x = max(df$time)*0.1,
                        y = min(df$Survival),
                        label = p1txt)
  }
  
  
  # If needed, custom legends
  if (!is.null(legend.text)) {
    q <- q + scale_colour_discrete(labels = legend.text) + 
      scale_linetype_discrete(labels = legend.text)
  }    
  
  q <- q + theme(axis.title.x = element_blank(), text = element_text(size = element.text.size))
   
  #Extract appropiate times from survfit
  times <- ggplot_build(q)$panel$ranges[[1]]$x.minor_source  
  sum.obj <- summary(survobj, times=times, extend=TRUE)
  
  #put data into a table
  table.df <- data.frame(grupo = rep(categorias, each=length(times)),
                         time = sum.obj$time,
                         n.risk = sum.obj$n.risk,
                         n.event = sum.obj$n.event )
  
  #Build the table plot
  
  table.df$shift <- (table.df$time[2] - table.df$time[1])/2
  # Reverse table group order to be consistent with legends.
  table.df$grupo <- factor(table.df$grupo, levels = rev(levels(table.df$grupo)))
  p <- ggplot(table.df, aes(x = time, y = grupo, label = format(n.risk, nsmall=0))) 
  p <- p + geom_text(size = table.text.size) + 
    geom_text(data = table.df, 
              aes(x = time-shift, y = grupo, label = format(paste0("(",n.event,")"), nsmall = 0)), 
              color = "red", size = table.text.size) +    
   scale_y_discrete(breaks = (as.character(levels(table.df$grupo))), labels = (levels(table.df$grupo))) +
   scale_x_continuous(limits = xrange, breaks = times) +    
   theme(
     text = element_text(size=17),
     #plot.background = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_blank(),
      #plot.title = element_text(size = rel(0.75)),
      #axis.title.x = element_blank(),
      ### change background to white
      ### panel.background = element_blank(),
      axis.title.y = element_blank() 
    ) +
    guides(colour = FALSE) + 
    xlab(xlab)
	  #Add extra stuff to curve?
  if (!is.null(ggargsTable)) eval(parse(text=paste("p <- p +", ggargsTable)))
  
  #If only 1 strata remove y-axiss text elements
  if (num.cat==1) {
    p <- p + theme(axis.text.y = element_blank(), axis.title.y = element_blank(), 
                   axis.ticks.y = element_blank(), text = element_text(size = element.text.size))
  }  
   
    if (returnPlots) return(value  =list(curve = q,table = p))
  
    plotAlign(q,p)  
  
}

## Function used to align plots, separate in case needed

plotAlign <- function(q,p) {
  gA <- ggplotGrob(q)
  gB <- ggplotGrob(p)
  maxWidth = grid::unit.pmax(gA$widths, gB$widths)
  gA$widths <- as.list(maxWidth)
  gB$widths <- as.list(maxWidth)
  grid.arrange(gA, gB, heights=unit(c(0.75,0.25), units="npc"), ncol=1)
  
}


# Step geombibon stat for the CIs. copied from somewhere. by #jdnewmil @ https://groups.google.com/d/msg/ggplot2/9cFWHaH1CPs/STwRwSn1v0kJ 
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

stat_stepribbon <- function( mapping = NULL, data  =NULL, geom = "ribbon", position = "identity" ) {
  StatStepribbon$new( mapping = mapping, data = data, geom = geom, position = position )
}
require(proto)
StatStepribbon <- proto(ggplot2:::Stat, {
  objname <- "stepribbon"
  desc <- "Stepwise area plot"
  desc_outputs <- list(
    x = "stepped independent variable",
    ymin = "stepped minimum dependent variable",
    ymax = "stepped maximum dependent variable"
  )
  required_aes <- c( "x", "ymin", "ymax" )
  
  default_geom <- function(.) GeomRibbon
  default_aes <- function(.) aes( x = ..x.., ymin = ..y.., ymax = Inf )
  
  calculate <- function( ., data, scales, direction = "hv", yvars = c( "ymin", "ymax" ), ...) {
    stairstepn( data = data, direction = direction, yvars = yvars )
  }
  
})