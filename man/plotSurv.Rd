\name{plotSurv}
\alias{plotSurv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots kaplan meier curves
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotSurv(survobj, CI = TRUE, legend.text = NULL, lsize = NULL, csize = NULL, title = "Survival", xlab = "Time", ylab = "Probability", table.text.size = 5, element.text.size = 17, returnPlots = FALSE, pval = TRUE, rho = 0, makeTable = TRUE, ggargsCurve = NULL, ggargsTable = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{survobj}{
A survfit object from survival package
}
  \item{CI}{
Logical: Whether to plot confidence interval
}
  \item{legend.text}{
Manually override legend text
}
  \item{lsize}{
Line size?
}
  \item{csize}{
dot size?
}
  \item{title}{
Title of plot
}
  \item{xlab}{
x-axis title
}
  \item{ylab}{
y-axis title
}
  \item{table.text.size}{
Text size for the n. at risk table
}
  \item{element.text.size}{
General text size
}
  \item{returnPlots}{
Logical: Whether to return a plot or a list of ggplot objects.
}
  \item{pval}{
Logical. Include a survival::survdiff, p value?
}
  \item{rho}{
rho parameter for p-value
}
  \item{makeTable}{
Logical: plot a tbale of n-at risk?
}
  \item{ggargsCurve}{
additional arguments to be passed to ggplot when plotting the curve
}
  \item{ggargsTable}{
additional arguments to be passed to ggplot when plotting the table
}
  \item{\dots}{
Adittional arguments
}
}
\details{
Uses ggplot2, and survival package
}
\value{
If returnPlot is TRUE returns a plot, if FALSE return a list of ggplot2 objects
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Federico Lasa
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (survobj, CI = TRUE, legend.text = NULL, lsize = NULL, 
    csize = NULL, title = "Survival", xlab = "Time", ylab = "Probability", 
    table.text.size = 5, element.text.size = 17, returnPlots = FALSE, 
    pval = TRUE, rho = 0, makeTable = TRUE, ggargsCurve = NULL, 
    ggargsTable = NULL, ...) 
{
    require(ggplot2)
    require(survival)
    require(gridExtra)
    p <- NULL
    num.cat = ifelse(is.null(survobj$strata), 1, length(survobj$strata))
    n <- survobj$strata
    categorias <- "Survival"
    if (num.cat > 1) {
        categorias <- sapply(names(survobj$strata), function(x) substr(x, 
            regexpr("=[^=]*$", x) + 1, 10000L), USE.NAMES = FALSE)
        if (!is.null(legend.text)) 
            categorias <- legend.text
        categorias <- factor(categorias, levels = categorias)
        start.df <- data.frame(time = rep(0, num.cat), n.risk = survobj$n, 
            Survival = rep(1, num.cat), cens = rep(FALSE, num.cat), 
            upper = rep(1, num.cat), lower = rep(1, num.cat), 
            grupo = categorias)
        categorias.rep <- rep(categorias, n)
    }
    else {
        categorias.rep <- rep("Survival", length(survobj$time))
        start.df <- data.frame(time = 0, n.risk = survobj$n, 
            Survival = 1, cens = FALSE, upper = 1, lower = 1, 
            grupo = "Survival")
    }
    df <- data.frame(time = survobj$time, n.risk = survobj$n.risk, 
        Survival = survobj$surv, cens = survobj$n.censor != 0, 
        upper = survobj$upper, lower = survobj$lower, grupo = categorias.rep)
    df <- rbind(start.df, df)
    cens <- which(df$cens)
    xrange = range(df$time)
    q <- ggplot(data = df, aes(x = time, y = Survival, ymin = lower, 
        ymax = upper, color = grupo)) + geom_step(size = 1) + 
        scale_shape_discrete(guide = FALSE) + scale_fill_discrete(guide = FALSE) + 
        theme(legend.title = element_blank()) + ylab(ylab) + 
        ggtitle(title)
    if (length(cens) > 0) 
        q <- q + geom_point(data = df[cens, ], aes(x = time, 
            y = Survival, color = grupo), shape = 3, size = 3, 
            alpha = 0.8)
    if (CI) 
        q <- q + geom_ribbon(alpha = 0.1, colour = NA, aes(fill = grupo), 
            stat = "stepribbon")
    if (pval & num.cat > 1) {
        sd1 <- survival::survdiff(eval(survobj$call$formula), 
            data = eval(survobj$call$data), rho = rho)
        p1 <- stats::pchisq(sd1$chisq, length(sd1$n) - 1, lower.tail = FALSE)
        p1txt <- ifelse(p1 < 1e-04, "p < 0.0001", paste(" p =", 
            signif(p1, 3)))
        q <- q + annotate("text", x = max(df$time) * 0.1, y = min(df$Survival), 
            label = p1txt)
    }
    if (!is.null(legend.text)) {
        q <- q + scale_colour_discrete(labels = legend.text) + 
            scale_linetype_discrete(labels = legend.text)
    }
    q <- q + theme(axis.title.x = element_blank(), text = element_text(size = element.text.size))
    if (!is.null(ggargsCurve)) 
        eval(parse(text = paste("q <- q +", ggargsCurve)))
    times <- ggplot_build(q)$panel$ranges[[1]]$x.minor_source
    sum.obj <- summary(survobj, times = times, extend = TRUE)
    table.df <- data.frame(grupo = rep(categorias, each = length(times)), 
        time = sum.obj$time, n.risk = sum.obj$n.risk, n.event = sum.obj$n.event)
    table.df$shift <- (table.df$time[2] - table.df$time[1])/2
    p <- ggplot(table.df, aes(x = time, y = grupo, label = format(n.risk, 
        nsmall = 0)))
    p <- p + geom_text(size = table.text.size) + geom_text(aes(x = time - 
        shift, y = grupo, label = format(paste0("(", n.event, 
        ")"), nsmall = 0)), data = table.df, color = "red", size = table.text.size) + 
        scale_y_discrete(breaks = (as.character(levels(table.df$grupo))), 
            labels = (levels(table.df$grupo))) + scale_x_continuous(limits = xrange, 
        breaks = times) + theme(text = element_text(size = 17), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        panel.grid.minor.x = element_blank(), panel.border = element_blank(), 
        axis.title.y = element_blank()) + guides(colour = FALSE) + 
        xlab(xlab)
    if (!is.null(ggargsTable)) 
        eval(parse(text = paste("p <- p +", ggargsTable)))
    if (num.cat == 1) {
        p <- p + theme(axis.text.y = element_blank(), axis.title.y = element_blank(), 
            axis.ticks.y = element_blank(), text = element_text(size = element.text.size))
    }
    if (returnPlots) 
        return(value = list(curve = q, table = p))
    plotAlign(q, p)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
