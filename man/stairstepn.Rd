\name{stairstepn}
\alias{stairstepn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
generate steps
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
stairstepn(data, direction = "hv", yvars = "y")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{direction}{
%%     ~~Describe \code{direction} here~~
}
  \item{yvars}{
%%     ~~Describe \code{yvars} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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
function (data, direction = "hv", yvars = "y") 
{
    direction <- match.arg(direction, c("hv", "vh"))
    data <- as.data.frame(data)[order(data$x), ]
    n <- nrow(data)
    if (direction == "vh") {
        xs <- rep(1:n, each = 2)[-2 * n]
        ys <- c(1, rep(2:n, each = 2))
    }
    else {
        ys <- rep(1:n, each = 2)[-2 * n]
        xs <- c(1, rep(2:n, each = 2))
    }
    data.frame(x = data$x[xs], data[ys, yvars, drop = FALSE], 
        data[xs, setdiff(names(data), c("x", yvars)), drop = FALSE])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
