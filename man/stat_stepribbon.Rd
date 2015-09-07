\name{stat_stepribbon}
\alias{stat_stepribbon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Stair step shaped, geom_ribbon
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
stat_stepribbon(mapping = NULL, data = NULL, geom = "ribbon", position = "identity")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mapping}{
%%     ~~Describe \code{mapping} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{geom}{
%%     ~~Describe \code{geom} here~~
}
  \item{position}{
%%     ~~Describe \code{position} here~~
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
function (mapping = NULL, data = NULL, geom = "ribbon", position = "identity") 
{
    StatStepribbon$new(mapping = mapping, data = data, geom = geom, 
        position = position)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
