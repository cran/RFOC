\name{fancyarrows}
\alias{fancyarrows}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Make fancy arrows}
\description{
  Create and plot fancy arrows. Aspect ratio must be set to 1-1
  for these arrows to plot correctly.
}
\usage{
fancyarrows(x1, y1, x2, y2, thick = 0.08,
     headlength = 0.4, headthick = 0.2, col = grey(0.5),
     border = "black")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{x  tail coordinate}
  \item{y1}{y  tail coordinate}
  \item{x2}{x  head coordinate }
  \item{y2}{y  head coordinate }
  \item{thick}{thickness of arrow }
  \item{headlength}{length of head }
  \item{headthick}{thickness of head}
  \item{col}{fill color }
  \item{border}{color of border}
}
\note{
fancyarrows only work if te aspect ratio is set to 1. See example below.
  }
\value{
 Graphical side effects.
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{TEACHFOC}
\examples{
   thick = 0.01; headlength = 0.2; headthick = 0.1

x = runif(10, -1, 1)
y = runif(10, -1, 1)

############   MUST set asp=1 here
plot(x,y, asp=1)

fancyarrows(rep(0, 10) , rep(0, 10) ,x, y,
thick =thick , headlength =  headlength,
headthick =headthick)

}

\keyword{aplot}
