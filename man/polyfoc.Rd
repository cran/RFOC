\name{polyfoc}
\alias{polyfoc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Polt the focal mechanism polygon }
\description{
  Calculate the projection of the focal mechanism polygon
}
\usage{
polyfoc(strike1, dip1, strike2, dip2, PLOT = FALSE, UP = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{strike1}{strike of plane 1, degrees}
  \item{dip1}{dip of plane 1, degrees}
  \item{strike2}{strike of plane 1, degrees }
  \item{dip2}{dip of plane 2, degrees }
  \item{PLOT}{logical, TRUE = add to plot}
  \item{UP}{upper/lower hemisphere }
}

\value{
  List of coordinates of polygon
  \item{Px}{x-coordinates of polygon}
  \item{Py}{y-coordinates of polygon}
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{faultplane}
\examples{
MEC = SDRfoc(13,59,125, PLOT=FALSE)

net()
ply = polyfoc(MEC$az1, MEC$dip1, MEC$az2, MEC$dip2, PLOT = TRUE, UP = TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
