\name{faultplane}
\alias{faultplane}

\title{fault plane projection on focal sphere}
\description{
  given azimuth and dip of fault mechanism, calculate and plot the
  fault plane.
}
\usage{
faultplane(az, dip, col = par("col"), PLOT = TRUE, UP = FALSE,lwd=2, lty=1, ...)
}

\arguments{
  \item{az}{degrees, strike of the plane (NOT down dip azimuth)}
  \item{dip}{degrees,  dip from horizontal}
  \item{col}{color for line}
  \item{PLOT}{option for adding to plot}
  \item{UP}{upper or lower hemisphere}
  \item{lwd}{Line Width}
  \item{lty}{Line Type}
  \item{\dots}{graphical parameters}
}
\details{
  Azimuth is the strike in degrees, not the down dip azimuth as
  described in other routines. 
}
\value{
  list of points along fault plane
  \item{x}{coordinates on focal sphere}
  \item{y}{coordinates on focal sphere}
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{Beachfoc}
\examples{


gcol='black'
border='black'
ndiv=36
phi = seq(0,2*pi, by=2*pi/ndiv);
  x = cos(phi);
  y = sin(phi);

plot(x,y, type='n', asp=1)
  lines(x,y, col=border)
  lines(c(-1,1), c(0,0), col=gcol)
  lines(c(0,0), c(-1,1), col=gcol)

faultplane(65, 34)


}

\keyword{aplot}
