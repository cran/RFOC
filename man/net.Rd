\name{net}
\alias{net}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{EqualArea Stereonet}
\description{
  Plot  Equal Area Stereo-Net.
  Lambert azimuthal Equal-Area (Schmidt) from Snyder p. 185-186
}
\usage{
net(add = FALSE, col = gray(0.7), border = "black", lwd = 1, LIM = c(-1, -1, +1, +1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{add}{logical, TRUE=add to existing plot }
  \item{col}{color of lines}
  \item{border}{color of outer rim of stereonet}
  \item{lwd}{linewidth of lines}
  \item{LIM}{bounding area for a new plot}
}

\value{
 Used for graphical side effects
}
\references{Snyder, John P., 1987,  Map Projections-a working manual, USGS-Professional Paper, 383p.  pages 185-186}
\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{pcirc}
\examples{
net(FALSE, col=rgb(.8,.7,.7) ,border='blue' )

}

\keyword{hplot}
