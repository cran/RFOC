\name{qpoint}
\alias{qpoint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Point on Stereonet}
\description{
  Plot a set of (azimuths, takeoff) angles on a stereonet.
}
\usage{
qpoint(az, iang, col = 2, pch = 5, lab = "", POS = 4, UP = FALSE, PLOT = FALSE, cex = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{az}{vector of azimuths, degrees}
  \item{iang}{vector of incident angles, degrees}
  \item{col}{color}
  \item{pch}{plotting character}
  \item{lab}{text labels}
  \item{POS}{position for labels}
  \item{UP}{ logical, TRUE=upper }
  \item{PLOT}{logical,  add to existing plot }
  \item{cex}{character expansion of labels }
}
\details{
  The iang argument represents the takeoff angle, and
  is measured from the nadir (z-axis pointing down).
}
\value{
  List
  \item{x}{coordinate on plot}
  \item{y}{coordinate on plot}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{FixDip, focpoint}
\examples{

d = runif(10, 0, 90)
a = runif(10, 0,360)
net()
qpoint(a, d)


}

\keyword{aplot}
