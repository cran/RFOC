\name{addsmallcirc}
\alias{addsmallcirc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Small Circle on Stereonet}
\description{
  Calculate and plot small circle on Stereo net
  at arbitrary azimuth, orientation and conical angle
}
\usage{
addsmallcirc(az, iang, alphadeg, BALL.radius = 1, N = 100, add = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{az}{Azimuth of axis}
  \item{iang}{angle of dip, degrees}
  \item{alphadeg}{width of cone in degrees}
  \item{BALL.radius}{size of sphere}
  \item{N}{NUmber of points to calculate}
  \item{add}{logical, TRUE=add to existing plot}
  \item{\dots}{graphical parameters}
}
\details{
  Given the azimuth and dip of a vector, plot the small circle around
  the pole with conical angle alphadeg
}
\value{
LIST:
  \item{x}{x-coordinates}
  \item{y}{y-coordinates}
 
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
  alphadeg is the radius of the conic projection
}
\seealso{net}
\examples{
net()
addsmallcirc(65, 13, 20, BALL.radius = 1, N = 100, add = TRUE)
addsmallcirc(165, 73, 5.6, BALL.radius = 1, N = 100, add = TRUE)



}

\keyword{aplot}
