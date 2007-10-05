\name{TOCART.DIP}
\alias{TOCART.DIP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert to Cartesian}
\description{
 Convert azimuth and dip to cartesian coordinates
}
\usage{
TOCART.DIP(az, dip)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{az}{azimuth, degrees}
  \item{dip}{dip, degrees }
}

\value{
  LIST
  \item{x}{x-coordinate}
  \item{y}{y-coordinate}
  \item{z}{z-coordinate}
  \item{az}{azimuth, degrees}
  \item{dip}{dip, degrees }
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{to.spherical}
\examples{
TOCART.DIP(134, 32)
}

\keyword{misc}
