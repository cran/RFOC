\name{TOSPHERE}
\alias{TOSPHERE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert to Spherical Coordinates}
\description{
  Get Azimuth and Dip from Cartesian vector on a sphere.
}
\usage{
TOSPHERE(x, y, z)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{x-coordinate}
  \item{y}{y-coordinate}
  \item{z}{z-coordinate}
}

\value{

  \item{az}{azimuth angle, degrees}
  \item{dip}{dip, degrees}

}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{TOSPHERE.DIP, tosphereL,  to.spherical}
\examples{
TOSPHERE(3, 4, 5)


}
\keyword{misc}
