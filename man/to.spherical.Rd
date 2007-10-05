\name{to.spherical}
\alias{to.spherical}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert Cartesian to Spherical}
\description{
 Convert cartesian coordinates to strike and dip
}
\usage{
to.spherical(x, y, z)
}

\arguments{
  \item{x}{x-coordinate}
  \item{y}{y-coordinate}
  \item{z}{z-coordinate}
}

\value{
  LIST
  \item{az}{angle, degrees}
  \item{dip}{angle, degrees}
   \item{x}{x-coordinate}
  \item{y}{y-coordinate}
  \item{z}{z-coordinate}
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{SDRfoc}
\examples{
to.spherical(3, 4, 5)
}
\keyword{misc}
