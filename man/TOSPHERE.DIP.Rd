\name{TOSPHERE.DIP}
\alias{TOSPHERE.DIP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{convert to spherical coordinates}
\description{
convert to spherical coordinates
}
\usage{
TOSPHERE.DIP(x, y, z)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{x-coordinate }
  \item{y}{y-coordinate  }
  \item{z}{z-coordinate }
}
\details{
 takes three components and returns azimuth and dip
}
\value{
 List
  \item{az}{azimuth, degrees}
  \item{dip}{Dip, degrees}
   \item{x}{x-coordinate }
  \item{y}{y-coordinate  }
  \item{z}{z-coordinate }
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{to.spherical}
\examples{
TOSPHERE.DIP(3, 4, 5)

}

\keyword{misc}
