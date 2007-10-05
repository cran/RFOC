\name{tosphereL}
\alias{tosphereL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{convert to spherical coordinates}
\description{
 convert to spherical coordinates
}
\usage{
tosphereL(A)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{list (x,y,z)}
}
\details{
  takes list of three components and returns azimuth and dip
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

\seealso{TOSPHERE}
\examples{

A = list(x=12 ,y=2, z=-3 )
tosphereL(A)

}

\keyword{misc}
