\name{tocartL}
\alias{tocartL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert to cartesian coordinate}
\description{
 Convert azimuth-dip to cartesian coordinates with list as argument
}
\usage{
tocartL(A)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
   \item{az}{degrees, azimuth}
   \item{dip}{degrees, dip}
   }
   
}

\value{
  List
  \item{x}{x-coordinate}
  \item{y}{y-coordinate}
    \item{z}{z-coordinate}
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}
\note{
x positive north, y positive east, z positive downward
 
}
\seealso{TOCART.DIP, TOCART, tosphereL, to.spherical}
\examples{

A = list(az=23, dip=84)
tocartL(A)
  
}

\keyword{misc}
