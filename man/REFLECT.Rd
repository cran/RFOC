\name{REFLECT}
\alias{REFLECT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{reflect pole}
\description{
  Reflect pole to lower hemisphere
}
\usage{
REFLECT(A)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{structure of azimuth and Dips in degrees}
}

\value{

  list of:cartesian coordinates of reflected pole
 \item{x}{x-coordinate} 
  \item{y}{y-coordinate} 
   \item{z}{z-coordinate} 
  \item{az}{azimuth, degrees} 
   \item{dip}{dip, degrees} 
  
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{Preflect}
\examples{

A = list(az=231, dip = -65)
REFLECT(A)


}

\keyword{misc}
