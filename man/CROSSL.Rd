\name{CROSSL}
\alias{CROSSL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Vector Cross Product}
\description{
  returns cross product of two vectors in list format
}
\usage{
CROSSL(A1, A2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A1}{list x,y,z}
  \item{A2}{list x,y,z}
}

\value{
  List
  \item{x,y,z}{input vector}
  \item{az}{azimuth, degrees}
  \item{dip}{dip, degrees}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{RSEIS::xprod}
\examples{

A1 = list(x=1,y=2, z=3)
A2 = list(x=12,y=-2, z=-5)

N = CROSSL(A1, A2)

}

\keyword{misc}
