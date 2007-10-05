\name{Preflect}
\alias{Preflect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reflect a pole through to the lower hemisphere}
\description{
  Takes a vector to a pole and reflects it to the lower hemisphere
}
\usage{
Preflect(az, dip)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{az}{azimuth angle, degrees }
  \item{dip}{dip in degrees }
}

\value{
  list
  \item{az }{azimuth angle, degrees}
  \item{dip }{dip in degrees}
  ...
}
\seealso{REFLECT}
\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\examples{


z = Preflect(65, -23)
z = Preflect(265, -23)


}

\keyword{misc}
