\name{mc2cart}
\alias{mc2cart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert azimuth, dip to Cartesian Coordinates}
\description{
  takes the pole information from a steroplot and returns the cartesian coordinates
}
\usage{
mc2cart(az, dip)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{az}{degrees, orientation angle, from North}
  \item{dip}{degrees, dip of pole}
}

\value{
 list of x,y,z values
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}


\examples{
v1  = mc2cart(65,32)
v2  = mc2cart(135,74)


}

\keyword{misc}
