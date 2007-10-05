\name{ROTX}
\alias{ROTX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{X-axis Rotation Matrix}
\description{
 Matrix rotation about the X-axis
}
\usage{
ROTX(deg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{deg}{Angle in degrees}
}

\value{
   A 4 by 4 matrix for rotation and translation for 3-D transformation
}
\references{Rogers and Adams, 1990, Mathematical Elements for Computer
  Graphics, McGraw-Hill, 611p.}
\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{ROTY, ROTZ}
\examples{
v = c(1,4,5)
A = ROTX(23)
vp = c(v, 1)  %*% A

}
\keyword{dplot}
\keyword{array}
