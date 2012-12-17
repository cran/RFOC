\name{ROTY}
\alias{ROTY}

\title{Y-axis Rotation Matrix}
\description{
 Matrix rotation about the Y-axis
}
\usage{
ROTY(deg)
}

\arguments{
  \item{deg}{Angle in degrees}
}

\value{
  A 4 by 4 matrix for rotation and translation for 3-D transformation
}
\references{Rogers and Adams, 1990, Mathematical Elements for Computer
  Graphics, McGraw-Hill, 611p.}
\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{ROTX, ROTZ}
\examples{
v = c(1,4,5)
A = ROTY(23)
vp = c(v, 1)  %*% A

}

\keyword{dplot}
\keyword{array}

