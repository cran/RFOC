\name{ROTY}
\alias{ROTY}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Y-axis Rotation Matrix}
\description{
 Matrix rotation about the Y-axis
}
\usage{
ROTY(deg)
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

\seealso{ROTX, ROTZ}
\examples{
v = c(1,4,5)
A = ROTY(23)
vp = c(v, 1)  %*% A

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
\keyword{array}

