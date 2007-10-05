\name{rotz3}
\alias{rotz3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Rotate about the z axis}
\description{
 3x3 Rotation  about the z axis
}
\usage{
rotz3(deg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{deg}{ angle, degrees }
}
\details{
 returns a 3 by 3 rotation matrix
}
\value{
 matrix, 3 by 3
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{roty3, rotx3, ROTX, ROTZ, ROTY}
\examples{

a = 45
rotz3(a)


}
\keyword{misc}
