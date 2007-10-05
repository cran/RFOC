\name{roty3}
\alias{roty3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Rotate about the y axis}
\description{
 3x3 Rotation  about the y axis
}
\usage{
roty3(deg)
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

\seealso{rotz3, rotx3, ROTX, ROTZ, ROTY}
\examples{

a = 45
roty3(a)


}
\keyword{misc}
