\name{rotx3}
\alias{rotx3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Rotate about the x axis}
\description{
 3x3 Rotation  about the x axis
}
\usage{
rotx3(deg)
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

\seealso{roty3, rotz3, ROTX, ROTZ, ROTY}
\examples{

a = 45
rotx3(a)


}
\keyword{misc}
