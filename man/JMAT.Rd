\name{JMAT}
\alias{JMAT}
\title{Vertical Rotation matrix
}
\description{Vertical Rotation matrix
}
\usage{
JMAT(phi)
}
\arguments{
  \item{phi}{angle, degrees
}
}
\details{First rotate to plan, then within plane
  rotate to view angle.
}
\value{3 by 3 matrix
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{ROTX, ROTZ, ROTY
}
\examples{

phi = 18

MAT = JMAT(phi)

v1 = c(1,1,0)

v2 = MAT %*% v1 



}
\keyword{misc}
