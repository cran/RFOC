\name{RotTP}
\alias{RotTP}
\title{Rotate T-P axes
}
\description{Rotate T-P axes
}
\usage{
RotTP(rotmat, strk1, dip1)
}
\arguments{
  \item{rotmat}{rotation matrix, 3 by 3 
}
  \item{strk1}{strike angle
}
  \item{dip1}{dip angle
}
}
\details{These are used as functions auxiallry to rotateFoc.
}
\value{
  list:
  \item{strk}{strike angle}
  \item{dip}{dip angle}
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\seealso{Rotfocphi, TP2XYZ
}
\examples{

phi = 18

MAT = JMAT(phi)

RotTP(MAT, 30, 40)

}
\keyword{misc}
