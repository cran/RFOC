\name{MomentDist}
\alias{MomentDist}
\title{Distance Between Moment Tensors
}
\description{Calculate the distance between moment tensors
  based on quaternions.
}
\usage{
MomentDist(E1, E2)
}
\arguments{
  \item{E1}{Moment tensor
}
  \item{E2}{Moment tensor
}
}
\details{Moment tensors should be right handed.
}
\value{angle in degrees
}
\references{Tape and Tape, 2012
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{forcerighthand, testrightHAND
}
\examples{

Mtens = c(-0.412, 0.084, 0.328 ,0.398, -1.239, 1.058)
M1 = matrix(c(Mtens[1], Mtens[4], Mtens[5], Mtens[4], Mtens[2],
Mtens[6], Mtens[5],Mtens[6], Mtens[3]), ncol=3, nrow=3, byrow=TRUE)



Mtens = c(5.054, -2.235, -2.819, -0.476, 5.420, 5.594)
M2 = matrix(c(Mtens[1], Mtens[4], Mtens[5], Mtens[4], Mtens[2],
Mtens[6], Mtens[5],Mtens[6], Mtens[3]), ncol=3, nrow=3, byrow=TRUE)

E1 = eigen(M1)

###  make sure these are a right handed system,
###   ie x1 cross x2 = x3


E2 = eigen(M2)

###  make sure these are a right handed system,
###   ie x1 cross x2 = x3
testrightHAND(E1$vectors) 
testrightHAND(E2$vectors) 

E1$vectors = forcerighthand(E1$vectors)

E2$vectors = forcerighthand(E2$vectors)


testrightHAND(E1$vectors) 
testrightHAND(E2$vectors) 

MomentDist(E1, E2)


}
\keyword{misc}
