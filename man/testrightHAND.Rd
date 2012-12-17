\name{testrightHAND}
\alias{testrightHAND}
\title{Test Right Hand of tensor
}
\description{Test Right Hand of tensor
}
\usage{
testrightHAND(U)
}
\arguments{
  \item{U}{3 by 3 matrix
}
}
\details{The fuction eigen does not
  always produce a right-handed eigenvector
  matrix.  The code tests each cross product
  to see if it creates a right-hand system.
}
\value{logical vector
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{forcerighthand
}
\examples{
Mtens <- c(-0.412, 0.084, 0.328 ,0.398, -1.239, 1.058)

M1 <-  matrix(c(Mtens[1], Mtens[4], Mtens[5], Mtens[4],
Mtens[2], Mtens[6], Mtens[5],Mtens[6],
Mtens[3]), ncol=3, nrow=3, byrow=TRUE)

E1 <-  eigen(M1)
testrightHAND(E1$vectors) 

}
\keyword{misc}
