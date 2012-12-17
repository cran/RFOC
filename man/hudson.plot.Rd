\name{hudson.plot}
\alias{hudson.plot}
\title{Hudson Source Type Plot
}
\description{Hudson Source Type Plot
}
\usage{
hudson.plot(m, col = "red", pch = 21, lwd = 2, cex = 1, bg="white")
}
\arguments{
  \item{m}{vector of eigen values, sorted
}
  \item{col}{color
}
  \item{pch}{plotting char
}
  \item{lwd}{line width
}
  \item{cex}{character expansion
  }
  \item{bg}{background color for filled symbols
  }
   
}
\details{Add to existing Hudson net
}
\value{Side effects
}
\references{Hudson, J.A., Pearce, R.G. and Rogers, R.M., 1989. Source time plot for inversion of the moment tensor, J. Geophys. Res., 94(B1), 765-774.
}
\author{
  
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{hudson.net
}
\examples{
hudson.net()

Mtens <- c(-0.412, 0.084, 0.328 ,0.398, -1.239, 1.058)

M1 <- matrix(c(Mtens[1], Mtens[4], Mtens[5], Mtens[4],
Mtens[2], Mtens[6], Mtens[5],Mtens[6],
Mtens[3]), ncol=3, nrow=3, byrow=TRUE)

E1 <-  eigen(M1)

hudson.plot(E1$values)


}
\keyword{misc}
