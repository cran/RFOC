\name{hudson.net}
\alias{hudson.net}
\title{Hudson Net Plot
}
\description{Plot a Hudson plot as preparation for plotting
  T-k values for focal mechanisms.
}
\usage{
hudson.net(add = FALSE, POINTS = TRUE, TEXT = TRUE, colint = "grey", colext = "black")
}
\arguments{
  \item{add}{logical, TRUE=add to existing plot
}
  \item{POINTS}{logical, TRUE=add points
}
  \item{TEXT}{logical, TRUE=add points
}
  \item{colint}{color for interior lines
}
  \item{colext}{color for exterior lines
}
}
\details{Draws a T-k plot for moment tensors
}
\value{Graphical Side effects
}
\references{Hudson, J.A., Pearce, R.G. and Rogers, R.M., 1989. Source time plot for inversion of the moment tensor, J. Geophys. Res., 94(B1), 765-774.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{hudson.plot
}
\examples{
hudson.net()
Mtens = c(-0.412, 0.084, 0.328 ,0.398, -1.239, 1.058)
M1 = matrix(c(Mtens[1], Mtens[4], Mtens[5], Mtens[4], Mtens[2], Mtens[6], Mtens[5],Mtens[6], Mtens[3]), ncol=3, nrow=3, byrow=TRUE)
E1 = eigen(M1)

hudson.plot(E1$values)



}
\keyword{misc}
