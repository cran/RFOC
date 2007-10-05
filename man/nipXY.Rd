\name{nipXY}
\alias{nipXY}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fault-Slip vector plot}
\description{
  Plots a fault plane and the slip vector.  Used for geographic
  representation of numerous focal spheres.
}
\usage{
nipXY(MEC, x = x, y = y, size = c(1, 1), fcol = gray(0.9), nipcol = "black", cex = 0.4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MEC}{MEC structure}
  \item{x}{coordinate on plot }
  \item{y}{ coordinate on plot }
  \item{size}{size in user coordinates}
  \item{fcol}{color for plotting}
  \item{nipcol}{color of slip point}
  \item{cex}{character expansion for slip point}
}
\details{
  Slip vector is the cross product of the poles to the
  fault plane and auxilliary planes.  
}
\value{
  LIST
  \item{Q}{output of qpoint}
  \item{N}{slip vector}

}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{qpoint, CROSSL, lowplane,  TOCART  }
\examples{
MEC = SDRfoc(65,25,13, u=FALSE, ALIM=c(-1,-1, +1, +1), PLOT=FALSE)

plot(c(-1,1), c(-1,1), type='n', axes=FALSE, ann=FALSE, asp=1)

   nipXY( MEC,  0, 0 , fcol = 'red', nipcol='blue',  size = c(.6,.6), cex=2 )

}

\keyword{hplot}
\keyword{aplot}
