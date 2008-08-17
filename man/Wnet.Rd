\name{Wnet}
\alias{Wnet}

\title{Wulff Stereonet}
\description{
 plot a Wulff Stereonet (Equal-Angle)
}
\usage{
Wnet(add = FALSE, col = gray(0.7), border = "black", lwd = 1)
}

\arguments{
  \item{add}{Logical, TRUE=add to existing plot}
  \item{col}{color}
  \item{border}{border color}
  \item{lwd}{line width }
}
\details{
 Plots equal-angle stereonet as opposed to equal-area.
}
\value{
  graphical side effects
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{net, pnet}
\examples{
Wnet()

}

\keyword{hplot}
