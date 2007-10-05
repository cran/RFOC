\name{PlotPlanes}
\alias{PlotPlanes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Fault an Auxilliary Planes}
\description{
 Plot both fault and auxilliary planes
}
\usage{
PlotPlanes(MEC, col1 = 1, col2 = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MEC}{ MEC structure }
  \item{col1}{color for plane 1 }
  \item{col2}{color for plane 2 }
}
\details{
  Given MEC structure and focal mechanism plot both planes.
  This code adds to existing plot, so net() should be called.
}
\value{
 Graphical Side Effects
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{net, lowplane}
\examples{
net()

MFOC1 = SDRfoc(65,25,13, u=FALSE, ALIM=c(-1,-1, +1, +1), PLOT=FALSE)
PlotPlanes(MFOC1, 'green', 'red' )


}

\keyword{aplot}
