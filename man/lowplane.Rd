\name{lowplane}
\alias{lowplane}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot one Fault plane on stereonet}
\description{
 takes azimuth and dip and projects the greaat circle on the focla sphere
}
\usage{
lowplane(az, dip, col = par("col"), UP = FALSE, PLOT = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{az}{degrees, azimuth of strike of plane}
  \item{dip}{ degrees, dip}
  \item{col}{color of plane}
  \item{UP}{upper/lower hemisphere}
  \item{PLOT}{add to plot}
}
\details{
  Here azimuth is measured from North, and represents the actual strike
  of the fault line.
}
\value{
 list of x,y coordinates of plane
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{net}
\examples{
net()
lowplane(65,23)

}

\keyword{aplot}
