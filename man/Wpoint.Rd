\name{Wpoint}
\alias{Wpoint}

\title{Plot points on Wulff Stereonet}
\description{
Adds points to Wulff Equal-Angle Stereonet
}
\usage{
Wpoint(az1, dip1, col = 2, pch = 5, lab = "", UP = FALSE)
}

\arguments{
  \item{az1}{azimuth angle, degrees}
  \item{dip1}{dip angle, degrees }
  \item{col}{color  }
  \item{pch}{plotting character  }
  \item{lab}{label for point  }
  \item{UP}{logical, TRUE=Upperhemisphere }
}
\details{
  Wulff net point is added to existing plot.
}
\value{
graphical side effects
}

\author{ Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{Wnet}
\examples{

Wnet()
Wpoint(23, 34)

}

\keyword{aplot}
