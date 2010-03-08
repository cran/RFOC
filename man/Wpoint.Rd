\name{Wpoint}
\alias{Wpoint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot points on Wulff Stereonet}
\description{
Adds points to Wulff Equal-Angle Stereonet
}
\usage{
Wpoint(az1, dip1, col = 2, pch = 5, lab = "", UP = FALSE)
}
%- maybe also 'usage' for other objects documented here.
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
