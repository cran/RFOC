\name{addPT}
\alias{addPT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add P-T Axis to focal plot}
\description{
 Add Pressure and tension Axes to focal mechanism
}
\usage{
addPT(MEC, pch = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MEC}{ MEC structure }
  \item{pch}{plotting character }
}

\value{
Graphical Side Effect
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{addPTarrows}
\examples{


MEC =SDRfoc(65,25,13, u=FALSE, ALIM=c(-1,-1, +1, +1), PLOT=FALSE)
Beachfoc(MEC)
addPT(MEC, pch = 5)

}

\keyword{aplot}
