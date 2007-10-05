\name{addmecpoints}
\alias{addmecpoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add points to Focal Mech}
\description{
 Add a standard set of points to a Focal Mechanism
}
\usage{
addmecpoints(MEC, pch = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MEC}{MEC structure list}
  \item{pch}{plotting character}
}

\value{
Graphical Side effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{SDRfoc, focpoint}
\examples{

MEC= SDRfoc(12,34,-120)
addmecpoints(MEC)


}

\keyword{aplot}
