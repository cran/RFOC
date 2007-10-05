\name{pcirc}
\alias{pcirc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Circle Plot}
\description{
  Add a circle to a plot, with cross-hairs
}
\usage{
pcirc(gcol = "black", border = "black", ndiv = 36)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gcol}{color of crosshairs}
  \item{border}{ border color }
  \item{ndiv}{ number of divisions for the circle }
}

\value{
no return values, used for side effects
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{net }
\examples{
net()
pcirc(gcol = "green", border = "purple", ndiv = 36)

}

\keyword{aplot}% __ONLY ONE__ keyword per line
