\name{SVradfoc}
\alias{SVradfoc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot SV-wave radiation}
\description{
  Plot SV-wave radiation with information from the pickfile and
  waveform data
}
\usage{
SVradfoc(A, MEC, GU, pscale, col)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{Pickfile structure}
  \item{MEC}{MEC structure}
  \item{GU}{Waveform Event Structure}
  \item{pscale}{logical (not used)}
  \item{col}{color palette}
}
\details{
 Image plot of the SV radiation pattern
}
\value{
 Graphical Side effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{imageSV}
\examples{
MEC = SDRfoc(65, 32, -34, u=TRUE, ALIM=c(-1,-1, +1, +1), PLOT=FALSE)

SVradfoc(NULL, MEC , NULL, TRUE, rainbow(100) )



}
\keyword{hplot}
