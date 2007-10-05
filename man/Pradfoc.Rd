\name{Pradfoc}
\alias{Pradfoc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot P-wave radiation}
\description{
  Plot P-wave radiation with information from the pickfile and
  waveform data
}
\usage{
Pradfoc(A, MEC, GU, pscale, col)
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
 Image plot of the P radiation pattern
}
\value{
 Graphical Side effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{imageP}
\examples{
MEC = SDRfoc(65, 32, -34, u=TRUE, ALIM=c(-1,-1, +1, +1), PLOT=FALSE)

Pradfoc(NULL, MEC , NULL, TRUE, rainbow(100) )



}
\keyword{hplot}
