\name{SHradfoc}
\alias{SHradfoc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot SH-wave radiation}
\description{
  Plot SH-wave radiation with information from the pickfile and
  waveform data
}
\usage{
SHradfoc(A, MEC, GU, pscale, col)
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
 Image plot of the SH radiation pattern
}
\value{
 Graphical Side effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{imageSH}
\examples{
MEC = SDRfoc(65, 32, -34, u=TRUE, ALIM=c(-1,-1, +1, +1), PLOT=FALSE)

SHradfoc(NULL, MEC , NULL, TRUE, rainbow(100) )



}
\keyword{hplot}


