\name{pnet}
\alias{pnet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plot stereonet}
\description{
 Plots stereonet created by makenet
}
\usage{
pnet(MN, add = FALSE, col = gray(0.7), border = "black", lwd = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MN}{Net strucutre created by makenet}
  \item{add}{TRUE= add to existing plot}
  \item{col}{ color of lines }
  \item{border}{color for outside border}
  \item{lwd}{line width }
}

\value{
 Used Graphical Side Effects.
}


\references{Snyder, John P., 1987,  Map Projections-a working manual, USGS-Professional Paper, 383p.  pages 185-186}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{net, pnet}
\examples{

MN = makenet()

  pnet(MN)

}
\keyword{aplot}
