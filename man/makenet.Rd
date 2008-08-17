\name{makenet}
\alias{makenet}

\title{Equal-Angle Stereonet}
\description{
Creates but does not plot an Equal-Angle (Schmidt) Stereonet
}
\usage{
makenet()
}

\value{
  
 
  list of x,y, values for drawing lines
  \item{x1}{x-coordinate start of lines}
  \item{y1}{y-coordinate start of lines}
  \item{x2}{x-coordinate end of lines}
  \item{y2}{y-coordinate end of lines}

  
}
\references{Snyder, John P., 1987,  Map Projections-a working manual, USGS-Professional Paper, 383p.  pages 185-186}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{net, pnet}
\examples{

MN = makenet()

  pnet(MN)

}
\keyword{misc}
