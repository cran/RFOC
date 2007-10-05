\name{plotfoc}
\alias{plotfoc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Focal Radiation Patterns}
\description{
  Takes a MEC structure and plots all three radiation patterns.
}
\usage{
plotfoc(MEC)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MEC}{MEC list}
}
\details{
  Plot makes three figures after calling par(mfrow=c(3,1)).
  
}
\value{
   Graphical Side Effects.
  
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
Basic MEC List Structure
  \tabular{ll}{
  az1 \tab azimuth angle plane 1, degrees\cr
  dip1 \tab dip angle plane 1, degrees\cr
  az2 \tab azimuth angle plane 2, degrees\cr
  dip2 \tab dip angle plane 2, degrees\cr
  dir \tab 0,1 to determine which section of focal sphere is shaded\cr
  rake1 \tab rake angle plane 1, degrees\cr
  dipaz1 \tab dip azimuth angle plane 1, degrees\cr
  rake2 \tab rake angle plane 2, degrees\cr
  dipaz2 \tab dip azimuth angle plane 2, degrees\cr
  P \tab pole list(az, dip) P-axis\cr
  T \tab pole list(az, dip) T-axis\cr
  U \tab pole list(az, dip) U-axis\cr
  V \tab pole list(az, dip) V-axis\cr
  F \tab pole list(az, dip) F-axis\cr
  G \tab pole list(az, dip) G-axis \cr
  sense \tab 0,1 to determine which section of focal sphere is shaded\cr
  M \tab list of focal parameters used in some calculations\cr
  UP \tab logical, TRUE=upper hemisphere\cr
  icol \tab index to suite of colors for focal mechanism\cr
  ileg \tab Kind of fault\cr
  fcol \tab color of focal mechanism\cr
  CNVRG \tab Character, note on convergence of solution\cr
  LIM \tab vector plotting region (x1, y1, x2, y2) \cr
}
}
\seealso{SDRfoc, Mrake, Pradfoc,   radiateSH,  radP,   radSV,     SVradfoc,
radiateP,  radiateSV,  radSH,  SHradfoc, imageP, imageSH, imageSV
}
\examples{

M = SDRfoc(-25, 34, 16,u = FALSE, ALIM = c(-1, -1, +1, +1), PLOT=FALSE)
plotfoc(M)


}

\keyword{hplot}
