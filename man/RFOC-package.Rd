\name{RFOC-package}
\alias{RFOC-package}
\alias{RFOC}
\docType{package}
\title{
Calculates and plot Earthquake Focal Mechanisms
}
\description{Graphics for statistics on a sphere, as applied to
geological fault data, crystallography,
earthquake focal mechanisms,
radiation patterns,
ternary plots and geographical/geological maps.
Given strike-dip-rake or a set of fault planes, 
focal planes, RFOC creates structures for manipulating
and plotting
earthquake focal mechanisms
as individual plots or distributed spatially maps.

RFOC can be used for analysis of plane orientation,
geologic structure, distribution of stress and strain
analyses.

}
\details{
\tabular{ll}{
Package: \tab RFOC\cr
Type: \tab Package\cr
Version: \tab 2.0-00\cr
Date: \tab 2011-05-26\cr
License: \tab GPL\cr
}
Visualize focal mechanisms in a
number of modes, including: beachball plots, radiation plots,
fault planes and ternary diagrams.  Shows spatial distribution of
spherically distributed data.

}
\author{
Jonathan M. Lees
Maintainer: Jonathan M. Lees <jonathan.lees@unc.edu>
}
\references{
  J. M.  Lees.  Geotouch: Software for three and four dimensional
  GIS in the earth sciences.   \emph{Computers  and  Geosciences} ,
  26(7):751--761, 2000.
  

  K.~Aki and P.~G. Richards.\emph{Quantitative seismology}. University
  Science Books, Sausalito, Calif., 2nd edition, 2002.

  Snyder, John P., 1987,  Map Projections-a working manual,
  USGS-Professional Paper, 383p.

  C. Frohlich.   Triangle   diagrams:   ternary  graphs   to display
  similarity and   diversity  of earthquake    focal
  mechanisms.   \emph{Physics  of the Earth  and Planetary Interiors},
  75:193-198,   1992.


}
\keyword{ package }
\seealso{RSEIS, GEOmap, zoeppritz
}
\examples{

#############  plot one focal mechanism:
M = SDRfoc(-25, 34, 16,u = FALSE, ALIM = c(-1, -1, +1, +1), PLOT=TRUE)


#############  plot many P-axes:
paz = rnorm(100, mean=297, sd=100)
pdip = rnorm(100, mean=52, sd=20)
net()
focpoint(paz, pdip, col='red',  pch=3, lab="", UP=FALSE)

#############

#### Show many Focal mechanisms on a plot:

Z1 = c(159.33,51.6,206,18,78,
161.89,54.5,257,27,133,
170.03,53.57,-44,13,171,
154.99,50.16,-83,19,-40,
151.09,47.15,123,23,-170,
176.31,51.41,-81,22,122,
153.71,46.63,205,28,59,
178.39,51.21,-77,16,126,
178.27,51.1,-86,15,115,
177.95,51.14,-83,25,126,
178.25,51.18,215,16,27
)

MZ = matrix(Z1, ncol=5, byrow=TRUE)

plot(MZ[,1], MZ[,2], type='n', xlab="LON", ylab="LAT", asp=1)

for(i in 1:length(MZ[,1]))
{
paste(MZ[i,3], MZ[i,4], MZ[i,5])


MEC =  SDRfoc(MZ[i,3], MZ[i,4], MZ[i,5], u=FALSE, ALIM=c(-1,-1, +1, +1), PLOT=FALSE)
fcol =  foc.color(foc.icolor(MEC$rake1), pal=1)
justfocXY(MEC, x=MZ[i,1], y =MZ[i,2] , size = c(1, 1), fcol =fcol , fcolback = "white", xpd = TRUE)


}



}
