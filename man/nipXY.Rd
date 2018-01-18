\name{nipXY}
\alias{nipXY}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fault-Slip vector plot}
\description{
  Plots a fault plane and the slip vector.  Used for geographic
  representation of numerous focal spheres.
}
\usage{
nipXY(MEC, x = x, y = y, focsiz=1, fcol = gray(0.9), nipcol = "black", cex = 0.4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MEC}{MEC structure}
  \item{x}{coordinate on plot }
  \item{y}{ coordinate on plot }
  \item{focsiz}{size in inches}
  \item{fcol}{color for plotting}
  \item{nipcol}{color of slip point}
  \item{cex}{character expansion for slip point}
}
\details{
  Slip vector is the cross product of the poles to the
  fault plane and auxilliary planes.  
}
\value{
  LIST
  \item{Q}{output of qpoint}
  \item{N}{slip vector}

}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{qpoint, CROSSL, lowplane,  TOCART  }
\examples{

set.seed(2015)
N = 20
lon=runif(20, 268.1563 , 305)
lat=runif(20, 7.593004,  25.926045)
str1=runif(20,50,100)
dip1=runif(20,10, 80)
rake1=runif(20,5, 180)

dep=runif(20,1,15)
name=seq(from=1, to=length(lon), by=1)
Elat=NULL
Elon=NULL
yr = rep(2017, times=N)
jd = runif(N, min=1, max=365)

 MEKS = list(lon=lon, lat=lat, str1=str1, dip1=dip1,
rake1=rake1, dep=dep, name=name, yr=yr, jd = jd)

PROJ = GEOmap::setPROJ(type=2, LAT0=mean(lat) , LON0=mean(lon) )   ##   utm

XY = GEOmap::GLOB.XY(lat, lon, PROJ)

plot(range(XY$x), range(XY$y), type='n', asp=1, xlab='km', ylab='km' )
for(i in 1:length(XY$x))
{
  Msdr = CONVERTSDR(MEKS$str1[i], MEKS$dip1[i],MEKS$rake1[i])
     MEC = MRake(Msdr$M)
       MEC$UP = FALSE

         jcol =  foc.color(foc.icolor(MEC$rake1), pal=1)



nipXY(MEC, x = XY$x[i], y = XY$y[i], focsiz=0.5, fcol = jcol, nipcol = 'black' , cex = 1)
}



}

\keyword{hplot}
\keyword{aplot}
