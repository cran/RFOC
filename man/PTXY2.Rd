\name{PTXY2}
\alias{PTXY2}
\title{Plot P-T Axes
}
\description{given a focal mechanism, add P-T lines to a plot
}
\usage{
PTXY2(x = x, y = y, MEC, focsiz, pt = 0, ...)
}
\arguments{
  \item{x}{x-location on plot
}
  \item{y}{y-location on plot
}
  \item{MEC}{Focal Mechanism list from SDRFOC
}
  \item{focsiz}{size of mechanism, inches
}
  \item{pt}{pt = 0(plot both), 1=only P axes, 2=only T axes, default=0
}
  \item{\dots}{graphical parameters
}
}
\details{This is a summary plot to be used instead of Beach Balls.
}
\value{Graphical Side Effects
}
\references{
Lees, J. M., Geotouch: Software for Three and Four Dimensional GIS in the Earth Sciences, Computers & Geosciences, 26, 7, 751-761, 2000.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{nipXY, justfocXY
}
\examples{

###  HAiti Earthquake Jan, 2010
MEC <-  SDRfoc(71, 64, 25 , u=FALSE, ALIM=c(-1,-1, +1, +1), PLOT=FALSE)
plot(c(0, 1), c(0,1), type='n', asp=1)
u <- par("usr")

justfocXY(MEC, x=.5,  y= .5,  focsiz=0.5,
fcol ='brown' , fcolback = "white", xpd = TRUE)

 PTXY2(1.0, .5 , MEC  ,0.5, col="purple", lwd=3 )
 
nipXY(MEC, x = 0.25, y = .5, focsiz=0.5,
fcol ='purple', nipcol = "black", cex = 0.4)
#####  or
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

plot(range(XY$x), range(XY$y), type='n', asp=1)

for(i in 1:length(XY$x))
{
  Msdr = CONVERTSDR(MEKS$str1[i], MEKS$dip1[i],MEKS$rake1[i])
     MEC = MRake(Msdr$M)
       MEC$UP = FALSE

         jcol =  foc.color(foc.icolor(MEC$rake1), pal=1)

PTXY2(XY$x[i], XY$y[i] , MEC  ,focsiz=0.5, col=jcol, lwd=3)

}




}
\keyword{misc}
