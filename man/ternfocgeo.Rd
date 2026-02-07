\name{ternfocgeo}
\Rdversion{1.1}
\alias{ternfocgeo}
\title{Ternary Focals
}
\description{Ternary plots of rake categories (strike-slip, normal, thrust)
  distributed geographically.
}
\usage{
ternfocgeo(CMTSOL, PROJ = NULL, icut = 5, ndivs = 10,
 bbox=c(0,1, 0, 1), PLOT = TRUE, add = FALSE, RECT = FALSE)
}
\arguments{
  \item{CMTSOL}{see output of getCMT for list input
}
  \item{PROJ}{Map projection
}
  \item{icut}{cut off for number of points in box, default=5
}
  \item{ndivs}{divisions of map area, default=10
  }
  \item{bbox}{bounding box for dividing the area,
    given as minX, maxX, minY, maxY;
    default=usr
    coordinates from par()
}
  
  \item{PLOT}{logical, default=TRUE
}
  \item{add}{logical, add to existing plot
}
  \item{RECT}{logical, TRUE=plot rectangles
}
  

}
\details{Program divides the area into blocks, tests each one
  for minimum number per block and plots a ternary plot
  for each block.
}
\value{Graphical Side Effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\examples{

N = 100
LATS = c(7.593004,  25.926045)
LONS = c(268.1563 , 305)
lon=rnorm(N, mean=mean(LONS), sd=diff(LONS)/2 )
lat=rnorm(N, mean=mean(LATS), sd=diff(LATS)/2)

str1=runif(N,50,100)
dip1=runif(N,10, 80)
rake1=runif(N,5, 180)


dep=runif(N,1,15)
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

##  points(XY$x, XY$y)

ternfocgeo(MEKS , PROJ, PLOT=TRUE, icut = 3,
ndivs = 4, add=TRUE, RECT=TRUE)

points(XY$x, XY$y, pch=8, col="purple" )

#################  next restrict the boxes to a specific region
plot(range(XY$x), range(XY$y), type='n', asp=1)
points(XY$x, XY$y)

ternfocgeo(MEKS , PROJ, PLOT=TRUE, icut = 3, ndivs = 5,
 bbox=c(-2000,2000,-2000,2000) , add=TRUE, RECT=TRUE)




  }

\seealso{PlotTernfoc, spherefocgeo, prepFOCS, RectDense
}

\keyword{misc}
