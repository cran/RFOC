\name{spherefocgeo}
\Rdversion{1.1}
\alias{spherefocgeo}
\title{SphereFocGeo
}
\description{Spherical Projections of PT axes
  distributed geographically.
}
\usage{
spherefocgeo(CMTSOL, PROJ = NULL, icut = 5,
ndivs = 10,  bbox=c(0,1, 0, 1), PLOT = TRUE,
 add = FALSE, RECT = FALSE, pal = terrain.colors(100))
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
  \item{pal}{palette fo rimages in each box
}
}
\details{Program divides the area into blocks, tests each one
  for minimum number per block and projects
  the P and T axes onto an equal area  stereonet.
}
\value{Graphical Side Effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{PlotPTsmooth, ternfocgeo, prepFOCS, RectDense
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

 MEKS = list(lon=lon, lat=lat, str1=str1, dip1=dip1,
 rake1=rake1, dep=dep, name=name)
PROJ = GEOmap::setPROJ(type=2, LAT0=mean(lat) , LON0=mean(lon) )   ##   utm
XY = GEOmap::GLOB.XY(lat, lon, PROJ)
plot(range(XY$x), range(XY$y), type='n', asp=1)

  points(XY$x, XY$y)
spherefocgeo(MEKS, PROJ, PLOT=TRUE, icut = 3, ndivs = 4,
 add=TRUE, pal=terrain.colors(100), RECT=TRUE )







\dontrun{

plot(x=range(IZ$x), y=range(IZ$y), type='n', asp=1, axes=FALSE, ann=FALSE)

image(x=IZ$x, y=IZ$y, z=(UZ), col=blues, add=TRUE)

image(x=IZ$x, y=IZ$y, z=(AZ), col=terrain.colors(100) , add=TRUE)


 plotGEOmapXY(haiti.map,
              LIM = c(Lon.range[1],Lat.range[1] ,
Lon.range[2] ,Lat.range[2]),
              PROJ =PROJ, MAPstyle = 2,
 MAPcol = 'black' , add=TRUE  )

H = rectPERIM(JMAT$xo, JMAT$yo)


antipolygon(H$x ,H$y,   col=grey(.85)  , corner=1, pct=.4)

sqrTICXY(H , PROJ, side=c(1,2,3,4),   LLgrid=TRUE, col=grey(.7) )


spherefocgeo(OLDCMT, PROJ, PLOT=TRUE, add=TRUE, pal=topo.colors(100) )



}





}
\keyword{misc}
