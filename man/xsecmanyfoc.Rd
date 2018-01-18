\name{xsecmanyfoc}
\alias{xsecmanyfoc}
\title{Plot Focal Mechs at X-Y position on cross sections
}
\description{
  Plot Focal Mechs at X-Y positions on cross sectionsor other plots
  that do not have geographic coordinates and projection.
}
\usage{
xsecmanyfoc(MEK,  theta=NULL, focsiz = 0.5, 
 foccol = NULL, UP=TRUE, focstyle=1, LEG = FALSE, DOBAR = FALSE)
}
\arguments{
  \item{MEK}{List of Focal Mechanisms, see details
}
  \item{focsiz}{focal size, inches
  }
  \item{theta}{degrees, angle from north  for projecting the focal mechs
  }
   
  \item{foccol}{focal color, default is to calculate based on rake
  }

  \item{UP}{logical, UP=TRUE means plot upper hemisphere (DEFAULT=TRUE) 
  }
 \item{focstyle}{integer, 1=beach ball, 2=nipplot }
  
  \item{LEG}{logical, TRUE= add focal legend for color codes
}
  \item{DOBAR}{add strike dip bar at epicenter
}
}
\details{
Input MEK list contains

     MEKS = list(lon=0, lat=0, str1=0, dip1=0, rake1=0, dep=0, name="",
     Elat=0, Elon=0, x=0, y=0)

     The x, y coordinates of the input list are
     location where the focals will be plotted.
     For cross sections x=distance along the
     section and y would be depth.  The focal mechs are
     added to the current plot.

  
   }

\value{Graphical Side Effects

}
\references{
 Lees, J. M., Geotouch: Software for Three and Four Dimensional GIS
     in the Earth Sciences, Computers & Geosciences, 26, 7, 751-761,
     2000.

  
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
  If theta is NULL focals are plotted as if they were
  on a plan view.
  If theta is provided, however, the mechs are plotted
  with view from the vertical cross section.
  The cross section is taken at two points.
  Theta should be determined by viewing  the cross section with the first
  point on the left and the second on the right.  The
  view angle is through the section measured in degrees from north.
}


\seealso{justfocXY, plotmanyfoc
}
\examples{

############# create and  plot the mechs in plan view:
N = 20
lon=runif(20, 235, 243)
     lat=runif(20, 45.4, 49)
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

     plotmanyfoc(MEKS, PROJ, focsiz=0.5)

ex = range(XY$x)
why = range(XY$y)


JJ = list(x=ex, y=why)


SWA = GEOmap::eqswath(XY$x, XY$y, MEKS$dep, JJ, width = diff(why) , PROJ = PROJ)


MEKS$x = rep(NA, length(XY$x))
MEKS$y = rep(NA, length(XY$y))


MEKS$x[SWA$flag] = SWA$r
MEKS$y[SWA$flag] = -SWA$depth
bigR = sqrt(  (JJ$x[2]-JJ$x[1])^2 + (JJ$y[2]-JJ$y[1])^2)

plot(c(0,bigR)   , c(0, min(-SWA$depth)) , type='n',
 xlab="Distance, KM", ylab="Depth")
points(SWA$r, -SWA$depth)

xsecmanyfoc(MEKS, focsiz=0.5,  LEG = TRUE, DOBAR=FALSE)
title("cross section: focals are plotted as if in plan view")


ang1 = atan2( JJ$y[2]-JJ$y[1] , JJ$x[2]-JJ$x[1])

 degang =  ang1*180/pi


xsecmanyfoc(MEKS, focsiz=0.5, theta=degang, LEG = TRUE, DOBAR=FALSE)
title("cross section: focals are view from the side projection (outer hemisphere)")





}
\keyword{misc}
