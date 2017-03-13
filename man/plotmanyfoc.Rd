\name{plotmanyfoc}
\Rdversion{1.1}
\alias{plotmanyfoc}
\title{Plot Many Focals
}
\description{Plot a long list of focal mechanisms
}
\usage{
plotmanyfoc(MEK, PROJ, focsiz = 0.5, foccol = NULL,
UP=TRUE, focstyle=1, PMAT = NULL, LEG = FALSE, DOBAR = FALSE)
}
\arguments{
  \item{MEK}{List of Focal Mechanisms, see details
}
  \item{PROJ}{Projection
}
  \item{focsiz}{focal size, inches
}
  \item{foccol}{focal color
  }
  \item{UP}{logical, UP=TRUE means plot upper hemisphere (DEFAULT=TRUE) 
  }
    \item{focstyle}{integer, 1=beach ball, 2=nipplot, 3=strike-slip, 4=P-T, 5=P, 6=T }
  
  \item{PMAT}{Projection Matrix from persp
}
  \item{LEG}{logical, TRUE= add focal legend for color codes
}
  \item{DOBAR}{add strike dip bar at epicenter
}
}
\details{Input MEK list contains
  

 MEKS = list(lon=0, lat=0, str1=0, dip1=0, rake1=0, dep=0, name="", Elat=0, Elon=0)


  
}
\value{Graphical Side Effects
}
\references{
Lees, J. M., Geotouch: Software for Three and Four Dimensional GIS in the Earth Sciences, Computers & Geosciences, 26, 7, 751-761, 2000.

}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{justfocXY
}
\examples{

set.seed(2015)
lon=runif(20, 268.1563 , 305)
lat=runif(20, 7.593004,  25.926045)
str1=runif(20,50,100)
dip1=runif(20,10, 80)
rake1=runif(20,5, 180)

dep=runif(20,1,15)
name=seq(from=1, to=length(lon), by=1)
Elat=NULL
Elon=NULL

 MEKS = list(lon=lon, lat=lat, str1=str1, dip1=dip1,
rake1=rake1, dep=dep, name=name)

PROJ = GEOmap::setPROJ(type=2, LAT0=mean(lat) , LON0=mean(lon) )   ##   utm

XY = GEOmap::GLOB.XY(lat, lon, PROJ)

plot(range(XY$x), range(XY$y), type='n', asp=1)

plotmanyfoc(MEKS, PROJ, focsiz=0.5)





}
\keyword{misc}
