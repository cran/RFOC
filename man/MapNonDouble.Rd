\name{MapNonDouble}
\alias{MapNonDouble}
\title{Map moment tensors
}
\description{Plot moment tensors on map
}
\usage{
MapNonDouble(Locs, moments, sel = 1, siz = 0.2, col=rgb(1, .75, .75), PLANES = TRUE, add = FALSE, LEG=FALSE)
}
\arguments{
  \item{Locs}{Locations, x,y
}
  \item{moments}{list of moments: seven elements. See details.
}
  \item{sel}{integer, index of which to plot
}
  \item{siz}{size to plot, inches
  }
  \item{col}{color, either a single color, rgb, or a color palette.
  }
   \item{PLANES}{logical, whether to add nodal planes, default=TRUE
}
  \item{add}{logical, whether to add to plot, default=FALSE
  }
  \item{LEG}{logical, whether to add focal mech legend based on color
    coding,
    default=FALSE
  }
   
}
\details{
  
  Moment tensors are added to an existing plot.
  The first element of the list is the integer index of the event.
  The next six elements are the moments in
  the following order, c(Mxx, Myy, Mzz, Mzy, Mxz, Mxy) .

  If the data is in spherical coordinates, one must switch the
  sign of the Mrp and Mtp components, so:
    \verb{
    Mrr = Mzz
    Mtt = Mxx
    Mpp = Myy
    Mrt = Mxz
    Mrp = -Myz
    Mtp = -Mxy
}

  A color palette can be provided for some
  details of the radiation patterns, e.g.  col=rainbow(12).
  If col is NULL, the colors will be chosen according to
  focal.color from RFOC, based on rake of first nodal plane.

  If col is NULL, then the colors are set by foc.color
  and it is appropriate to add a legend.

}
\value{
  list:
  \item{FOC}{matrix, focal mechanism angles (strike, dip rake)}
    \item{LAB}{matrix, x-y location for labels}
}
\references{
 Ekstrom, G.; Nettles, M. & DziewoDski, A.
  The Global CMT Project 2004-2010: centroid-moment tensors for 13,017
  earthquakes Physics of the Earth and Planetary Interiors, 2012.
  
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{

  If events are read in using spherical rather than cartesian
  coordinates
  need a conversion:
   \verb{
    Mrr = Mzz
    Mtt = Mxx
    Mpp = Myy
    Mrt = Mxz
    Mrp = -Myz
    Mtp = -Mxy
}


}


\seealso{doNonDouble, ShadowCLVD, angles, nodalLines, PTaxes,
  focal.color,  foc.icolor
}
\examples{

\dontrun{

library(maps)
library(GEOmap)

##########  load the data
data(widdenMoments)

#################   to read in the data from a file,
##   GG = scan("widdenMoments.txt",sep=" ",
## what=list(ID=0,Event="",Lat=0,Long=0,Depth=0,Mw=0,ML=0,DC=0,
## CLVD=0,ISO=0,VR=0,nsta=0,Mxx=0,Mxy=0,Mxz=0,
##  Myy=0,Myz=0,Mzz=0,Mo=0,Ftest=0) )



GG = widdenMoments
Locs = list(y=GG$Lat,x=GG$Long)


ef = 1e20
moments = cbind(GG$ID, ef*GG$Mxx, ef*GG$Myy,
ef*GG$Mzz, ef*GG$Myz, ef*GG$Mxz,ef*GG$Mxy)




UTAH =  map('state', region = c('utah'), plot=FALSE )

mlon = mean(UTAH$x, na.rm=TRUE)
mlat = mean(UTAH$y, na.rm=TRUE)


Gutah   = maps2GEOmap(UTAH)


############   for mercator projection
PROJ =  setPROJ(type = 1, LAT0 = mlat , LON0 = mlon)
Glocs = GLOB.XY(Locs$y, Locs$x, PROJ       )
############   for UTM projection
PROJ =  setPROJ(type = 2, LAT0 = mlat , LON0 = mlon)
Glocs = GLOB.XY(Locs$y, Locs$x, PROJ       )

LIMlat = expandbound(Gutah$POINTS$lat)
LIMlon = expandbound(Gutah$POINTS$lon)

PLAT =  pretty(LIMlat)
 PLON  = pretty(LIMlon)

###############  plot the map

########  Utah is a little rectangular
dev.new(width=9, height=12)

plotGEOmapXY(Gutah, LIM = c(min(PLON), min(PLAT) , max(PLON) , max(PLAT)) ,
             PROJ=PROJ, axes=FALSE, xlab="", ylab="" )


### add tic marks
kbox = GLOB.XY(PLAT,PLON, PROJ)

      sqrTICXY(kbox , PROJ, side=c(1,2,3,4), LLgrid=TRUE, col=grey(.7) )

########  add focal mechs
siz = 0.2

MapNonDouble(Glocs, moments,col=NULL,  add=TRUE, LEG=TRUE)

  up = par("usr")
    ui = par("pin")
    ratx = (up[2] - up[1])/ui[1]
    raty = (up[4] - up[3])/ui[2]
usizx = siz * ratx

AXY = NoOverlap(Glocs$x,Glocs$y, usizx )

 MapNonDouble(AXY, moments,col=NULL,  add=TRUE, LEG=TRUE)

####  MapNonDouble(NXY, moments,col=NULL,  add=TRUE, LEG=TRUE)



}

}
\keyword{misc}
