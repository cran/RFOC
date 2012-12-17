\name{readCMT}
\alias{readCMT}
\title{Read Harvard  CMT moment
}
\description{Read and plot a CMT solution copied from the
  Harvard CMT website.
}
\usage{
readCMT(filename, PLOT=TRUE)
}
\arguments{
  \item{filename}{character, file name
  }
 \item{PLOT}{Logical, TRUE=plot mechanisms sequentially
  }


  
}
\details{Uses the standard output format.
}
\value{List of mechanisms and graphical Side effects.
  Each element in the list consists of a list
  including:
  FIRST,yr,mo,dom,hr,mi,sec,name,tshift,half,lat,lon,z,Mrr,Mtt,Mpp,Mrt,Mrp,Mtp.
  The FIRST element is simply a duplicate of the PDE solution card.
}
\references{
  Ekstrom, G.; Nettles, M. and  DziewoDski, A.
  The Global CMT Project 2004-2010: centroid-moment tensors for 13,017
  earthquakes Physics of the Earth and Planetary Interiors, 2012.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{Other formats are available.
}


\seealso{doNonDouble, MapNonDouble
}
\examples{

\dontrun{
Hcmt = readCMT("CMT_FULL_FORMAT.txt")

########  or,

Hcmt = readCMT("CMT_FULL_FORMAT.txt", PLOT=FALSE)

 moments = matrix(ncol=7, nrow=length(Hcmt))
Locs = list(y=vector(length=length(Hcmt)) ,x=vector(length=length(Hcmt)))


for(i in 1:length(Hcmt))
{
P1 = Hcmt[[i]]
#########  Note the change of sign for cartesian coordinates
 moments[i,] = cbind(i, P1$Mtt, P1$Mpp, P1$Mrr,
        -P1$Mrp, P1$Mrt ,-P1$Mtp)
Locs$y[i] = P1$lat
Locs$x[i] = P1$lon

}


mlon = mean(Locs$x, na.rm=TRUE)
mlat = mean(Locs$y, na.rm=TRUE)


PROJ =  setPROJ(type = 1, LAT0 = mlat , LON0 = mlon)
Glocs = GLOB.XY(Locs$y, Locs$x, PROJ       )


LIMlat = expandbound(Locs$y)
LIMlon = expandbound(Locs$x)

PLAT =  pretty(LIMlat)
 PLON  = pretty(LIMlon)

data(worldmap)
par(xpd=FALSE)

plotGEOmapXY(worldmap, LIM = c(LIMlon[1],LIMlat[1] ,LIMlon[2],LIMlat[2]) ,
             PROJ=PROJ, axes=FALSE, xlab="", ylab="" )

### add tic marks
kbox = GLOB.XY(PLAT,PLON, PROJ)

      sqrTICXY(kbox , PROJ, side=c(1,2,3,4), LLgrid=TRUE, col=grey(.7) )

########  add focal mechs

MapNonDouble(Glocs, moments, col=NULL, add=TRUE)



}

}
\keyword{misc}
