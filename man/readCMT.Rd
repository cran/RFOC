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

Z = c('MLI 1981  1 30  8 52 44.10  51.7400  176.2700  33.0 6.3 7.0 RAT ISLANDS, ALEUTIANS',
'event name:     013081B',         
'time shift:      6.0000', 
'half duration:   7.0000', 
'latitude:       51.5200', 
'longitude:     176.1000', 
'vdepth:          20.0000', 
'Mrr:       2.375000e+26', 
'Mtt:      -2.777000e+26', 
'Mpp:       4.030000e+25',
'Mrt:        2.8e+26',
'Mrp:       1.19e+26',
'Mtp:      -5.39e+25'
)

fn =  tempfile("temp", fileext = ".cmt")
cat(file=fn, Z, sep="\n")

Hcmt = readCMT(fn, PLOT=FALSE)

}
\keyword{misc}
