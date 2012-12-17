\name{ShadowCLVD}
\alias{ShadowCLVD}
\title{Plot CLVD focal mechanism
}
\description{
  Plot non-double couple part of the focal mechanism
  provided in the moment tensor.
}
\usage{
ShadowCLVD(m, PLOT = TRUE, col=rgb(1, .75, .75))
}
\arguments{
  \item{m}{moment tensor
}
  \item{PLOT}{logical, TRUE means plot
  }
  \item{col}{color, either a single color, rgb, or a color palette
  }

  
}
\details{
This code is meant to be used with 
doNonDouble or MapNonDouble functions for plotting
the non-double couple components
of the moment tensor.
A color palette can be provided for some
details of the radiation patterns, e.g.  col=rainbow(12).
  
}
\value{
  Side effects and image list
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{Lower Hemisphere.
}


\seealso{doNonDouble, MapNonDouble
}
\examples{

############  moment tensor from Harvard CMT catalog
sponent = 26
ef = 1*10^(sponent)
Mrr =  2.375*ef
Mtt = -2.777*ef
Mpp = 0.403*ef
Mrt = 2.800*ef
Mrp = 1.190*ef
Mtp = -0.539*ef

############  convert to cartesian coordinates
Mzz=Mrr
Mxx= Mtt
Myy= Mpp
Mxz= Mrt
Myz= -Mrp 
Mxy= -Mtp


m=matrix( c(Mxx,Mxy,Mxz,
      Mxy,Myy,Myz,
       Mxz,Myz,Mzz), ncol=3, byrow=TRUE)

Fi=seq(from=0, by=0.1, to=361)
  ###  dev.new()
    plot(cos(Fi*pi/180.0),sin(Fi*pi/180.0),type='l', asp=1 , ann=FALSE, axes=FALSE)
  
  ShadowCLVD(m, col='red')



}
\keyword{misc}
