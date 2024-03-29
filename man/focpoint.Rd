\name{focpoint}
\alias{focpoint}

\title{add point on focal sphere}
\description{
Add points on equal-area focal plot
}
\usage{
focpoint(az1, dip1, col = 2, pch = 5, lab = "", cex=1,  UP = FALSE, PLOT = TRUE, ...)
}

\arguments{
  \item{az1}{degrees, azimuth angle }
  \item{dip1}{degrees, dip angle }
  \item{col}{color  }
  \item{pch}{plot character for point }
  \item{lab}{text lable for point }
  \item{cex}{Character Size }
  \item{UP}{upper or lower hemisphere  }
  \item{PLOT}{logical, PLOT=TRUE add points to current plot }
  \item{\dots}{graphical parameters}
}

\value{
  List of x,y coordinates on the plot
 
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{Beachfoc, addmecpoints}
\examples{

###  create focal mech
ALIM=c(-1,-1, +1, +1)
s=65
d=25
r=13
mc = CONVERTSDR(s,d,r )
  MEC = MRake(mc$M)
  MEC$UP = FALSE
  MEC$icol =  foc.icolor(MEC$rake1)
  MEC$ileg =  focleg(MEC$icol)
  MEC$fcol =   foc.color(MEC$icol)
  MEC$CNVRG = NA
  MEC$LIM = ALIM

###  plot focal mech
Beachfoc(MEC, fcol=MEC$fcol, fcolback="white")

###  now add the F anf G axes
focpoint(MEC$F$az, MEC$F$dip, pch=5, lab="F", UP=MEC$UP)
    focpoint(MEC$G$az, MEC$G$dip, pch=5, lab="G", UP=MEC$UP)


}

\keyword{aplot}
