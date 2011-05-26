\name{nipXY}
\alias{nipXY}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fault-Slip vector plot}
\description{
  Plots a fault plane and the slip vector.  Used for geographic
  representation of numerous focal spheres.
}
\usage{
nipXY(MEC, x = x, y = y, size = c(1, 1), fcol = gray(0.9), nipcol = "black", cex = 0.4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MEC}{MEC structure}
  \item{x}{coordinate on plot }
  \item{y}{ coordinate on plot }
  \item{size}{size in user coordinates}
  \item{fcol}{color for plotting}
  \item{nipcol}{color of slip point}
  \item{cex}{character expansion for slip point}
}
\details{
  Slip vector is the cross product of the poles to the
  fault plane and auxilliary planes.  
}
\value{
  LIST
  \item{Q}{output of qpoint}
  \item{N}{slip vector}

}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{qpoint, CROSSL, lowplane,  TOCART  }
\examples{
MEC = SDRfoc(65,25,13, u=FALSE, ALIM=c(-1,-1, +1, +1), PLOT=FALSE)

plot(c(-1,1), c(-1,1), type='n', axes=FALSE, ann=FALSE, asp=1)

   nipXY( MEC,  0, 0 , fcol = 'red', nipcol='blue',  size = c(.6,.6),
cex=2 )

\dontrun{
##############  application with a map plot
load("pnwmap.RDATA")
amf = "/home/lees/Vignettes/PNW/sel1996_1997.am"
gf1 = getUWfocs(amf)
MEKS =
  list(lon=gf1$lon, lat=gf1$lat, z=gf1$z,
str1=gf1$str1, dip1=gf1$dip1, rake1=gf1$rake1,
dep=gf1$z, name=gf1$name)



  plot(range(XY$x), range(XY$y), type='n', asp=1, ann=FALSE, axes=FALSE)

plotGEOmapXY(pnwmap, LIM=M$LIM ,  PROJ =PROJ, add=TRUE, MAPcol=gray(0.90))

  for(i in 1:length(MEKS$str1))
       {
         Msdr = CONVERTSDR(MEKS$str1[i], MEKS$dip1[i],MEKS$rake1[i])
     MEC = MRake(Msdr$M)
       MEC$UP = FALSE

         jcol =  foc.color(foc.icolor(MEC$rake1), pal=1)

nipXY(MEC, x=XY$x[i], y=XY$y[i], size=c(15,15) , fcol=jcol, nipcol="black", cex=.4)


       }


 usr = par("usr")
kbox = list(x=c(usr[1:2] ), y=c(usr[3:4] ))
 sqrTICXY(kbox , PROJ, side=c(1,2,3,4), LLgrid=FALSE, col=grey(.7) )




}



}

\keyword{hplot}
\keyword{aplot}
