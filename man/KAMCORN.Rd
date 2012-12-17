\name{KAMCORN}
\alias{KAMCORN}
\docType{data}
\title{SDR data from the Harvard CMT catalog}
\description{
  Strike-Dip-Rake and Locations of Harvard CMT catalog for the
  intersection of the Kamchataka and Aleutian arcs
}
\usage{data(KAMCORN)}
\format{
  The format is:
 chr "KAMCORN"
}
\details{
  The data is selected fromt eh CMT catalog.  Parameters are
  extracted from the normal distribution.  Format of the list
  of data save in KAMCORN is:
  list(LAT=0  , LON =0  ,   DEPTH=0  ,  STRIKE=0  , DIP=0  ,    RAKE=0  )

}
\source{
 http://www.globalcmt.org/CMTsearch.html

}
\references{
  G. Ekstrom.     Rapid    earthquake   analysis  utilizes the internet.  Computers     in Physics,   8:632-638,  1994.
}
 
\examples{
data(KAMCORN)
plot(KAMCORN$LON, KAMCORN$LAT, xlab="LON", ylab="LAT" ,
          main="Kamchatka-Aleutian Inersection", asp=1)
######  
Paz =vector()
Pdip =vector()
Taz =vector()
Tdip =vector()
h = vector()
v = vector()

IFcol = vector()
Fcol = vector()

for(i in 1:10)
  {
    Msdr = CONVERTSDR(KAMCORN$STRIKE[i],
          KAMCORN$DIP[i], KAMCORN$RAKE[i]   )
  MEC = MRake(Msdr$M)
  MEC$UP = FALSE
  IFcol[i] = foc.icolor(MEC$rake1)
    Fcol[i] = foc.color(IFcol[i], 1)

      az1 = Msdr$M$az1
  dip1 = Msdr$M$d1
  az2 = Msdr$M$az2
  dip2 = Msdr$M$d2
  BBB = Bfocvec(az1, dip1,  az2,  dip2)
  V = ternfoc.point(BBB$Bdip, Msdr$M$pd, Msdr$M$td )
 Paz[i] = Msdr$M$paz
  Pdip[i] = Msdr$M$pd
  Taz[i] = Msdr$M$taz
  Tdip[i] = Msdr$M$td
  h[i] = V$h
  v[i] = V$v

     justfocXY( MEC, fcol = Fcol[i], KAMCORN$LON[i],
          KAMCORN$LAT[i] , size = c(.21,.21) )
  }
}

\keyword{misc}
