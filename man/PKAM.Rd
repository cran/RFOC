\name{PKAM}
\alias{PKAM}
\docType{data}
\title{P and T-axes data from the Harvard CMT catalog}
\description{
 P and T-axes  and Locations of Harvard CMT catalog for the
  intersection of the Kamchataka and Aleutian arcs
}
\usage{data(PKAM)}
\format{
  The format is:
 chr "PKAM"
}
\details{
  The data is selected from the CMT catalog.  Parameters are
  extracted from the standard web distribution.  Format of the list
  of data save in PKAM is:
  \itemize{
  \item Paz P-axis azimuth angle 
  \item Pdip P-axis dip angle 
  \item Taz  T-axis  azimuth angle 
  \item Tdip  T-axis dip angle 
  \item h  horizontal point to plot on ternary plot  
  \item v  vertical point to plot on ternary plot  
  \item fcols  colors, not used 
  \item LATS  Latitude 
  \item LONS  Longitude 
  \item IFcol  integer pointer to internal color 
  \item yr  year, not used 
  \item JDHM  Julian Day, hour, minute, not used 
  \item JDHMS  Julian Day, hour, minute, seconds 
}


}
\source{
 http://www.globalcmt.org/CMTsearch.html

}
\references{
  G. Ekstrom. Rapid earthquake analysis utilizes the internet.  Computers in Physics,   8:632-638,  1994.
}
 
\examples{
data(PKAM)
## 

######  plot the locations:
plot( RPMG::fmod(PKAM$LONS, 360), PKAM$LATS)
######  

  PlotTernfoc(PKAM$h,PKAM$v,x=0, y=0, siz=1, fcols='black', add=FALSE,
LAB=TRUE)

######  change the colors for the plot

acols = rainbow(7)
fcols = acols[PKAM$IFcol]

######  


 PlotTernfoc(PKAM$h,PKAM$v,x=0, y=0, siz=1, fcols=fcols, add=FALSE,
LAB=TRUE)





}
\keyword{datasets}
