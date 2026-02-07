\name{getUWfocs}
\alias{getUWfocs}
\title{Get UW focals
}
\description{Get UW focal mechansims from a file.
  These are often called A and M cards
}
\usage{
getUWfocs(amfile)
}
\arguments{
  \item{amfile}{character, file name
}
}
\details{
  UW focal mechanisms are stored as A and M cards.
  The A card described the hypocenter the M card describes the
  focal mechanism.
}
\value{
List:
\item{lon}{numeric, longitude}
\item{lat}{numeric, latitude}
\item{str1}{numeric, strike of plane 1}
\item{dip1}{numeric, dip of plane 1}
\item{rake1}{numeric, rake of plane 1}
\item{str2}{numeric, strike of plane 2 }
\item{dip2}{numeric, dip of plane 2 }
\item{rake2}{numeric, rake of plane 2}
\item{sc}{character, some GMT info for scale}
\item{iexp}{character, some GMT info for scale}
\item{name}{character, name}
\item{yr}{numeric, year}
\item{mo}{numeric, month}
\item{dom}{numeric, day of month}
\item{jd}{numeric, julian day}
\item{hr}{numeric, hour}
\item{mi}{numeric, minute}
\item{se}{numeric, second}
\item{z}{numeric, depth}
\item{mag}{numeric, magnitude}
}
\references{http://www.unc.edu/~leesj/XM_DOC/xm_hypo.doc.html
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{Uses UW2 format, so full 4 digit year is required
}
\examples{

#####  uwpickfile is an ascii format file from University of Washington

data(GH, package='RSEIS')

pf = GH$pickfile$PF

### save only the A and M cards to temp file:
fn =  tempfile("temp", fileext = ".UW")
cat(file=fn, c(pf[1], pf[34]) , sep='\n')

G1 = getUWfocs(fn)

plot(G1$lon, G1$lat)

 MEKS = list(lon=G1$lon, lat=G1$lat, str1=G1$str1,
dip1=G1$dip1, rake1=G1$rake1, dep=G1$z, name=G1$name)

##   utm projection
 PROJ = GEOmap::setPROJ(type=2, LAT0=mean(G1$lat) , LON0=mean(G1$lon) )   

     XY = GEOmap::GLOB.XY(G1$lat, G1$lon, PROJ)

     plot(range(XY$x), range(XY$y), type='n', asp=1)

     plotmanyfoc(MEKS, PROJ, focsiz=0.5)



}




\seealso{getCMT
}

\keyword{misc}
