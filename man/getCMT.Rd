\name{getCMT}
\Rdversion{1.1}
\alias{getCMT}
\title{Read CMT 
}
\description{Read and reformat CMT solutions downloaded from the web.
}
\usage{
getCMT(fn)
}
\arguments{
  \item{fn}{character file name
}
}
\details{

  Data can be extracted from web site:
  http://www.globalcmt.org/CMTsearch.html

  
  The file must be cleaned prior to scanning -
  on download from the web site there are extra
  lines on top and bottom of file.
  Delete these.
  Leave one line on the top that describesthe columns.
  Data is separated by blanks.
  The files have a mixture of
  dates - some with 7 component dates (YYMMDD and others with 14
  components YYYYMODDHHMM these are read in separately.
  Missing hours and minutes areset to zero.
  
}
\value{
  list of CMT solution data:
\item{lon}{lon of epicenter}
\item{lat}{lat of epicenter}
\item{str1}{strike of fault plane}
\item{dip1}{dip of fault plane}
\item{rake1}{rake  of fault plane}
\item{str2}{strike of auxilliary plane}
\item{dip2}{dip  of auxilliary plane}
\item{rake2}{rake of auxilliary plane }
\item{sc}{scale?}
\item{iexp}{exponent?}
\item{name}{name, includes the date}
\item{Elat}{exploding latitude, set to lat initially}
\item{Elon}{exploding longitude, set to lon initially}
\item{jd}{julian day}
\item{yr}{year}
\item{mo}{month}
\item{dom}{day of month}
  
}
\references{
  http://www.globalcmt.org/CMTsearch.html

    G. Ekstrom. Rapid earthquake analysis utilizes the internet. 
     Computers in Physics,   8:632-638,  1994.


}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{
  Use ExplodeSymbols or explode to get new locations
  for expanding the plotting points.
}


\seealso{ExplodeSymbols, spherefocgeo, ternfocgeo
}

\keyword{misc}