\name{prepFOCS}
\Rdversion{1.1}
\alias{prepFOCS}
\title{Prepare Focals
}
\description{Prepare Focals for plotting.  Program
  cycles through data and prepares a
  relevant data for further plotting and
  analysis.
}
\usage{
prepFOCS(CMTSOL)
}
\arguments{
  \item{CMTSOL}{
    see getCMT for the format for the input here.
    
}
}
\details{Used internally in spherefocgeo and ternfocgeo.
}
\value{
List:
\item{Paz}{P-axis azimuth}
\item{Pdip}{P-axis dip}
\item{Taz}{T-axis azimuth}
\item{Tdip}{T-axis dip}
\item{h}{horizontal distance on ternary plot}
\item{v}{vertical distance on ternary plot}
\item{fcols}{focal color}
\item{LATS}{latitudes}
\item{LONS}{longitudes}
\item{IFcol}{index of color}
\item{yr}{year}
\item{JDHM}{character  identification}
\item{JDHMS}{character identification}

  
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{getCMT, spherefocgeo, ternfocgeo
}

\keyword{misc}
