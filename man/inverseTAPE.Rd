\name{inverseTAPE}
\alias{inverseTAPE}
\title{Inverse Moment Tensor
}
\description{Inverse moment tensor from
  Tape angles.
}
\usage{
inverseTAPE(GAMMA, BETA)
}
\arguments{
  \item{GAMMA}{Longitude, degrees
}
  \item{BETA}{CoLatitude, degrees 
}
}
\details{Uses Tape and Tape lune angles
  to estimate the moment tensor.  This function
  is the inverse of the SourceType calculation.
  There are two solutions to the
  systems of equations.

  Vectors are scaled by the maximum value.

 
}
\value{Moment tensor list:
  \item{Va}{vector, First solution}
   \item{Vb}{vector, First solution}
}
\references{Tape, W.,and C.Tape(2012), A geometric comparison of source-type plots for moment tensors, Geophys. J. Int., 190, 499-510.
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{The latitude is the CoLatitude.
  
  Either vector can be used as a solution.

   Orientation of moment tensor is not preserved int he lune plots.

   
}


\seealso{SourceType
}
\examples{

lats = seq(from = -80, to = 80, by=10)
    lons = seq(from=-30, to=30, by=10)

i = 3
j = 3
 u =  inverseTAPE( lons[i], 90-lats[j] ) 



}
\keyword{misc}
