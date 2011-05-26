\name{mijsdr}
\alias{mijsdr}
\title{Moment Tensor to Strike-Dip-Rake
}
\description{Convert a normalized
  moment tensor from the CMT catalog
  to Strike-Dip-Rake.
}
\usage{
mijsdr(mxx, myy, mzz, mxy, mxz, myz)
}
\arguments{
  \item{mxx}{moment tensor 1,1
}
  \item{myy}{moment tensor 2,2
}
  \item{mzz}{moment tensor 3,3
}
  \item{mxy}{moment tensor 1,2
}
  \item{mxz}{moment tensor 1,3
}
  \item{myz}{moment tensor 2,3
}
}
\details{
  the coordinate system is
  modified to represent a system centered on the
  source.
  
}
\value{Focal Mechanism list
}
\references{http://www.globalcmt.org/CMTsearch.html
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}
\note{This code will convert the output of
  the website, 
  http://www.globalcmt.org/CMTsearch.html
  when dumped in the
  psmeca (GMT v>3.3) format.
}


\seealso{getCMT
}
\examples{

mijsdr(-1.96, 1.07, 0.89, 0.51, 0.08, -0.68)


}
\keyword{misc}
