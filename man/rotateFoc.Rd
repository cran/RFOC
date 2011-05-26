\name{rotateFoc}
\alias{rotateFoc}
\title{Rotate Focal Mechanism 
}
\description{Rotate mechanism to vertical plan at specified angle
}
\usage{
rotateFoc(MEX, phi)
}
\arguments{
  \item{MEX}{Focal Mechanism list
}
  \item{phi}{angle in degrees
}
}
\details{Assumed vertical plane, outer hemisphere
}
\value{Focal Mechanism
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{plotfoc, SDRfoc,Beachfoc,  TEACHFOC, plotmanyfoc, getUWfocs
}
\examples{


a1 = SDRfoc(90, 90, 90, u = TRUE , PLOT = TRUE)


par(mfrow=c(2,2))

SDRfoc(a1$az1, a1$dip1, a1$rake1, u = TRUE, PLOT = TRUE)
ra1 = rotateFoc(a1, -90)

SDRfoc(ra1$az1, ra1$dip1, ra1$rake1, u = TRUE , PLOT = TRUE)

ra1 = rotateFoc(a1, 0)


SDRfoc(a1$az1, a1$dip1, a1$rake1, u = TRUE, PLOT = TRUE)

SDRfoc(ra1$az1, ra1$dip1, ra1$rake1, u = TRUE , PLOT = TRUE)





}
\keyword{misc}
