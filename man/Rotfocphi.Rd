\name{Rotfocphi}
\alias{Rotfocphi}
\title{Rotate Focal Mechanism
}
\description{Rotate Focal Mechanism into the vertical plane
  by a certain number of degrees
}
\usage{
Rotfocphi(phi, urot, udip, vrot, vdip, az1, d1, az2, d2, prot, pdip, trot, tdip)
}
\arguments{
  \item{phi}{degrees in plane to rotate
}
  \item{urot}{U-vector azimuth
}
  \item{udip}{U-vector dip
}
  \item{vrot}{V-vector azimuth
}
  \item{vdip}{V-vector dip
}
  \item{az1}{First plane - azimuth
}
  \item{d1}{First plane -  dip
}
  \item{az2}{Second plane - azimuth
}
  \item{d2}{Second  plane -  dip
}
  \item{prot}{P-axis azimuth
}
  \item{pdip}{P-axis dip
}
  \item{trot}{T-axis azimuth
}
  \item{tdip}{T-axis dip
}
}
\details{Rotate the focal mech by phi degrees
}
\value{list:
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{xsecmanyfoc, rotateFoc
}

\keyword{misc}
