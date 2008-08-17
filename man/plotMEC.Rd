\name{plotMEC}
\alias{plotMEC}
\title{Plot a Focal Mechanism}
\description{Plot a Focal Mechanism
}
\usage{
plotMEC(x, detail = 0, up = FALSE)
}
\arguments{
  \item{x}{Mechanism list}
  \item{detail}{level of detail}
  \item{up}{logical, Upper or lower hemisphere}
}

\value{
 Side Effects
}
\examples{

 mc = CONVERTSDR(65, 32, -34 )
plotMEC(mc, detail=2, up=FALSE)

}



\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\keyword{misc}
