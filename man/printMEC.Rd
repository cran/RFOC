\name{printMEC}
\alias{printMEC}
\title{Print focal mechanism}
\description{Print focal mechanism
}
\usage{printMEC(x, digits = max(3, getOption("digits") - 3), ...)
}
\arguments{
  \item{x}{Mechanism list}
  \item{digits}{digits for numeric information}
  \item{\dots}{standard printing parameters}
}

\value{
 Side Effects
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\examples{
 mc = CONVERTSDR(65, 32, -34 )

printMEC(mc)

}



\keyword{misc}
