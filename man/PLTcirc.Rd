\name{PLTcirc}
\alias{PLTcirc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Circle Plot with Cross Hairs}
\description{
 Plot an arc of a circle  with cross-hairs.  
}
\usage{
PLTcirc(gcol = "black", border = "black", ndiv = 36,
         angs = c(-pi, pi), PLOT = TRUE, add = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gcol}{ cross hairs color   }
  \item{border}{border color }
  \item{ndiv}{number of divisions }
  \item{angs}{vector from angs[1] to angs[2] in radians }
  \item{PLOT}{ logical, if TRUE plot }
  \item{add}{  logical, if TRUE add to existing plot }
}

\value{
 list used for plotting:
  \item{x }{x coordinates}
  \item{y }{y coordinates}
  \item{phi }{angles, radians }
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}


\examples{

PLTcirc(gcol = "purple", border = "black", ndiv = 36, angs = c(-pi, pi), PLOT = TRUE, add = FALSE)

PLTcirc(gcol = NULL, border = "green" , ndiv = 36, angs = c(-pi/4, pi/4), PLOT = TRUE, add = TRUE)



}
\keyword{aplot}
