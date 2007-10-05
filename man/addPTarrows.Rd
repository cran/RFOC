\name{addPTarrows}
\alias{addPTarrows}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add fancy 3D arrows}
\description{
  Illustrate Pressure and Tension axis on Focal Plot using
  3D arrows
}
\usage{
addPTarrows(MEC)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MEC}{Mechanism Structure}
}

\value{
 Graphical Side Effects
}
\note{This function looks better when plotting the upper hemisphere}


\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{focpoint, BOXarrows3D,Z3Darrow}
\examples{


 MEC = SDRfoc(65,25,13, u=TRUE, ALIM=c(-1,-1, +1, +1), PLOT=TRUE)
  
addPTarrows(MEC)


}

\keyword{aplot}
