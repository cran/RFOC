\name{AXpoint}
\alias{AXpoint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extract Axis pole on Stereonet}
\description{
Interactive extract axis point on Stereonet
}
\usage{
AXpoint(UP = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{UP}{logical, TRUE=upper hemisphere }
}
\details{
  Program uses locator to create a vector of poles.
}
\value{

  \item{phiang}{azimuth angle, degrees}
  \item{dip}{dip angle, degrees}
   \item{x}{x-coordinate of cartesian vector}
   \item{y}{y-coordinate of cartesian vector}
  \item{z}{z-coordinate of cartesian vector}
  \item{gx}{x-coordinate of prjection}
   \item{gy}{y-coordinate of prjection}
 
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{EApoint }
\examples{
####################  this is interactive
\dontrun{
net()
Z = AXpoint(UP = TRUE)
##  click in steronet
Z
}

}

\keyword{iplot}
