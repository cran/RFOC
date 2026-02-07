\name{AXpoint}
\alias{AXpoint}
\title{Extract Axis pole on Stereonet}
\description{
Interactive extract axis point on Stereonet
}
\usage{
AXpoint(UP = TRUE, col=2, n=1)
}

\arguments{
  \item{UP}{logical, TRUE=upper hemisphere }
  \item{col}{plotting color }
  \item{n}{maximum number to locate, default=unlimited }
  
}
\details{
  Program uses locator to create a vector of poles.  Points outside the
  focal sphere (r>1) are ignored.  If n is missing, locator continues
  until stopped (middle mouse in linux, stop in windows).
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

\seealso{locator, qpoint, EApoint}
\examples{
####################  this is interactive
if(interactive()){ 
net()
Z = AXpoint(UP = TRUE)


##  click in steronet
Z
}

}

\keyword{iplot}
