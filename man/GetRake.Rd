\name{GetRake}
\alias{GetRake}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate Rake angles}
\description{
 Calculates rake angles for fault and auxilliary planes
}
\usage{
GetRake(az1, dip1, az2, dip2, dir)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{az1}{azimuth in degrees of fault plane 1}
  \item{dip1}{dip in degrees of fault plane 1}
  \item{az2}{azimuth in degrees of auxilliary plane 2}
  \item{dip2}{dip in degrees of auxilliary plane 2}
  \item{dir}{polarity }
}
\details{
 uses output of CONVERTSDR or MEC structure
}
\value{
 list of angles for fault plane and auxiallary plane
 
  \item{az1,dip1, rake1, dipaz1 }{strike, dip rake and downdip direction
  for plane 1}
  \item{az2,dip2,rake2,  dipaz2 }{strike, dip rake and downdip direction
  for plane 2}
 
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{GetRakeSense, CONVERTSDR, Beachfoc, justfocXY }
\examples{

GetRake(345.000000, 25.000000, 122.000000, 71.000000, 1)

}

\keyword{misc}
