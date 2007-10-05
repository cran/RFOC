\name{CONVERTSDR}
\alias{CONVERTSDR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert Strike-Dip-Rake to MEC structure}
\description{
 Takes Strike-Dip-Rake and creates planes and pole locations for MEC structure
}
\usage{
CONVERTSDR(strike, dip, rake)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{strike}{angle, degrees,  strike of  down dip directin}
  \item{dip}{angle, degrees, dip is measured from the horizontal NOT from the  NADIR}
  \item{rake}{angle, degrees}
}
\details{
  input is strike dip and rake in degrees
 
}
\value{
  LIST:
  
  \item{strike}{strike}
  \item{dipdir}{dip}
  \item{rake}{rake}
  \item{F}{list(az, dip) of F-pole}
  \item{G}{list(az, dip) of G-pole}
  \item{U}{list(az, dip) of U-pole}
  \item{V}{list(az, dip) of V-pole}
  \item{P}{list(az, dip) of P-pole}
  \item{T}{list(az, dip) of T-pole}
  \item{M}{list( az1=0, d1=0,  az2=0, d2=0, uaz=0, ud=0, vaz=0, vd=0, paz=0, pd =0, taz=0, td=0)}

}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{BeachFoc}
\examples{

s=65
d=25
r=13

  mc = CONVERTSDR(s,d,r )
}
\keyword{list}
