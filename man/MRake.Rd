\name{MRake}
\alias{MRake}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Rake Calculation}
\description{
  Calculate various parameters associated with the
  Rake or Slip of an earthquake
}
\usage{
MRake(M)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{list(uaz, ud, vaz, vd, paz, pd, taz, td) }
}
\details{
  This routine takes the four poles U, V, P, T,
  and returns a MEC structure.
  (uaz, ud ) = U pole azimuth and dip
  ( vaz, vd)= V pole azimuth and dip
   (paz, pd)= P pole azimuth and dip
   (taz, td)= T pole azimuth and dip
}
\value{
returns a MEC structure
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}
\seealso{CONVERTSDR, GetRakeSense,  GetRake }
\examples{
 mc = CONVERTSDR(329, 8, 110 )
    MEC = MRake(mc$M)
}

\keyword{misc}
