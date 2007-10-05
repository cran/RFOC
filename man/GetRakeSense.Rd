\name{GetRakeSense}
\alias{GetRakeSense}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get Rake Sense }
\description{
 Get the sense of the focal mechanism rake, from the U, V, P, T vectors
}
\usage{
GetRakeSense(uaz, upl, vaz, vpl, paz, ppl, taz, tpl)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{uaz}{Azimuth of U vector}
  \item{upl}{dip of U vector}
  \item{vaz}{Azimuth of V vector }
  \item{vpl}{dip of V vector}
  \item{paz}{Azimuth of P vector}
  \item{ppl}{dip of P vector }
  \item{taz}{Azimuth of T vector}
  \item{tpl}{dip of T vector}
}

\value{
  1, 0 to make sure the region of the T-axis is shaded and the P-axis
  is blank.
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}
\note{
  The convention is for the T-axis to be shaded,
  so this subroutine determines the order of the polygons to be plotted
  so that the appropriate regins are filled.
}
\seealso{GetRake}
\examples{


mc =CONVERTSDR(65,25,13)


angsense = GetRakeSense(mc$U$az, mc$U$dip, mc$V$az, mc$V$dip,mc$P$az, mc$P$dip,mc$T$az, mc$T$dip)


}

\keyword{misc}
