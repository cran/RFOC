\name{PROJ3D}
\alias{PROJ3D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Project 3D}
\description{
 Project a 3D body after rotation and translation
}
\usage{
PROJ3D(aglyph, M = diag(1, nrow = 4), M2 = diag(1, nrow = 4),
             anorms = list(), zee = c(0, 0, 1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{aglyph}{glyph structure}
  \item{M}{rotation matrix}
  \item{M2}{rotation matrix}
  \item{anorms}{normals to structure}
  \item{zee}{Up direction of body}
}
\details{
  This function takes a 3D body, rotates it and
  projects it for plotting. An example glyph is found in
  Z3Darrow.
}
\value{
Glyph structure
  \item{x,y,z}{coordinates of rotated body faces}
  \item{xp}{rotated normal vectors}
  \item{zd}{depth mean value of each face}
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{makeblock3D, ROTZ, ROTY, ROTX, BOXarrows3D, Z3Darrow, TRANmat}
\examples{
  block1 = matrix(c(0,0,0,
      1,0,0,
      1,0.5,0,
      0,0.5,0,
      0,0,-2,
      1,0,-2,
      1,0.5,-2,
      0,0.5,-2), byrow=TRUE, ncol=3)

    Bblock1 = makeblock3D(block1)

    R3 = ROTX(-40)
    R2 = ROTY(0)
    R1 = ROTZ(20)
    T =  TRANmat(.1, 0, 0 )
    M =     R1  \%*\% R2  \%*\%  R3  \%*\% T

    T2 =  TRANmat(1, 0.5, 0 )
    MT =       T2 \%*\%   R1  \%*\% R2  \%*\%   R3 \%*\% T

    Z1 =  PROJ3D(Bblock1$aglyph, M=MT,  anorms=Bblock1$anorm , zee=c(0,0,1))


}

\keyword{misc}
