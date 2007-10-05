\name{pglyph3D}
\alias{pglyph3D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a 3D body on an existing graphic}
\description{
  rotates a body in 3D and plots projection on existing plot
}
\usage{
pglyph3D(aglyph, M = diag(1, nrow = 4), M2 = diag(1, nrow = 4),
         anorms = list(), zee = c(0, 0, 1), col = "white", border = "black")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{aglyph}{glyph structure describing the vertices  and normal
    vectors of a 3D body }
  \item{M}{rotation matrix 1 }
  \item{M2}{rotation matrix 2 }
  \item{anorms}{up vector}
  \item{zee}{up vector }
  \item{col}{coor of body}
  \item{border}{color of border }
}
\details{
  Hidden sides are removed and phong shading is introduced to create 3D
  effect.

  The input consists of an object defined by
  a list structure,
  list(aglyph, anorm)
  where aglyph is list of 3D polygons (faces) and anorm are outward normals to
  these faces.
  
}
\value{
 Used for side effect on plots
}
\references{Rogers and Adams, 1990, Mathematical Elements for Computer
  Graphics, McGraw-Hill, 611p.}
\author{Jonathan M. Lees <jonathan.lees@unc.edu>}
\note{ For unusual rotations or bizarre bodies, this routine may produce strange looking shapes.
}
\seealso{Z3Darrow, ROTX, ROTY, ROTZ}
\examples{

### create the 3D object
len = .7
basethick=.05
headlip=.02
headlen=.3

####  create a 3D glyph structure
aglyph = Z3Darrow(len = len , basethick =basethick , headlen =headlen ,
headlip=headlip )

#### define the up vector 
myzee = matrix(c(0,0,1, 1), nrow=1, ncol=4)

##### set rotation angles:
gamma =12
beta =39
alpha = 62

########  set up rotation matrix
R3 = ROTZ(gamma)

R2 = ROTY(beta)

R1 = ROTZ(alpha)

###  create rotation matrix
M =      R1  %*% R2  %*%  R3 

M2 =       R1  %*% R2  %*%  R3 


plot(c(-1,1), c(-1,1))

 pglyph3D(aglyph$aglyph, anorms=aglyph$anorm  , M=M, M2=M2, zee=myzee ,
col=rgb(.7, 0,0) )



}
\keyword{aplot}
