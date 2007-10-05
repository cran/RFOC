\name{phong3D}
\alias{phong3D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Phong shading for a 3D body}
\description{
  Create phong shading for faces showing on the 3D block
}
\usage{
phong3D(aglyph, M = diag(1, nrow = 4), M2 = diag(1, nrow = 4),
          Light = c(45, 45), anorms = list(), zee = c(0, 0, 1),
         col = "white", border = "black")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{aglyph}{3-D body list of faces and normals}
  \item{M}{Rotation Matrix }
  \item{M2}{Viewing Matrix}
  \item{Light}{ light source direction }
  \item{anorms}{ normals to faces}
  \item{zee}{Up vector for Body}
  \item{col}{color for faces}
  \item{border}{border color for sides}
}
\details{
  Uses a standard phong shading model based ont eh dot product of the
  face normal vector and direction of incoming light.
  
}
\value{
Graphical Side effect
}
\references{Watt, Alan. Fundamentals of Three-dimensional Computer
  Graphics, Addison-Wesley, 1989, 430p.}
\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{makeblock3D, BOXarrows3D, PROJ3D, Z3Darrow, pglyph3D}
\examples{


###########  create a block and rotation matrix, then color it
ANG=(45)
DEGRAD = pi/180

y1 = 1.5

  y2 = y1 - 1/tan((ANG)*DEGRAD)


  z1 = 1
  x1 = 1


Ablock1 = matrix(c(0,0,0,
    1,0,0,
    1,y1,0,
    0,y1,0,
    0,0,-1,
    1,0,-1,
    1,y2,-1,
    0,y2,-1), byrow=TRUE, ncol=3)


Nblock1 = makeblock3D(Ablock1)
Light=c(45,45)
angz = -45
angx = -45

R1 = ROTZ(angz)
R2 = ROTX(angx)

   M =    R1 %*% R2

Z2 = PROJ3D(Nblock1$aglyph, M=M,  anorms=Nblock1$anorm ,  zee=c(0,0,1))
RangesX = range(attr(Z2, "RangesX"))

  RangesY = range(attr(Z2, "RangesY"))


plot( RangesX, RangesY, type='n', asp=1, ann=FALSE, axes=FALSE)

phong3D(Nblock1$aglyph, M=M,  anorms=Nblock1$anorm , Light = Light,
zee=c(0,0,1), col=rgb(.7,.5, .5)  , border="black")



}

\keyword{aplot}
