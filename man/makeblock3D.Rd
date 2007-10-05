\name{makeblock3D}
\alias{makeblock3D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Make a 3D block Structure}
\description{
  Given vertices of a 3D block,
  create a glyph structure (faces and normals)
}
\usage{
makeblock3D(block1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{block1}{matrix of vertices}
}

\value{
glyph structure list
  \item{aglyph }{list of faces (x,y,z)}
  \item{anorm }{Normals to faces}

}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}

\seealso{ROTZ, ROTY, ROTX, BOXarrows3D, Z3Darrow, TRANmat}
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

}

\keyword{misc}
