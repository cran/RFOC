\name{Z3Darrow}
\alias{Z3Darrow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Make a 3D arrow}
\description{
  Create the list structure for a 3D arrow.
}
\usage{
Z3Darrow(len = 1, basethick = 0.1, headlen = 0.6, headlip = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{len}{Length in user coordinates}
  \item{basethick}{Thickness of the base }
  \item{headlen}{Length of the head}
  \item{headlip}{Width of the overhang lip }
}
\details{
  Creates a strucutre suitable for plotting rotated and translated 3D arrows.
}
\value{
  List
  \item{aglyph}{List of vertices of the faces}
  \item{anorm}{Outward facing normal vectors to faces}
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\seealso{PROJ3D, pglyph3D, phong3D}
\examples{

ZA = Z3Darrow(len = 1, basethick = 0.1, headlen = 0.6, headlip = 0.1)

}

\keyword{misc}
