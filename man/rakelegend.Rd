\name{rakelegend}
\alias{rakelegend}
\title{Focal Legend based on rake
}
\description{Focal Legend based on rake
}
\usage{
rakelegend(corn="topright", pal=1)
}
\arguments{
  \item{corn}{position of legend, default="topright"
}
  \item{pal}{palette number, default=1
}
}
\details{
  Colors are based on earlier publication
  of Geotouch program.

  For pal = 1,   colors are ,
  DarkSeaGreen, cyan1, SkyBlue1, RoyalBlue,
  GreenYellow, orange, red.


  

}
\value{Graphical Side Effects
}
\references{
  Lees, J. M., (1999) Geotouch: Software for Three
  and Four-Dimensional GIS in the Earth Sciences,
  Computers and Geosciences, 26(7) 751-761. 
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{foc.color,focleg 
}
\examples{

plot(c(0,1), c(0,1), type='n')

rakelegend(corn="topleft", pal=1)

}
\keyword{misc}
