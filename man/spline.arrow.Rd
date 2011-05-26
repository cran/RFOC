\name{spline.arrow}
\alias{spline.arrow}
\title{Spline Arrow
}
\description{Given a set of points, draw a spline and
  affix an arrow at the end.
}
\usage{
spline.arrow(x, y = 0, kdiv = 20, arrow = 1, length = 0.2, col = "black", thick = 0.01, headlength = 0.2, headthick = 0.1, code = 2, ...)
}
\arguments{
  \item{x}{vector, x-coordinates
}
  \item{y}{vector, y-coordinates
}
  \item{kdiv}{Number of divisions
}
  \item{arrow}{style of arrow, 1=simple arrow, 2=fancy arrow
}
  \item{length}{length of head
}
  \item{col}{color of arrow
}
  \item{thick}{thickness of arrow stem
}
  \item{headlength}{length of arrow head
}
  \item{headthick}{thickness of arrow head
}
  \item{code}{code, 1=arrow on end of spline, 3=arrow on beginning.
}
  \item{\dots}{graphical parameters for the line
}
}
\details{Can use either simple arrows
  or fancy arrows.
}
\value{list of x,y coordinates
  of the spline and Graphical Side effects
}

\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{fancyarrows
}
\examples{

plot(c(0,1), c(0,1), type='n')


G=list()
G$x=c(0.1644,0.1227,0.0659,0.0893,0.2346,
0.3514,0.5518,0.7104,0.6887,0.6903,0.8422)
G$y=c(0.8816,0.8305,0.7209,0.6086,0.5372,
0.6061,0.6545,0.6367,0.4352,0.3025,0.0475)



spline.arrow(G$x, G$y)



}
\keyword{misc}
