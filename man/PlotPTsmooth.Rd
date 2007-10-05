\name{PlotPTsmooth}
\alias{PlotPTsmooth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot Smooth PT-axes}
\description{
Project PT axes on the sphere and smooth the image.  This function
requires function kde2d, from the MASS library.  
}
\usage{
PlotPTsmooth(paz, pdip, x = 0, y = 0, siz = 1, bcol = "white", border ="black",
        IMAGE = TRUE, CONT = TRUE, cont.col = "black",
         pal = terrain.colors(100), LABS = FALSE, add = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{paz}{vector of Axis azimuths, degrees}
  \item{pdip}{vector of dip angles, degrees}
  \item{x}{x-location of plot center in user coordinates}
  \item{y}{y-location of plot center in user coordinates}
  \item{siz}{siz of plot in user coordinates}
  \item{bcol}{color}
  \item{border}{border color}
  \item{IMAGE}{logical, TRUE=create an image plot}
  \item{CONT}{logical, TRUE=add contour lines}
  \item{cont.col}{color of contour lines}
  \item{pal}{pallete for image plot}
  \item{LABS}{text Label for image}
  \item{add}{logical, TRUE=add to plot}
}
\details{
  Program requires MASS libary for 2D smoothing routine kde2d.
  
}
\value{
 Graphical Side Effect
}

\author{Jonathan M. Lees<jonathan.lees@unc.edu>}
\note{
  Points that fall on the
opposite hemisphere are reflected through the origin.
}
\seealso{kde2d}
\examples{
library(MASS)

plot(c(-1,1), c(-1,1), asp=1, type='n')

paz = rnorm(100, mean=297, sd=10)
 pdip = rnorm(100, mean=52, sd=8)

PlotPTsmooth(paz, pdip, x=0.5, y=.5, siz=.3, border=NA, bcol='white' ,
LABS=FALSE, add=FALSE, IMAGE=TRUE, CONT=FALSE)

taz = rnorm(100, mean=138, sd=10)
 tdip = rnorm(100, mean=12, sd=8)

 PlotPTsmooth(taz, tdip, x=-.5, y=.4, siz=.3, border=NA, bcol='white' ,
LABS=FALSE, add=FALSE, IMAGE=TRUE, CONT=TRUE)

###########  put them together
plot(c(-1,1), c(-1,1), asp=1, type='n')
PlotPTsmooth(paz, pdip, x=0, y=, siz=1, border=NA, bcol='white' ,
LABS=FALSE, add=FALSE, IMAGE=TRUE, CONT=FALSE)
PlotPTsmooth(taz, tdip, x=0, y=, siz=1, border=NA, bcol='white' ,
LABS=FALSE, add=TRUE, IMAGE=FALSE, CONT=TRUE)




}

\keyword{hplot}
\keyword{aplot}
