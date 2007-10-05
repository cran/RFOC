\name{BOXarrows3D}
\alias{BOXarrows3D}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a 3D Arrow structure}
\description{
Create and project and plot 3D arrows with viewing Matrix.
}
\usage{
BOXarrows3D(x1, y1, z1, x2, y2, z2, aglyph = NULL, Rview = ROTX(0),
col = grey(0.5), border = "black", len = 0.7, basethick = 0.05,
headlen = 0.3, headlip = 0.02)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{x-coordinates of base of arrows }
  \item{y1}{y-coordinates of base of arrows }
  \item{z1}{z-coordinates of base of arrows}
  \item{x2}{x-coordinates of head of arrows}
  \item{y2}{y-coordinates of head of arrows}
  \item{z2}{z-coordinates of head of arrows}
  \item{aglyph}{glyph structure, default is Z3Darrow}
  \item{Rview}{Viewing matrix}
  \item{col}{fill color}
  \item{border}{Border color}
  \item{len}{Length}
  \item{basethick}{thickness of the base}
  \item{headlen}{thickness of the head }
  \item{headlip}{width of the overhanging lip}
}
\details{
  Arrows point from base to head.
}
\value{
  Used for graphical side effects.
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}
\note{ Any 3D glyph strucutre can be used
}
\seealso{Z3Darrow}
\examples{

#### animate 10 random arrow vectors


 L   = list(x1 = runif(10, min=-2, max=2),
    y1 = runif(10, min=-2, max=2),
    z1=runif(10, min=-4, max=4),
    x2 = runif(10, min=-2, max=2),
    y2 = runif(10, min=-2, max=2),
    z2=runif(10, min=-4, max=4)
    )
  headlen = .3
  len = .7
  basethick = 0.05
  headlip = .02
  aglyph = Z3Darrow(len = len , basethick =basethick , headlen =headlen , headlip=headlip )

  r1 = 8
  theta = seq(from=0, to=2*360, length=200)
  mex = r1*cos(theta*pi/180)
  mey = r1*sin(theta*pi/180)
  mez = seq(from=r1, to =0 , length=length(mex))
  ##  mez=rep(r1, length=length(mex))
  
  angz = atan2(mey, mex)*180/pi
  angx = atan2(sqrt(mex^2+mey^2), mez)*180/pi
  pal=c("red", "blue", "green")

##  aglyph = gblock

  
  for(j in 1:length(angz))
    {
      Rview  =    ROTZ(angz[j])%*%  ROTX(-angx[j])
      plot(c(-4,4), c(-4,4), type='n', asp=1); grid()
      
      BOXarrows3D(L$x1,L$y1,L$z1, L$x2,L$y2,L$z2,  aglyph=aglyph,  Rview=Rview, col=pal)
      
      Sys.sleep(.1)
    }
  


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
