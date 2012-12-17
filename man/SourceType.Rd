\name{SourceType}
\alias{SourceType}
\title{Moment Tensor Source Type
}
\description{Given a vector of EigenValues,
  extract the source type.
}
\usage{
SourceType(v)
}
\arguments{
  \item{v}{vector of decreasing eigenvalues}
}
\details{plotting for -30 to 30 degree quadrant.
}
\value{list:
  \item{phi}{latitude angle in degrees}
  \item{lam}{longitude angle in degrees}
}
\references{Tape, W.,and C.Tape(2012), A geometric comparison of source-type plots for moment tensors, Geophys. J. Int., 190, 499-510.}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}

\seealso{ HAMMERprojXY, TapeBase, TapePlot}
\examples{
SourceType(c(1,-1,1) )

T1 = TapeBase()

m1 = list(Mxx=1.543,  Mxy=0.786,  Myy=0.336, Mxz=-2.441,  Myz=0.353,  Mzz=0.961)

i = 1
M1=matrix( c(m1$Mxx[i],m1$Mxy[i],m1$Mxz[i],
      m1$Mxy[i],m1$Myy[i],m1$Myz[i],
       m1$Mxz[i],m1$Myz[i],m1$Mzz[i]), ncol=3, byrow=TRUE)

 E1 = eigen(M1)
           h = SourceType( sort(E1$values, decreasing=TRUE) )
           h$dip = 90-h$phi
           ##  cat(paste(h$dip, h$lam, sep=" "), sep="\n")
           h1 = HAMMERprojXY(h$dip*pi/180, h$lam*pi/180)



TapePlot(T1)
           points(h1$x, h1$y,  pch=21, bg="red" )





}
\keyword{misc}
