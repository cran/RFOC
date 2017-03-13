\name{TapePlot}
\alias{TapePlot}
\title{Tape style Lune Plot
}
\description{Tape style Lune Plot using Hammer projection
}
\usage{
TapePlot(TapeList = list(), add = FALSE, ann = TRUE,
pcol = c(grey(0), grey(0.85), grey(0.95)))
}
\arguments{
  \item{TapeList}{List of strokes from TapeBase
}
  \item{add}{logical, TRUE=add to existing plot
}
  \item{ann}{logical, TRUE=annotape
}
  \item{pcol}{3-vector of colors: inner lines, upper polygon, lower polygon
}
}
\details{Plot an Tape net from the TapeBase function.  
}
\value{Side effects
}
\references{Tape, W., and C. Tape (2012), A geometric comparison of source-type plots for moment tensors, Geophys. J. Int., 190, 499-510. https://doi.org/10.1111/j.1365-246X.2012.05490.x
}
\author{
Jonathan M. Lees<jonathan.lees@unc.edu>
}


\seealso{TapeBase, HAMMERprojXY
}
\examples{

T1 = TapeBase()
TapePlot(T1)

 data(widdenMoments)
WM = widdenMoments
        
         par(mfrow=c(1,1), mai=c(0,0,0,0))
         T1 = TapeBase()
         TapePlot(T1)

         for(i in 1:length(WM$Mxx))
         {
           M1=matrix( c(WM$Mxx[i],WM$Mxy[i],WM$Mxz[i],
      WM$Mxy[i],WM$Myy[i],WM$Myz[i],
       WM$Mxz[i],WM$Myz[i],WM$Mzz[i]), ncol=3, byrow=TRUE)

           E1 = eigen(M1)
           h = SourceType( sort(E1$values, decreasing=TRUE) )
           h$dip = 90-h$phi
           ##  cat(paste(h$dip, h$lam, sep=" "), sep="\n")
           h1 = HAMMERprojXY(h$dip*pi/180, h$lam*pi/180)
           
           points(h1$x, h1$y,  pch=21, bg="orange" )

         }





}
\keyword{misc}
