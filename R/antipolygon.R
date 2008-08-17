`antipolygon` <-
  function(x,y,col=0, corner=1)
{
  if(missing(corner)) { corner=1 }
  ##  use the polygon in x,y to blank out (mask) the image on the screen
  ## useful for plotting contour plots and images
  ##  antipolygon(POL$x, POL$y, col=rgb(1,1,1) )
  ##  see contPfile  for an example of usage:
###   corners: 1 = LowerLeft(default) ; 2:UpperLeft 3 = UpperRight; 4=LowerRight
  
  u <- par("usr")

  dxu = 0.2*(u[2]-u[1])
  dyu = 0.2*(u[4]-u[3])

  if(corner==1)
    {
      x <- c(x,x[1],u[1]-dxu,u[1]-dxu,u[2]+dxu,u[2]+dxu,u[1]-dxu)
      
      y <- c(y,y[1],u[3]-dyu,u[4]+dyu,u[4]+dyu,u[3]-dyu,u[3]-dyu)

    }
  if(corner==2)
    {

      
      x <- c(x,x[1],u[1]-dxu,u[2]+dxu,u[2]+dxu,u[1]-dxu, u[1]-dxu)
      
      y <- c(y,y[1],u[4]+dyu,u[4]+dyu,u[3]-dyu,u[3]-dyu,u[3]-dyu)

    }
  if(corner==3)
    {   
      x <- c(x,x[1],u[2]+dxu,u[2]+dxu,u[1]-dxu, u[1]-dxu,u[1]-dxu)
      y <- c(y,y[1],u[4]+dyu,u[3]-dyu,u[3]-dyu,u[3]-dyu,u[4]+dyu)

    }
  if(corner==4)
    {
      x <- c(x,x[1],u[2]+dxu,u[1]-dxu, u[1]-dxu,u[1]-dxu,u[2]+dxu)
      y <- c(y,y[1],u[3]-dyu,u[3]-dyu,u[3]-dyu,u[4]+dyu,u[4]+dyu)

    }

  
  polygon(x,y,border=col,col=col)
  ## box()
}

