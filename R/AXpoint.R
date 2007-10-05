`AXpoint` <-
function(UP=TRUE)
{
                   #   to plot points on an equal area stero net:
  if(missing(UP)) { UP = TRUE}
  deg2rad = pi/180
  rad2deg = 180/pi
  p = locator(n=1, type='p', col=2)
  r = sqrt(p$x^2+p$y^2)
  if(r>1) return(list(az=0, dip=0))
  iang = rad2deg*2*asin(r/sqrt(2))
  phiang = rad2deg*( pi/2 - atan2(p$y,p$x))
  if(UP==TRUE)
    {
      iang = 180-iang
    }
  a = TOCART(phiang, iang)
  return(list(az=phiang, dip=iang, x=a$x, y=a$y, z=a$z, gx=p$x, gy=p$y))
}

