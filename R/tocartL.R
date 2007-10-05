`tocartL` <-
function(A)
{
#######   /* Convert from spherical (az, dip in degrees) to Cartesian coordinates
#######     x pos north, y pos east, z pos downward */
  
  a = TOCART(A$az, A$dip)
  return(a)
}

