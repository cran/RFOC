`ternfoc.point` <-
function(deltaB,  deltaP,  deltaT)
{
####  center of plot angle sin(fb)^2=sin(fp)^2=sin(ft)^2  = 1/3
### Frohlic: azimuthal gnomic projection
####  ANGC =  asin(  sqrt(1/3) )*180/pi
    
    ANGC = 35.26438968275465
######### /* plot a focal point on a ternary diagram  */
  DEG2RAD = pi/180


  db=deltaB*DEG2RAD;
  dp=deltaP*DEG2RAD;
  dt=deltaT*DEG2RAD;
  
  censin=sin(DEG2RAD*ANGC);
  cencos=cos(DEG2RAD*ANGC);
  
  phi =atan2( sin(dt),sin(dp)) - 45*DEG2RAD;

  denom = (censin*sin(db)+cencos*cos(db)*cos(phi) );
  
  h = (cos(db)*sin(phi))/denom;
  
  v = (cencos*sin(db)-censin*cos(db)*cos(phi))/denom;

  return(list(h=h, v=v))
}

