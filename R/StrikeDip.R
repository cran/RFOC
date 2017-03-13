StrikeDip <-
function(x=x, y=y, MEC, focsiz, addDIP=TRUE, ...)
  {
###  Author: Jonathan Lees, Univerity of North Carolina, Feb 2010
                                        #  input:
###    x,y = location on the plot
###  MEC is theoutput of SDRfoc  program
###    focsiz  size of the focal sphere
    
#### using a strike and dip of a focal mechanism
#### plot a segment along the strike and
####a perpendicular segment representing the dip
###    see geotouch: this is the ralph archuletta style

###   Plot Style: planes(only)
###     planes/PT
###     PT (only)
###     add fault plane
###     fault plane only

    ##  points(x, y, col='blue', pch=3)

    if(missing(addDIP)) { addDIP=TRUE }
    ###############  use this to set plotting region for symbols
    vadj=0.5
    hadj=0.5

    xy <- xy.coords(x,y,recycle=TRUE)

    pin <- par('pin')
    usr <- par('usr')
    usr.x <- usr[2] - usr[1]
    usr.y <- usr[4] - usr[3]

    tmp <- list()
    tmp$x <- grconvertX(xy$x, to='npc')
    tmp$y <- grconvertY(xy$y, to='npc')

    tmp.xlen <- length(tmp$x)


    focsiz <- rep(focsiz, length.out=tmp.xlen)
    x.low  <- tmp$x -    hadj *focsiz/pin[1]
    x.high <- tmp$x + (1-hadj)*focsiz/pin[1]
    y.low  <- tmp$y -    vadj *focsiz/pin[2]
    y.high <- tmp$y + (1-vadj)*focsiz/pin[2]


    xy.low <- list()
    xy.low$x <- grconvertX(x.low, from='npc', to='nfc')
    xy.low$y <- grconvertY(y.low, from='npc', to='nfc')

    xy.high <- list()
    xy.high$x <- grconvertX(x.high, from='npc', to='nfc')
    xy.high$y <- grconvertY(y.high, from='npc', to='nfc')

    op <- par(c('plt','usr','xpd'))
    on.exit(par(op))
    par(xpd=TRUE)
    par(plt=c(xy.low$x,xy.high$x,xy.low$y,xy.high$y), new=TRUE)
    par(usr=c(-1,1,-1,1))
######################

    strk1 = MEC$az1
    dip1 = MEC$dip1
    
    tq = 1.0;
    xi =  0.0174532 * strk1 - 1.570796327;
    ang = strk1*pi/180
    dip  = dip1*pi/180
    
    p1x = sin(ang)
    p1y = cos(ang)
    p2x = -sin(ang)
    p2y = -cos(ang)
    
### points(p1x, p1y, col='blue', pch=3)
###  points(p2x, p2y, col='blue', pch=3)

    segments(p1x, p1y, p2x, p2y, ...)
    
    
####	 XDrawLine(dpy, xwin, gc, kx, ky, kx2, ky2);
    
####	 /*   Down  Dip line    */

    if(addDIP)
      {
        F = focpoint(MEC$F$az, MEC$F$dip, col = 'black' , lab = "F", UP = MEC$UP, PLOT = FALSE)


        p3x = F$x
        p3y = F$y

        segments(0, 0, p3x, p3y, ...)

      }


  }
