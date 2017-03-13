PTXY2 <-
function (x = x, y = y, MEC, focsiz=0.5, pt=0,  ...) 
{
    if(missing(pt)) { pt=0 }
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



    
    if(pt==0 | pt==1)
        {
            Ppnt = focpoint(MEC$P$az, MEC$P$dip, pch=18, lab="P", UP=MEC$UP, PLOT=FALSE)
            p1x =  Ppnt$x
            p1y =  Ppnt$y
            segments(0, 0, p1x, p1y, ...)
        }

    if(pt==0 | pt==2)
        {
            Tpnt = focpoint(MEC$T$az, MEC$T$dip, pch=18, lab="P", UP=MEC$UP, PLOT=FALSE)
            p3x =  Tpnt$x
            p3y =  Tpnt$y
            segments(0, 0, p3x, p3y, ...)
        }

    
}
