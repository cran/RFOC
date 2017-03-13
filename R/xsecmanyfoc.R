xsecmanyfoc <-
function(MEK, theta=NULL, focsiz=0.5,  foccol=NULL, UP=TRUE, focstyle=1,  LEG = FALSE, DOBAR=FALSE)
    {
        if(missing(theta)) { theta = NULL }
        
        if(missing(UP)) UP = TRUE
        
        
############################  focal mech have this list structure

#########    MEKS = list(lon=0, lat=0, str1=0, dip1=0, rake1=0, lat=0, lon=0, dep=0, name="", Elat=0, Elon=0)
###  Elat and Elon are the exploded lat-lon (a line will be drawn from the
########    epicenter to the plotting position
        
        
        if(missing(foccol)) foccol=NULL
        if(missing(LEG))   LEG = FALSE
        if(missing(DOBAR))  DOBAR=FALSE

        
        if(missing(focsiz)) { focsiz = 0.04 }
        
        if(length(MEK$Elat)<1 | (length(MEK$Elon)<1))
            {
                MEK$Elat = rep(NA, length(MEK$lat))
                MEK$Elon = rep(NA, length(MEK$lat))

            }

        
        
        XYplode = list(x=MEK$Elat, y=MEK$Elon)

        tem1 = list(x=MEK$x, y=MEK$y)
        
        tem2 = tem1


        ww = which(!is.na(MEK$x))


        
        if(  length(ww) < 1 ) {   return()  }

##############################  loop through focal mechs and plot each one
        for(IW in 1:length(ww))
            {

                i = ww[IW]
                

                lind= foc.icolor(MEK$rake1[i])
                focal.col = foc.color(lind, pal=1)

                
                a1  =  SDRfoc(MEK$str1[i], MEK$dip1[i],MEK$rake1[i] , u=UP, ALIM=c(-1,-1, +1, +1), PLOT=FALSE)

                if(!is.null(theta))
                    {
############  this is the rotation stuff:
                        b1 = Rotfocphi(theta, a1$M$uaz, a1$M$ud, a1$M$vaz,
                            a1$M$vd,  a1$M$az1,  a1$M$d1,  a1$M$az2,  a1$M$d2,
                            a1$M$paz,  a1$M$pd,  a1$M$taz,  a1$M$td)


                        
                        ## MEC = b1
                        FMEC=list(UP=a1$UP, LIM=a1$LIM, P=b1$P,  az1=b1$A2$az-90  , 
                            dip1=b1$A2$dip , az2=b1$A1$az-90, dip2=b1$A1$dip )
                        
                        
                        
                        FMEC$sense = 0
                        
                        pax = focpoint(FMEC$P$az, FMEC$P$dip,  lab="P", UP=FMEC$UP, PLOT=FALSE)
                        
                        PLS = polyfoc(FMEC$az1, FMEC$dip1, FMEC$az2, FMEC$dip2, UP=FMEC$UP, PLOT = FALSE)
                        kin = splancs::inout(cbind(pax$x, pax$y) ,cbind(PLS$Px, y =PLS$Py), bound=TRUE)

                        if(kin==0) FMEC$sense = 1
                        
                        
                        ang2 = GetRakeSense(b1$U$az, b1$U$dip, b1$V$az, b1$V$dip ,b1$P$az, b1$P$dip , b1$T$az, b1$T$dip )
                        
                        MEC = GetRake(FMEC$az1, FMEC$dip1, FMEC$az2, FMEC$dip2, ang2)
                        
                        MEC$sense = ang2
                        MEC$UP = TRUE
                        MEC = c(MEC, b1)
                        
                    }
                else
                    {
                        MEC = a1

                    }
                

                
                if(focstyle==1)
                    {
                        justfocXY(MEC, x=tem1$x[i],  y= tem1$y[i],  focsiz=focsiz,
                                   fcol =focal.col , fcolback = "white", xpd = TRUE)
                    }
                
                if(focstyle==2)
                    {
                        nipXY(MEC, x=tem1$x[i],  y= tem1$y[i],   focsiz =focsiz,
                               fcol =focal.col, nipcol="black", cex=.4)
                        
                    }
                if(focstyle==4)
                    {
### 
                        
                        PTXY2( tem1$x[i], tem1$y[i] , MEC  ,focsiz,  col=focal.col, pt=0, lwd=2 )
                        
                        
                    }
                if(focstyle==5)
                    {
### 
                        
                        PTXY2( tem1$x[i], tem1$y[i] , MEC  ,focsiz,  col=focal.col, pt=1, lwd=2 )
                        
                        
                    }
                if(focstyle==6)
                    {
###  
                        
                        PTXY2( tem1$x[i], tem1$y[i] , MEC  ,focsiz,  col=focal.col, pt=2, lwd=2 )
                        
                        
                    }


                
                
                
            }



    if(LEG)
      {
        fleg = 1:7
        flegc = foc.color(fleg, pal=1)
        flab = focleg(fleg)

        legend("topleft", legend=flab, col=flegc, pch=19, bg="white" )
      }


    
  }
