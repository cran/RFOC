flipnodal<-function(s1, d1, r1)
    {
        #####  flip or switch the nodal planes on a focal mechanism
        a2 = SDRfoc(s1, d1, r1, PLOT =FALSE)
        return(list(strike=a2$az2, dip=a2$dip2, rake=a2$rake2) )
    }
