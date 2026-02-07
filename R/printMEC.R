`printMEC` <-
function(x, digits = max(3, getOption("digits") - 3), ...)
  {
   #  message("Plane 1: ")
    message("Plane 1: ", paste(collapse=' ',  format(c(x$F$az, x$F$dip), digits = digits) ))
    #  message("Plane 2: ")
    message("Plane 2: " ,paste(collapse=' ', format(c(x$G$az, x$G$dip), digits = digits) ))

    #  message("Vector 1: ")
    message("Vector 1: " , paste(collapse=' ', format(c(x$U$az, x$U$dip), digits = digits) ))
    #  message("Vector 2: ")
    message("Vector 2: " , paste(collapse=' ',format(c(x$V$az, x$V$dip), digits = digits) ))

     # message("P-axis: ")
    message("P-axis: " ,paste(collapse=' ', format(c(x$P$az, x$P$dip), digits = digits) ))
     #  message("T-axis: ")
    message("T-axis: ", paste(collapse=' ', format(c(x$T$az, x$T$dip), digits = digits) ))

    

    message("\n")


  }

