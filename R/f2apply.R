f2apply <-
function( x, y, fun, knot.n=10, cuts=c("TRUE", "FALSE"), I.O.plot=c("FALSE", "TRUE"), ... ){
  x = as.PiecewiseLinearFuzzyNumber(x, knot.n)
  y = as.PiecewiseLinearFuzzyNumber(y, knot.n)

  step.x = length(supp(x)) / 30
  step.y = length(supp(y)) / 30

  if(( is.increasing.on.x(fun, x.bound=supp(x), y.bound=supp(y), step.x)  == TRUE ) &
     ( is.increasing.on.y(fun, x.bound=supp(x), y.bound=supp(y), step.y)  == TRUE ) ) 
          {
           print("fun is an increasing function from x and y on introduced bounds")
           L.result = fun( alphacut(x, seq(0, 1, len=knot.n))[,"L"] ,
                           alphacut(y, seq(0, 1, len=knot.n))[,"L"] )
           U.result = fun( alphacut(x, seq(0, 1, len=knot.n))[,"U"] ,
                           alphacut(y, seq(0, 1, len=knot.n))[,"U"] )
           result = c(L.result, U.result[length(U.result):1])
          }
  else{
  if(( is.decreasing.on.x(fun, x.bound=supp(x), y.bound=supp(y), step.x)  == TRUE ) &
     ( is.increasing.on.y(fun, x.bound=supp(x), y.bound=supp(y), step.y)  == TRUE ) ) 
          {
           print("fun is a decreasing function on x and increasing function on y on introduced bounds")
           L.result = fun( alphacut(x, seq(0, 1, len=knot.n))[,"U"] ,
                           alphacut(y, seq(0, 1, len=knot.n))[,"L"] )
           U.result = fun( alphacut(x, seq(0, 1, len=knot.n))[,"L"] ,
                           alphacut(y, seq(0, 1, len=knot.n))[,"U"] )
           result = c(L.result, U.result[length(U.result):1])
          }
  else{
  if(( is.increasing.on.x(fun, x.bound=supp(x), y.bound=supp(y), step.x)  == TRUE ) &
     ( is.decreasing.on.y(fun, x.bound=supp(x), y.bound=supp(y), step.y)  == TRUE ) ) 
          {
           print("fun is an increasing function on x and decreasing function on y on introduced bounds")
           L.result = fun( alphacut(x, seq(0, 1, len=knot.n))[,"L"] ,
                           alphacut(y, seq(0, 1, len=knot.n))[,"U"] )
           U.result = fun( alphacut(x, seq(0, 1, len=knot.n))[,"U"] ,
                           alphacut(y, seq(0, 1, len=knot.n))[,"L"] )
           result = c(L.result, U.result[length(U.result):1])
          }
  else{
  if(( is.decreasing.on.x(fun, x.bound=supp(x), y.bound=supp(y), step.x)  == TRUE ) &
     ( is.decreasing.on.y(fun, x.bound=supp(x), y.bound=supp(y), step.y)  == TRUE ) ) 
          {
           print("fun is a decreasing function from x and y on introduced bounds")
           L.result = fun( alphacut(x, seq(0, 1, len=knot.n))[,"U"] ,
                           alphacut(y, seq(0, 1, len=knot.n))[,"U"] )
           U.result = fun( alphacut(x, seq(0, 1, len=knot.n))[,"L"] ,
                           alphacut(y, seq(0, 1, len=knot.n))[,"L"] )
           result = c(L.result, U.result[length(U.result):1])
          }
  else{
    print("fun is not a monoton function on x and y for the introduced bounds. Therefore this function is not appliable for computation.")
  }
  }
  }
  }

    Alphacuts = c( seq(0,1,len=knot.n) , seq(1,0,len=knot.n) )
if (I.O.plot == TRUE)
   {
   op <- par(mfrow = c(3, 1))
   plot(x, ylab="membership func. of x")
   plot(y, col=1, xlab="y", ylab="membership func. of y")
   plot(result, Alphacuts, lwd=2, xlab="fun(x,y)", ylab="membership func. of fun(x,y)", ...)
   abline(v=fun(core(x),core(y)), lty=3)
   par(op)
   }
   else{
        plot(result, Alphacuts, xlab="fun(x,y)", ylab="membership func. of fun(x,y)", ...)
       }

  if( cuts == TRUE ){
    result2 <- c(L.result[length(L.result):1], U.result[length(U.result):1])
    cuts <- matrix(result2, ncol=2, byrow=FALSE, 
                   dimnames = list( round((length(L.result)-1):0/(length(L.result)-1),3), c("L", "U"))) 
    print( "The alphacuts of the result are:" )
    return( cuts )
    }

}
