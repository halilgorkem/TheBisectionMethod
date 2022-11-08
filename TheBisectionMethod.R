f <-function(x)
{
  f <- x^3+4*x^2-10
  return(f)
}
a <- 1;b <- 2

bisection <-  function(f, a, b, tol = 1e-5, maxiter = 100)
{
  #step1
  iter <-  1
  FA <-  f(a)
  
  #step2
  p_seq <- c()
  while (iter <= maxiter) 
  {
    
    #step3
    p <-  a + (b - a) / 2
    FP <- f(p)
    p_seq[iter] <- p
    
    #step4
    if(FP == 0 || ((b - a) / 2) < tol)
    {
      stop(p, "Procedure completed successfully.")
      return(as.data.frame(p_seq))
    }
    
    #step5
    iter <-  iter + 1
    
    #step6
    if(FA * FP > 0)
    {
      a <- p
    }else{
      b <- p
    }
  }
  
  #step7
  stop("başarısız oldu")
}
bisection(f,a,b)
