library(FitAR)
HamiltonAdjust <- function(x,h,d=4) {
  p_ham = h + (0:(d-1))
  s=tsp(x)[1]; f=tsp(x)[3]
  x.AR <- FitAR(x,p=p_ham,ARModel="ARp")
  
  res <- list(
    trend=ts(x.AR$fit,start=s,frequency=f),
    res=ts(x.AR$res,start=s,frequency=f)
  )
  return(res)
}