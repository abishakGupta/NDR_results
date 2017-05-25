#' This function normalizes the drug treated readouts. It uses the metric normalized drug response (NDR) proposed in Gupta et. al.  

computeNDR <- function(initReading, endReading, rateNeg,ratePos){
  try(if(nargs() < 4) stop("Number of arguments should be 4"))
  NDRvalues <- ((1-2^(log2(endReading/initReading)/log2(ratePos)))/ (1-2^(log2(rateNeg)/log2(ratePos))))
  return(NDRvalues)
}