sderror <- function(values) {
  
  if (any(is.na(values))) warning("NAs were removed")

  nonas <- na.omit(values)
  standardError = sd(nonas)/sqrt(length(nonas))
  
  return(standardError)
  
}