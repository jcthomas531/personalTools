################################################################################
#function for calculating the press residuals for a lm model
#there are existing versions of this in some packages but most just give you the
#the press statistic rather than the resids and the one i found that did give resids
#was not very cooperative. this one works and is simple. it is only designed to 
#work with lm and accepts an lm object as the sole arguement and outputs the press
#residuals aka the predicted residuals, shout out to Dr. Zamba and linear models 
#hw9 for this simple approach to calculation
pressResids <- function(model) {
  #extract model matrix
  X <- model.matrix(model)
  #calculate projection matrix and extract diagonals
  pii <- (X %*% solve(t(X) %*% X) %*% t(X)) |>
    diag()
  #calculate regular residuals
  res <- resid(m2)
  #calculate press residuals
  pressRes <- res/(1-pii)
  
  return(pressRes)
}
