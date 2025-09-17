library(vcdExtra)
goodKruskGamma <- function (x) {
  #x should be a data frame where the first column is the y variable 
  #and the rest of the columns are the x variables
  #see catagorical data analysis by agresti, page 88
  #checked that this ASE is in fact the correct ASE by copying an exampled
  #from SAS
  tableHolder <- vector(mode = "list", length = ncol(x)-1)
  testHolder <- vector(mode = "list", length = ncol(x)-1)
  resultHolder <- vector(mode = "list", length = ncol(x)-1)
  for (i in 1:(ncol(x)-1)) {
    tableHolder[[i]] <- table(x[[i+1]], x[[1]])
    gammaTest <- vcdExtra::GKgamma(tableHolder[[i]])
    testHolder[[i]] <- list("gamma" = gammaTest$gamma,
                            "ASE" = gammaTest$sigma,
                            "Z" = gammaTest$gamma/gammaTest$sigma,
                            "p-val" = pnorm(abs(gammaTest$gamma/gammaTest$sigma),
                                            lower.tail = FALSE)*2
    )
    resultHolder[[i]] <- list("contingencyTable" = tableHolder[[i]],
                              "gammaTest" = testHolder[[i]])
    names(resultHolder)[i] <- colnames(x)[i+1]
  }
  return(resultHolder)
}
