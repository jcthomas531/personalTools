#ideally this version will be removed when i can get kableExtra functioning on the HPS

#some helpful univariate frequency functions
#MODIFIED FOR HPC REMOVING ALL DEPENDENCY ON kableExtra
#scripts will perform calculations on HPC and then return the tables which
#will be formatted locally
library(dplyr)
library(arsenal)
library(stringr)

#formatting percentages in a way that I like
specPerc <- function(x){
  return(paste0(round(x,2), "%"))
}

#this function creates a frequency table for each variable in the data set
uniFreq <- function(x) {
  #preparing each frequency table
  freqLookup <- 1:ncol(x)
  freqHolder <- vector(mode = "list", length = length(freqLookup))
  for (i in 1:length(freqHolder)) {
    #get the appropriate column number
    colnum <- freqLookup[i]
    #create frequency table
    freqHolder[[i]] <- x[,colnum] |> 
      table() |>
      freqlist() |>
      as.data.frame()
    #format percentages correctly
    freqHolder[[i]]$freqPercent <- specPerc(freqHolder[[i]]$freqPercent)
    freqHolder[[i]]$cumPercent <- specPerc(freqHolder[[i]]$cumPercent)
    #rename first column with variable name
    colnames(freqHolder[[i]])[1] <- colnames(x)[colnum]
    #rename list item with correct variable name
    names(freqHolder)[i] <- colnames(x)[colnum]
  }
  return(freqHolder)
}


# create a frequency table for a single variable at a time
# data is the name of the data frame
#variable is the name of the variable stored as a character
#can be formatted with teh freqFormat function
#a potential issue here is if you use this with a numeric variable it will probably
# have all the factor levels in a random order
# to combat this, could add an if statement to this code to specifically handle
# turing numeric to factor and getting the order of the levels right

#adding in an option to include or exclude NA in the table

uniFreq2 <- function(data, variable, include_NA = TRUE) {
  
  
  if (include_NA == TRUE) {
    #select column of interest and create basic summary
    sumDat <- data |>
      select(all_of(variable)) |>
      mutate(across(all_of(variable), ~ as.factor(.)),
             across(all_of(variable), ~ addNA(.))) |>
      group_by(across(all_of(variable)), .drop = FALSE) |>
      summarise("Freq" = n(),
                "freqPercent" = n()/nrow(data))
    #cumulative frequencies and percents
    sumDat$cumFreq <- cumsum(sumDat$Freq)
    sumDat$cumPercent <- cumsum(sumDat$freqPercent)
    #formatting
    sumDat <- sumDat |>
      relocate(cumFreq, .after = Freq) |>
      mutate(freqPercent = formatC(freqPercent*100, format = "f", digits = 2) |> paste0("%"),
             cumPercent = formatC(cumPercent*100, format = "f", digits = 2) |> paste0("%"),
             Freq = format(Freq, big.mark = ",", scientific = FALSE),
             cumFreq = format(cumFreq, big.mark = ",", scientific = FALSE))
  } else { #no including NA
    sumDat <- data |>
      select(all_of(variable)) |>
      na.omit() |>
      mutate(across(all_of(variable), ~ as.factor(.))) |>
      group_by(across(all_of(variable)), .drop = FALSE) |>
      summarise("Freq" = n(),
                "freqPercent" = n()/nrow(data))
    sumDat$cumFreq <- cumsum(sumDat$Freq)
    sumDat$cumPercent <- cumsum(sumDat$freqPercent)
    sumDat <- sumDat |>
      relocate(cumFreq, .after = Freq) |>
      mutate(freqPercent = formatC(freqPercent*100, format = "f", digits = 2) |> paste0("%"),
             cumPercent = formatC(cumPercent*100, format = "f", digits = 2) |> paste0("%"),
             Freq = format(Freq, big.mark = ",", scientific = FALSE),
             cumFreq = format(cumFreq, big.mark = ",", scientific = FALSE))
  }
  
  
  
  
  
  
  return(sumDat)
}







#variable: a string with the name of the main variable of interest
#groupings: a vector of strings with the name of the variables interested in grouping on
#unlike uniFreq(), this creates one table at a time
#this function has its own formatting function, see below
#THIS IS SET UP FOR ONLY TWO VARIABLES
# ie ONE VARIABLE FOR variable ARGUMENT, ONE VATIABLE FOR groupings ARGUEMENT
#IF YOU WANT MORE GROUPINGS YOU NEED TO ADD MORE UNGROUPING AND GROUPING ARGUEMENT
#TO GET THE cumulative frequency and cumulative percent correct
#adding an argument to inlcude or exclude NA in the table
groupFreq <- function(data, variable, groupings, include_NA = TRUE) {
  #concatenate variables of intereste
  colsInter <- c(variable, groupings)
  #select columns of interest and create basic summary
  
  
  if (include_NA == TRUE) {
    sumDat <- data |>
      select(all_of(colsInter)) |>
      mutate(across(all_of(colsInter), ~ as.factor(.)),
             across(all_of(colsInter), ~ addNA(.))) |>
      group_by(across(all_of(colsInter)), .drop = FALSE) |>
      summarise("Freq" = n()
      ) |>
      ungroup() |>
      group_by(across(all_of(colsInter[1]))) |> 
      mutate(cumFreq = cumsum(Freq),
             freqPercent = Freq/sum(Freq),
             cumPercent = cumsum(freqPercent)
      ) #the logic to get this correct was insane lol
  } else { #excluding NA 
    sumDat <- data |> 
      select(all_of(colsInter)) |>
      na.omit() |>
      mutate(across(all_of(colsInter), ~ as.factor(.))) |>
      group_by(across(all_of(colsInter)), .drop = FALSE) |>
      summarise("Freq" = n()
      ) |>
      ungroup() |>
      group_by(across(all_of(colsInter[1]))) |> 
      mutate(cumFreq = cumsum(Freq),
             freqPercent = Freq/sum(Freq),
             cumPercent = cumsum(freqPercent))
  }
  
  
  #formatting
  sumDat$totCumFreq <- cumsum(sumDat$Freq)
  sumDat <- sumDat |>
    relocate(cumFreq, .after = Freq) |>
    mutate(freqPercent = formatC(freqPercent*100, format = "f", digits = 2) |> paste0("%"), 
           cumPercent = formatC(cumPercent*100, format = "f", digits = 2) |> paste0("%"),
           Freq = format(Freq, big.mark = ",", scientific = FALSE),
           cumFreq = format(cumFreq, big.mark = ",", scientific = FALSE),
           totCumFreq = format(totCumFreq, big.mark = ",", scientific = FALSE)
    )
  
  
  
  return(sumDat)
}









