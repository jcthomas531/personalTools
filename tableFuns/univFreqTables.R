#some helpful univariate frequency functions
library(dplyr)
library(arsenal)
library(stringr)
library(kableExtra)

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





#function for formatting the frequency tables caclulated with uniFreq and uniFreq2 functions
#with uniFreq: pass into x the item from the list to be formatted, use double bracket subsetting
########with uniFreq: this is done for one single element of uniFreq output at a time#########
#can also call names on the uniFreq object to know which number to use in bracket
#varName: input the desired name of the variable as a string
freqFormat <- function(x, varName = NULL, extraText = NULL) {
  #default variable name to the var name in the data
  if (is.null(varName)) {
    varName <- colnames(x)[1]
  }
  
  formed <- x |>
    kable(booktabs=TRUE,
          #"latex",
          caption = paste("Frequencies for", varName, "variable,", extraText), 
          col.names = c(varName, "Freq.", "Cum. Freq.", 
                        "Percent", "Cum. Percent"),
          linesep = "",
          align = c("l", "r", "r", "r", "r", "r")) |>
    kable_styling(latex_options = c("HOLD_position")) |> 
    column_spec(1,bold=TRUE) |>
    row_spec(0, bold = TRUE) 
  return(formed)
}

#to display all of the tables you want, use a for loop in r markdown
#but make sure that you have {r, results='asis'} in the header otherwise it prints
# a bunch of crap



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





#formatting function for groupFreq
#varNames takes a vector of strings that are the desired presentation names of the variables 
#extra text takes any additional things you want to add to title
groupFreqFormat <- function(x, varNames = NULL, extraText = NULL) {
  #number of variables in the table
  varNum <- ncol(x) - 5 #if you add more columns, this must change
  #default variable name to the var name in the data
  if (is.null(varNames)) {
    varNames <- colnames(x)[1:varNum]
  }
  
  
  #interval for the highlight switching
  #total number of rows divided by number of unique categories should give the desired intervals
  groupSize <- (x[,1] |> nrow())/(x[,1] |> unique() |> nrow())
  rows_to_color <- which((1:nrow(x) - 1) %/% groupSize %% 2 == 0)
  
  formed <- x |>
    kable(booktabs=TRUE,
          longtable = TRUE,
          #"latex",
          caption = paste("Frequencies for", varNames[1], "variable, by",
                          paste(varNames[2:varNum], extraText, sep = ", ")), 
          col.names = c(varNames, "Freq.", "Cum. Freq.", 
                        "Percent", "Cum. Percent", "Total Cum. Freq"),
          linesep = "",
          align = c("l", "l", "r", "r", "r", "r", "r")) |>
    kable_styling(latex_options = c("HOLD_position", "repeat_header")) |> 
    column_spec(1:varNum,bold=TRUE) |>
    row_spec(0, bold = TRUE) |>
    row_spec(rows_to_color, background = "#F0F0F0")
  return(formed)
}





