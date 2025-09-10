library(kableExtra)


################################################################################
#very basic table formatting for a pdf document
basicKablePdf <- function(table, 
                          title = "title",
                          colNames = NULL,
                          digits = NULL) {
  #argument set up
  kableArgs <- list(x = table, caption = title,
                    longtable=FALSE, format = "latex", booktabs = TRUE)
  if (!is.null(colNames)) kableArgs$col.names <- colNames
  if (!is.null(digits)) kableArgs$digits = digits
  #table creation
  retTab <- do.call(kable, kableArgs) |>
    kable_styling(latex_options = c("HOLD_position", "scale_down")) |>
    column_spec(1,bold=TRUE) |>
    row_spec(0, bold = TRUE) 
  
  
  #since this returns a kable object, you can pipe it into other kable extra funs
  return(retTab)
  
}
