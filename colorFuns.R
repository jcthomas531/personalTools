################################################################################
#I believe this is how ggplot chooses colors for discrete variables
#takes n, the number of colors you want
equallySpacedColor <- function(n) {
  #stolen from stack overflow
  #https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}