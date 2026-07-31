if(!require(pacman)){install.packages("pacman")}
pacman::p_load(plotly, purrr, shiny, scales, patchwork, ggplot2, forcats, shinyWidgets)

source("funcs.R")
source("tabs/main_dice.R")
source("tabs/opposite_test.R")
source("tabs/approx_vs_exaustive.R")

server = function(input, output, session){
  
  # in tabs/main_dice.R
  main_dice_server(input, output, session)
  
  # in tabs/opposite_test.R
  opposite_test_server(input, output, session)
  
  # in tabs/approx_vs_exaustive.R
  approx_vs_exaustive_server(input, output, session)
  
}