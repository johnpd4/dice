if(!require(pacman)){install.packages("pacman")}
pacman::p_load(shiny, bslib, plotly, shinyWidgets)

source("tabs/main_dice.R")
source("tabs/opposite_test.R")
source("tabs/approx_vs_exaustive.R")

ui = page_navbar(
  title = "Dices",
  theme = bs_theme(preset = "flatly"),
  
  # in tabs/main_dice.R
  main_dice_ui(),
  
  # in tabs/opposite_test.R
  opposite_test_ui(),
  
  # in tabs/approx_vs_exaustive
  approx_vs_exaustive_ui(),
  
)