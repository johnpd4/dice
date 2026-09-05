source("funcs.R")
addResourcePath("figs", "figs")

main_dice_ui = function(){
  
  nav_panel(
    
    title = "Distribution Simulation",
    
    layout_sidebar(
      
      sidebar = sidebar(
        open = "always",
        width = "20%",
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/dead_eye.png", width = "80px", style = "margin-top: 8px;"),
          
          numericInputIcon("opposite_test",
                           label = "Target",
                           value = 0,
                           min = 0,
                           max = 999
          ), # numeric input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/d1.png", width = "80px", style = "margin-top: 8px;"),
          
          numericInputIcon("flat_bonus",
                           label = "Flat Bonus",
                           value = 0,
                           min = 0,
                           max = 50
          ), # numeric input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/d4.png", width = "80px", style = "margin-top: 8px;"),
          
          numericInputIcon("num_d4",
                           label = "Number of 4-sided dice",
                           value = 0,
                           min = 0,
                           max = 20
          ), # numeric input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/d6.png", width = "80px", style = "margin-top: 8px;"),
          
          numericInputIcon("num_d6",
                           label = "Number of 6-sided dice",
                           value = 0,
                           min = 0,
                           max = 20
          ), # numeric input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/d8.png", width = "80px", style = "margin-top: 8px;"),
          
          numericInputIcon("num_d8",
                           label = "Number of 8-sided dice",
                           value = 0,
                           min = 0,
                           max = 20
          ), # numeric input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/d10.png", width = "80px", style = "margin-top: 8px;"),
          
          numericInputIcon("num_d10",
                           label = "Number of 10-sided dice",
                           value = 0,
                           min = 0,
                           max = 20
          ), # numeric input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/d12.png", width = "80px", style = "margin-top: 8px;"),
          
          numericInputIcon("num_d12",
                           label = "Number of 12-sided dice",
                           value = 0,
                           min = 0,
                           max = 10
          ), # numeric input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/d20.png", width = "80px", style = "margin-top: 8px;"),
          
          numericInputIcon("num_d20",
                           label = "Number of 20-sided dice",
                           value = 0,
                           min = 0,
                           max = 5
          ), # numeric input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/equality.png", width = "80px", style = "margin-top: 8px;"),
          
          radioButtons("draw_behavior",
                       label = "What to do with a draw",
                       choices = c("Count as win" = 1, "Keep as draw" = 2, "Count as fail" = 0),
                       selected = c("Keep as draw" = 2)
          ), # radio input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/theres_options.png", width = "80px", style = "margin-top: 8px;"),
          
          radioButtons("method",
                       label = "Method to Be Used",
                       choices = c("Probability Distribution (best)" = "dist", "Exaustive Search (slow)" = "exaustive",
                                   "Normal Approximation" = "norm", "Simulation Approximation" = "sim"),
                       selected = c("Probability Distribution (best)" = "dist")
                       
          ), # radio input
          
        ), # div
        
      ), # sidebar
      
      plotlyOutput("norm_approx")
      
    ) # layout sidebar
    
  ) # nav panel
  
} # ui

main_dice_server = function(input, output, session){
  
  dice_vec = reactive({
    dice_numbers_to_vec(dices = c(4, 6, 8, 10, 12, 20),
                        numbers = c(input$num_d4, input$num_d6, input$num_d8,
                                    input$num_d10, input$num_d12, input$num_d20))
  })
  
  dist = reactive({
    
    if(input$method == "norm"){
      dens = dice_density_norm_approx(dice_vec(),
                                      flat_bonus = input$flat_bonus,
                                      opposite_test = input$opposite_test,
                                      draw_behavior = as.numeric(input$draw_behavior))
    } else if(input$method == "exaustive"){
      dens = dice_density_exaustive(dice_vec(),
                                    flat_bonus = input$flat_bonus,
                                    opposite_test = input$opposite_test,
                                    draw_behavior = as.numeric(input$draw_behavior))
    } else if(input$method == "sim"){
      dens = dice_density_sim_approx(dice_vec(),
                                     flat_bonus = input$flat_bonus,
                                     opposite_test = input$opposite_test,
                                     draw_behavior = as.numeric(input$draw_behavior))
    } else if(input$method == "dist"){
      dens = dice_density_analytic(dice_vec(),
                                   flat_bonus = input$flat_bonus,
                                   opposite_test = input$opposite_test,
                                   draw_behavior = as.numeric(input$draw_behavior))
    }
    
    return(dens)
    
  })
  
  output$norm_approx = renderPlotly({
    dice_histogram(dist())
  })
  
} # server
