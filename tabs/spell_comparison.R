source("funcs.R")
addResourcePath("figs", "figs")

spell_comparison_ui = function(){
  
  nav_panel(
    
    title = "Spell Comparison",
    
    layout_sidebar(
      
      sidebar = sidebar(
        open = "always",
        width = "25%",
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/theres_options.png", width = "80px", style = "margin-top: 8px;"),
          
          radioButtons("method",
                       label = "Method to Be Used",
                       choices = c("Probability Distribution (best)" = "dist", "Exaustive Search (may destroy pc)" = "exaustive",
                                   "Normal Approximation" = "norm", "Simulation Approximation" = "sim"),
                       selected = c("Probability Distribution (best)" = "dist")
                       
          ), # radio input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/options.png", width = "80px", style = "margin-top: 8px;"),
          
          radioButtons("use_target",
                       label = "Use a Target?",
                       choices = c("No, only compare" = FALSE, "Yes, show a target" = TRUE),
                       selected = c("No, only compare" = FALSE)
                       
          ), # radio input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/more_options.png", width = "80px", style = "margin-top: 8px;"),
          
          radioButtons("type",
                       label = "Type of Plot",
                       choices = c("Negative" = "negative", "Opacity" = "opacity"),
                       selected = c("Negative" = "negative")
                       
          ), # radio input
          
        ), # div
        
        conditionalPanel(
          condition = "input.use_target == 'TRUE'",
        
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
        
        ), # conditional panel
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/d1.png", width = "80px", style = "margin-top: 8px;"),
          
          numericInputIcon("s1_flat_bonus",
                           label = "Spell 1 Flat Bonus",
                           value = 0,
                           min = 0,
                           max = 999
          ), # numeric input
          
          numericInputIcon("s2_flat_bonus",
                           label = "Spell 2 Flat Bonus",
                           value = 0,
                           min = 0,
                           max = 999
          ), # numeric input
          
        ), # div
        
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/d4.png", width = "80px", style = "margin-top: 8px;"),
          
          numericInputIcon("s1_num_d4",
                           label = "d4's Spell 1",
                           value = 0,
                           min = 0,
                           max = 20
          ), # numeric input
          
          numericInputIcon("s2_num_d4",
                           label = "d4's Spell 2",
                           value = 0,
                           min = 0,
                           max = 20
          ), # numeric input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/d6.png", width = "80px", style = "margin-top: 8px;"),
          
          numericInputIcon("s1_num_d6",
                           label = "d6's Spell 1",
                           value = 0,
                           min = 0,
                           max = 20
          ), # numeric input
          
          numericInputIcon("s2_num_d6",
                           label = "d6's Spell 2",
                           value = 0,
                           min = 0,
                           max = 20
          ), # numeric input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/d8.png", width = "80px", style = "margin-top: 8px;"),
          
          numericInputIcon("s1_num_d8",
                           label = "d8's Spell 1",
                           value = 0,
                           min = 0,
                           max = 20
          ), # numeric input
          
          numericInputIcon("s2_num_d8",
                           label = "d8's Spell 2",
                           value = 0,
                           min = 0,
                           max = 20
          ), # numeric input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/d10.png", width = "80px", style = "margin-top: 8px;"),
          
          numericInputIcon("s1_num_d10",
                           label = "d10's Spell 1",
                           value = 0,
                           min = 0,
                           max = 20
          ), # numeric input
          
          numericInputIcon("s2_num_d10",
                           label = "d10's Spell 2",
                           value = 0,
                           min = 0,
                           max = 20
          ), # numeric input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/d12.png", width = "80px", style = "margin-top: 8px;"),
          
          numericInputIcon("s1_num_d12",
                           label = "d12's Spell 1",
                           value = 0,
                           min = 0,
                           max = 10
          ), # numeric input
          
          numericInputIcon("s2_num_d12",
                           label = "d12's Spell 2",
                           value = 0,
                           min = 0,
                           max = 10
          ), # numeric input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/d20.png", width = "80px", style = "margin-top: 8px;"),
          
          numericInputIcon("s1_num_d20",
                           label = "d20's Spell 1",
                           value = 0,
                           min = 0,
                           max = 5
          ), # numeric input
          
          numericInputIcon("s2_num_d20",
                           label = "d20's Spell 2",
                           value = 0,
                           min = 0,
                           max = 5
          ), # numeric input
          
        ), # div
        
      ), # sidebar
      
      plotlyOutput("comparison_plot")
      
    ) # layout sidebar
    
  ) # nav panel
  
} # ui

spell_comparison_server = function(input, output, session){
  
  dice_vec_s1 = reactive({
    dice_numbers_to_vec(dices = c(4, 6, 8, 10, 12, 20),
                        numbers = c(input$s1_num_d4, input$s1_num_d6, input$s1_num_d8,
                                    input$s1_num_d10, input$s1_num_d12, input$s1_num_d20))
  })
  
  dist_s1 = reactive({
    
    if(input$method == "norm"){
      dens = dice_density_norm_approx(dice_vec_s1(),
                                      flat_bonus = input$s1_flat_bonus,
                                      opposite_test = input$opposite_test,
                                      draw_behavior = as.numeric(input$draw_behavior))
    } else if(input$method == "exaustive"){
      dens = dice_density_exaustive(dice_vec_s1(),
                                    flat_bonus = input$s1_flat_bonus,
                                    opposite_test = input$opposite_test,
                                    draw_behavior = as.numeric(input$draw_behavior))
    } else if(input$method == "sim"){
      dens = dice_density_sim_approx(dice_vec_s1(),
                                     flat_bonus = input$s1_flat_bonus,
                                     opposite_test = input$opposite_test,
                                     draw_behavior = as.numeric(input$draw_behavior))
    } else if(input$method == "dist"){
      dens = dice_density_analytic(dice_vec_s1(),
                                   flat_bonus = input$s1_flat_bonus,
                                   opposite_test = input$opposite_test,
                                   draw_behavior = as.numeric(input$draw_behavior))
    }
    
    return(dens)
    
  })
  
  dice_vec_s2 = reactive({
    dice_numbers_to_vec(dices = c(4, 6, 8, 10, 12, 20),
                        numbers = c(input$s2_num_d4, input$s2_num_d6, input$s2_num_d8,
                                    input$s2_num_d10, input$s2_num_d12, input$s2_num_d20))
  })
  
  dist_s2 = reactive({
    
    if(input$method == "norm"){
      dens = dice_density_norm_approx(dice_vec_s2(),
                                      flat_bonus = input$s2_flat_bonus,
                                      opposite_test = input$opposite_test,
                                      draw_behavior = as.numeric(input$draw_behavior))
    } else if(input$method == "exaustive"){
      dens = dice_density_exaustive(dice_vec_s2(),
                                    flat_bonus = input$s2_flat_bonus,
                                    opposite_test = input$opposite_test,
                                    draw_behavior = as.numeric(input$draw_behavior))
    } else if(input$method == "sim"){
      dens = dice_density_sim_approx(dice_vec_s2(),
                                     flat_bonus = input$s2_flat_bonus,
                                     opposite_test = input$opposite_test,
                                     draw_behavior = as.numeric(input$draw_behavior))
    } else if(input$method == "dist"){
      dens = dice_density_analytic(dice_vec_s2(),
                                   flat_bonus = input$s2_flat_bonus,
                                   opposite_test = input$opposite_test,
                                   draw_behavior = as.numeric(input$draw_behavior))
    }
    
    return(dens)
    
  })
  
  output$comparison_plot = renderPlotly({
    
    spell_comparison_plot(dist_s1(), dist_s2(), do_test = input$use_target,
                          opposite_test = input$opposite_test, type = input$type)
    
  })
  
} # server
