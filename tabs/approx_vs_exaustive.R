source("funcs.R")
addResourcePath("figs", "figs")

approx_vs_exaustive_ui = function(){
  
  nav_panel(
    
    title = "Dist. Method Comparison",
    
    layout_sidebar(
      
      sidebar = sidebar(
        open = "always",
        width = "20%",
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/theres_options.png", width = "80px", style = "margin-top: 8px;"),
          
          radioButtons("method_1",
                       label = "Which method to use for Dist. 1 (exacts)",
                       choices = c("Exaustive Search (slow)" = "exaustive",
                                   "Analytical Dice Distribution" = "dist"),
                       selected = c("Analytical Dice Distribution" = "dist")
          ), # radio input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/more_options.png", width = "80px", style = "margin-top: 8px;"),
          
          radioButtons("method_2",
                       label = "Which method to use for Dist. 2",
                       choices = c("Exaustive Search (slow)" = "exaustive",
                                   "Analytical Dice Distribution" = "dist",
                                   "Normal Approximation (approx.)" = "norm",
                                   "Simulation (approx.)" = "sim"),
                       selected = c("Normal Approximation" = "norm")
          ), # radio input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/d4.png", width = "80px", style = "margin-top: 8px;"),
          
          numericInputIcon("num_d4_timed",
                           label = "Number of 4-sided dice",
                           value = 0,
                           min = 0,
                           max = 5
          ), # numeric input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/d6.png", width = "80px", style = "margin-top: 8px;"),
          
          numericInputIcon("num_d6_timed",
                           label = "Number of 6-sided dice",
                           value = 0,
                           min = 0,
                           max = 5
          ), # numeric input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/d8.png", width = "80px", style = "margin-top: 8px;"),
          
          numericInputIcon("num_d8_timed",
                           label = "Number of 8-sided dice",
                           value = 0,
                           min = 0,
                           max = 5
          ), # numeric input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/d10.png", width = "80px", style = "margin-top: 8px;"),
          
          numericInputIcon("num_d10_timed",
                           label = "Number of 10-sided dice",
                           value = 0,
                           min = 0,
                           max = 4
          ), # numeric input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/d12.png", width = "80px", style = "margin-top: 8px;"),
          
          numericInputIcon("num_d12_timed",
                           label = "Number of 12-sided dice",
                           value = 0,
                           min = 0,
                           max = 3
          ), # numeric input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/d20.png", width = "80px", style = "margin-top: 8px;"),
          
          numericInputIcon("num_d20_timed",
                           label = "Number of 20-sided dice",
                           value = 0,
                           min = 0,
                           max = 3
          ), # numeric input
          
        ), # div
        
        p("Only the biggest 6 dices are used. You can only have 3 of each dice.")
        
      ), # sidebar
      
      # textOutput("exaustive_time"),
      # textOutput("norm_approx_time"),
      layout_columns(
        
        value_box(
          title = tags$span("Distribution Method 1", style = "font-size: 26px; font-weight: bold;"),
          p("Error = 0"),
          tags$span(textOutput("exaustive_time"), style = "font-size: 26px; font-weight: bold;"),
          theme = value_box_theme(bg = "#2E8B57")
        ), # card
        
        value_box(
          title = tags$span("Distribution Method 2", style = "font-size: 26px; font-weight: bold;"),
          textOutput("approx_error"),
          tags$span(textOutput("norm_approx_time"), style = "font-size: 26px; font-weight: bold;"),
          theme = value_box_theme(bg = "#00B894")
        ), # card
      
      ), # layout columns
      
      plotlyOutput("diff_plot")
      
    ) # layout sidebar
    
  ) # nav panel
  
} # ui

approx_vs_exaustive_server = function(input, output, session){
  
  dice_vec_timed = reactive({
    x = dice_numbers_to_vec(dices = c(4, 6, 8, 10, 12, 20),
                            numbers = c(input$num_d4_timed, input$num_d6_timed, input$num_d8_timed,
                                        input$num_d10_timed, input$num_d12_timed, input$num_d20_timed))
    
    if (is.null(x) || length(x) == 0) {
      x = c(4, 4)
    } else {
      x = sort(x, decreasing = TRUE)[1:min(6, length(x))]
    }
    
    x
  })
  
  dist_exact_timed = reactive({
    
    time_1 = Sys.time()
    
    if(input$method_1 == "exaustive"){
      dist = dice_density_exaustive(dice_vec_timed(),
                                    flat_bonus = input$flat_bonus,
                                    opposite_test = input$opposite_test,
                                    draw_behavior = 2)
      
    } else if(input$method_1 == "dist"){
      dist = dice_density_analytic(dice_vec_timed(),
                                   flat_bonus = input$flat_bonus,
                                   opposite_test = input$opposite_test,
                                   draw_behavior = 2)
      
    } else if(input$method_1 == "norm"){
      
      dist = dice_density_norm_approx(dice_vec_timed(),
                                      flat_bonus = input$flat_bonus,
                                      opposite_test = input$opposite_test,
                                      draw_behavior = 2)
      
    } else if(input$method_1 == "sim"){
      
      dist = dice_density_sim_approx(dice_vec_timed(),
                                     flat_bonus = input$flat_bonus,
                                     opposite_test = input$opposite_test,
                                     draw_behavior = 2)
      
    }
      
    attr(dist, "time") = Sys.time() - time_1
    
    dist
    
  })
  
  output$exaustive_time = renderText({
    paste0("Time: ", attr(dist_exact_timed(), "time") |> round(4), " secs")
  })
  
  dist_approx_timed = reactive({
    
    time_2 = Sys.time()
    
    if(input$method_2 == "exaustive"){
      dist = dice_density_exaustive(dice_vec_timed(),
                                    flat_bonus = input$flat_bonus,
                                    opposite_test = input$opposite_test,
                                    draw_behavior = 2)
      
    } else if(input$method_2 == "dist"){
      dist = dice_density_analytic(dice_vec_timed(),
                                   flat_bonus = input$flat_bonus,
                                   opposite_test = input$opposite_test,
                                   draw_behavior = 2)
      
    } else if(input$method_2 == "norm"){
      
      dist = dice_density_norm_approx(dice_vec_timed(),
                                      flat_bonus = input$flat_bonus,
                                      opposite_test = input$opposite_test,
                                      draw_behavior = 2)
      
    } else if(input$method_2 == "sim"){
      
      dist = dice_density_sim_approx(dice_vec_timed(),
                                     flat_bonus = input$flat_bonus,
                                     opposite_test = input$opposite_test,
                                     draw_behavior = 2)
      
    }
    
    attr(dist, "time") = Sys.time() - time_2
    
    dist
    
  })
  
  output$norm_approx_time = renderText({
    paste0("Time: ", attr(dist_approx_timed(), "time") |> round(4), " secs")
  })

  output$diff_plot = renderPlotly({
    diff_plot(dist_exact_timed(), dist_approx_timed())
  })
  
  output$approx_error = renderText({
    dens1 = dist_exact_timed() |> getElement("Density")
    dens2 = dist_approx_timed() |> getElement("Density")
    error = max(cumsum(dens1) - cumsum(dens2)) |> round(6)
    paste0("Error (K.S. Statistic) = ", error)
  })
  
}