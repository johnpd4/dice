source("funcs.R")
addResourcePath("figs", "figs")

opposite_test_ui = function(){
    
  nav_panel(
    
    title = "Test Simulation",
    
    layout_sidebar(
      
      sidebar = sidebar(
        open = "always",
        width = "20%",
        
        div(
          style = "display: flex; align-items: center;",
          
          uiOutput("pvp_img"),
          
          radioButtons("pvp",
                       label = "PvE or PvP",
                       choices = c("Character vs enviroment" = FALSE, "Two characters roll dice" = TRUE),
                       selected = c("Character vs enviroment" = FALSE)
          ), # numeric input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/marked.png", width = "80px", style = "margin-top: 8px;"),
          
          radioButtons("critical",
                       label = "Take critical rolls into account (1 and 20)",
                       choices = c("Yes" = TRUE, "No" = FALSE),
                       selected = c("Yes" = TRUE)
          ), # numeric input
          
        ), # div
        
        conditionalPanel(
          condition = "input.pvp == 'FALSE'",
        
          div(
            style = "display: flex; align-items: center;",
            
            tags$img(src = "figs/dead_eye.png", width = "80px", style = "margin-top: 8px;"),
            
            numericInput("opposite_test",
                         label = "Opposite Test Value",
                         value = 0,
                         min = 0,
                         max = 999
            ), # numeric input
            
          ), # div
        
        ), # conditional panel
        
        div(
          style = "display: flex; align-items: center;",
          
          uiOutput("p1_flat_bonus_img"),
          
          numericInput("p1_flat_bonus",
                       label = textOutput("p1_flat_bonus_text"),
                       value = 0,
                       min = 0,
                       max = 50
          ), # numeric input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          uiOutput("p1_advantage_img"),
          
          numericInput("p1_advantage",
                       label = textOutput("p1_advantage_text"),
                       value = 0,
                       min = 0,
                       max = 20
          ), # numeric input
          
        ), # div
        
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/spindown_dice.png", width = "80px", style = "margin-top: 8px;"),
          
          radioButtons("p1_disadvantage",
                       label = textOutput("p1_disadvantage_text"),
                       choices = c("Advantage" = FALSE, "Disadvantage" = TRUE),
                       selected = c("Advantage" = FALSE)
          ), # numeric input
          
        ), # div
        
        conditionalPanel(
          condition = "input.pvp == 'TRUE'",
          
          div(
            style = "display: flex; align-items: center;",
            
            tags$img(src = "figs/p2_d1.png", width = "80px", style = "margin-top: 8px;"),
            
            numericInput("p2_flat_bonus",
                         label = "P2 Flat Bonus",
                         value = 0,
                         min = 0,
                         max = 50
            ), # numeric input
            
          ), # div
          
        ), # conditional panel
        
        conditionalPanel(
          condition = "input.pvp == 'TRUE'",
          
          div(
            style = "display: flex; align-items: center;",
            
            tags$img(src = "figs/p2_d20.png", width = "80px", style = "margin-top: 8px;"),
            
            numericInput("p2_advantage",
                         label = "P2 Advantage",
                         value = 0,
                         min = 0,
                         max = 20
            ), # numeric input
            
          ), # div
          
        ), # conditional panel
        
        conditionalPanel(
          condition = "input.pvp == 'TRUE'",
        
          div(
            style = "display: flex; align-items: center;",
            
            tags$img(src = "figs/spindown_dice.png", width = "80px", style = "margin-top: 8px;"),
            
            radioButtons("p2_disadvantage",
                         label = "P2 has Advantage or Disadvantage?",
                         choices = c("Advantage" = FALSE, "Disadvantage" = TRUE),
                         selected = c("Advantage" = FALSE)
            ), # numeric input
            
          ), # div
        
      ), # conditional panel
      
        div(
          style = "display: flex; align-items: center;",
          
          tags$img(src = "figs/equality.png", width = "80px", style = "margin-top: 8px;"),
          
          uiOutput("draw_button"),
          
        ), # div
        
      ), # sidebar
      
      uiOutput("main_plot")
      
    ) # layout sidebar
    
  ) # nav panel
  
} # ui

opposite_test_server = function(input, output, session){
  
  output$pvp_img = renderUI({
    if(as.logical(input$pvp) == TRUE){tags$img(src = "figs/betrayal.png", width = "80px", style = "margin-top: 8px;")}
    else{tags$img(src = "figs/trisagion.png", width = "80px", style = "margin-top: 8px;")}
  })
  
  output$draw_button = renderUI({
    if(as.logical(input$pvp) == TRUE){txt_1 = "P1 win"; txt_2 = "P2 win"}
    if(as.logical(input$pvp) == FALSE){txt_1 = "Count as win"; txt_2 = "Count as fail"}
    
    choices = c("1" = 1,
                "Keep as draw" = 2,
                "0" = 0)
    
    names(choices) = c(txt_1, "Keep as draw", txt_2)
    
    radioButtons("draw_behavior",
                 label = "What to do with a draw",
                 choices = choices,
                 selected = c("Keep as draw" = 2))
  })
  
  output$p1_flat_bonus_text = renderText({
    if(as.logical(input$pvp) == TRUE){txt = paste0("P1 Flat Bonus")}
    if(as.logical(input$pvp) == FALSE){txt = paste0("Flat Bonus")}
    txt
  })
  
  output$p1_flat_bonus_img = renderUI({
    if(as.logical(input$pvp) == TRUE){tags$img(src = "figs/p1_d1.png", width = "80px", style = "margin-top: 8px;")}
    else{tags$img(src = "figs/d1.png", width = "80px", style = "margin-top: 8px;")}
  })
  
  output$p1_advantage_text = renderText({
    if(as.logical(input$pvp) == TRUE){txt = paste0("P1 Advantage")}
    if(as.logical(input$pvp) == FALSE){txt = paste0("Player Advantage")}
    txt
  })
  
  output$p1_disadvantage_text = renderText({
    if(as.logical(input$pvp) == TRUE){txt = paste0("P1 has Advantage or Disadvantage?")}
    if(as.logical(input$pvp) == FALSE){txt = paste0("Player has Advantage or Disadvantage?")}
    txt
  })
  
  output$p1_advantage_img = renderUI({
    if(as.logical(input$pvp) == TRUE){tags$img(src = "figs/p1_d20.png", width = "80px", style = "margin-top: 8px;")}
    else{tags$img(src = "figs/d20.png", width = "80px", style = "margin-top: 8px;")}
  })
  
  dice_dist = reactive({

    if (isTRUE(as.logical(input$pvp))) {
      return(pvp_test(advantages = c(input$p1_advantage, input$p2_advantage),
                      disavantage = c(as.logical(input$p1_disadvantage), as.logical(input$p2_disadvantage)),
                      flat_bonus = c(input$p1_flat_bonus, input$p2_flat_bonus), draw_behavior = as.numeric(input$draw_behavior),
                      critical = as.logical(input$critical)))
    }
    return(
      pve_test(advantages = input$p1_advantage,
               disavantage = as.logical(input$p1_disadvantage),
               flat_bonus = input$p1_flat_bonus,
               opposite_test = input$opposite_test,
               draw_behavior = as.numeric(input$draw_behavior),
               critical = as.logical(input$critical)))
})
  
  output$dice_histogram = renderPlotly({
    if(as.logical(input$pvp) == FALSE){dice_histogram(dice_dist(), legend = FALSE)}
  })
  
  output$pvp_matrix = renderPlot({
    if(as.logical(input$pvp) == TRUE){pvp_matrix(dice_dist())}
  }, res = 150)
  
  output$c1_text = renderText({
    if(as.logical(input$pvp) == TRUE){txt = "P2 Chances of Winning"}
    if(as.logical(input$pvp) == FALSE){txt = "Percentage of Failing"}
    txt
  })
  
  # output$c1_value = renderText({
  #   paste0(sum(dice_dist() |> subset(Result == 0) |> select(Density)) |> times(100) |> round(3), "%")
  # })
  
  output$c1_value <- renderText({
    df <- dice_dist()
    paste0(round(100 * sum(df$Density[df$Result == 0]), 3), "%")
  })
  
  output$c2_value <- renderText({
    df <- dice_dist()
    paste0(round(100 * sum(df$Density[df$Result == 1]), 3), "%")
  })
  
  output$c2_text = renderText({
    if(as.logical(input$pvp) == TRUE){txt = "P1 Chances of Winning"}
    if(as.logical(input$pvp) == FALSE){txt = "Percentage of Passing"}
    txt
  })
  
  # output$c2_value = renderText({
  #   paste0(sum(dice_dist() |> subset(Result == 1) |> select(Density)) |> times(100) |> round(3), "%")
  # })
  
  output$main_plot = renderUI({
    
    if(as.logical(input$pvp) == FALSE){
      card(
        style = "border: none;",
        fill = TRUE,
        height = "100%",
        
        layout_columns(
          
          value_box(
            title = textOutput("c1_text"),
            value = textOutput("c1_value"),
            style = "background-color: #B22222; color: white;"
          ),
          
          value_box(
            title = textOutput("c2_text"),
            value = textOutput("c2_value"),
            style = "background-color: #2E8B57; color: white;"
          )
          
        ), # layout columns
        
        card_body(
          fill = TRUE,
          plotlyOutput("dice_histogram")
        )
        
      ) # card
      
    } else if(as.logical(input$pvp) == TRUE){
    
      card(
        style = "border: none;",
        fill = TRUE,
        height = "100%",
        
        
        layout_columns(
        
          card(
            style = "border: none;",
          
            value_box(
              title = textOutput("c1_text"),
              value = textOutput("c1_value"),
              style = "background-color: #B22222; color: white;"
            ),
            
            value_box(
              title = textOutput("c2_text"),
              value = textOutput("c2_value"),
              style = "background-color: #2255B2; color: white;"
            ),
          
          ),
            
          card_body(
            fill = TRUE,
            plotOutput("pvp_matrix")
          )
          
        ), # layout columns
        
      ) # card
      
    }
    
  })
  
}