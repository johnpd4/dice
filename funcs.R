dice_density_exaustive = function(dice_vec, # A vector with n numbers. Every number is consider a "1dn" dice
                                            # c(8, 8, 4) -> 2d8 + 1d4
                                  flat_bonus = 0, # Flat bonus
                                  per_dice_bonus = 0, # Flat bonus per dice rolled
                                  draw_behavior = 2, # 0 = Draw is a loss
                                                     # 1 = Draw is a win
                                                     # 2 = Draw is a draw
                                  opposite_test = 15 # Value of the opposite test
                                  ){
  
  if(!is.numeric(dice_vec) || length(dice_vec) < 1){return(data.frame("Value" = 1, "Density" = 1, "Result" = 1))}
  
  max_value = sum(dice_vec)
  number_of_dice = length(dice_vec)
  
  dice_list = list()
  
  for(i in 1:length(dice_vec)){
    
    dice_list[[i]] = 1:dice_vec[i]
    
  }
  
  dice_grid = expand.grid(dice_list)
  number_of_combinations = nrow(dice_grid)
  
  dice_grid$val = rowSums(dice_grid) + per_dice_bonus * number_of_dice + flat_bonus
  
  dens = data.frame()
  
  for(i in number_of_dice:max_value){
    
    dice_total = i + per_dice_bonus * number_of_dice + flat_bonus
    
    aux = c(i,
            dice_grid[dice_grid$val == i,] |> nrow() / number_of_combinations,
            ((dice_total > opposite_test) + (dice_total == opposite_test) * draw_behavior))
    
    dens = rbind(dens, aux)
    
  }
  
  names(dens) = c("Value", "Density", "Result")
  
  return(dens)
  
}

dice_density_norm_approx = function(dice_vec, # A vector with n numbers. Every number is consider a "1dn" dice
                                              # c(8, 8, 4) -> 2d8 + 1d4
                                    flat_bonus = 0, # Flat bonus
                                    per_dice_bonus = 0, # Flat bonus per dice rolled
                                    draw_behavior = 2, # 0 = Draw is a loss
                                                       # 1 = Draw is a win
                                                       # 2 = Draw is a draw
                                    opposite_test = 15 # Value of the opposite test
                                    ){
  
  if(!is.numeric(dice_vec) || length(dice_vec) < 1){return(data.frame("Value" = 1, "Density" = 1, "Result" = 1))}
  
  max_value = sum(dice_vec)
  number_of_dice = length(dice_vec)
  
  expected_value_dices = c()
  variance_dices = c()
  
  for(i in 1:length(dice_vec)){
    
    # expected_value_dices[i] = qunif(0.5, 1, dice_vec[i])
    # variance_dices[i] = sum((1:dice_vec[i] - expected_value_dices[i])^2) / dice_vec[i]
    
    expected_value_dices[i] = (dice_vec[i] + 1) / 2
    variance_dices[i] = sum(((1:dice_vec[i]) - expected_value_dices[i])^2) / dice_vec[i]
    
  }
  
  dice_means = sum(expected_value_dices)
  dice_vars = sum(variance_dices)
  
  dens = data.frame()
  
  for(i in number_of_dice:max_value){
    
    dice_total = i + per_dice_bonus * number_of_dice + flat_bonus
    
    aux = c(i,
            pnorm(i + 0.5, mean = dice_means, sd = sqrt(dice_vars)) - pnorm(i - 0.5, mean = dice_means, sd = sqrt(dice_vars)),
            ((dice_total > opposite_test) + (dice_total == opposite_test) * draw_behavior))
    
    dens = rbind(dens, aux)
    
  }
  
  names(dens) = c("Value", "Density", "Result")
  
  dens$Density = dens$Density / sum(dens$Density)
  
  return(dens)
  
}

dice_density_sim_approx = function(dice_vec, # A vector with n numbers. Every number is consider a "1dn" dice
                                             # c(8, 8, 4) -> 2d8 + 1d4
                                   flat_bonus = 0, # Flat bonus
                                   per_dice_bonus = 0, # Flat bonus per dice rolled
                                   draw_behavior = 2, # 0 = Draw is a loss
                                                      # 1 = Draw is a win
                                                      # 2 = Draw is a draw
                                   opposite_test = 15, # Value of the opposite test
                                   nsim = 3000
                                   ){
  
  max_value = sum(dice_vec)
  number_of_dice = length(dice_vec)
  
  sums = c()
  
  for(i in 1:nsim){
    
    values = c()
    
    for(j in 1:length(dice_vec)){
      
      values[j] = floor(runif(1, 0, dice_vec[j])) + 1
      
    }
    
    sums[i] = sum(values)
    
  }
  
  dens = data.frame()
  
  for(i in number_of_dice:max_value){
    
    dice_total = i + per_dice_bonus * number_of_dice + flat_bonus
    
    aux = c(i,
            sums[sums == i] |> length() / nsim,
            ((dice_total > opposite_test) + (dice_total == opposite_test) * draw_behavior))
    
    dens = rbind(dens, aux)
    
  }
  
  names(dens) = c("Value", "Density", "Result")
  
  return(dens)
  
}

dice_density_analytic = function(dice_vec, # A vector with n numbers. Every number is consider a "1dn" dice
                                           # c(8, 8, 4) -> 2d8 + 1d4
                                 flat_bonus = 0, # Flat bonus
                                 per_dice_bonus = 0, # Flat bonus per dice rolled
                                 draw_behavior = 2, # 0 = Draw is a loss
                                                    # 1 = Draw is a win
                                                    # 2 = Draw is a draw
                                 opposite_test = 15 # Value of the opposite test
                                 ){
    
  if(!is.numeric(dice_vec) || length(dice_vec) < 1){return(data.frame("Value" = 1, "Density" = 1, "Result" = 1))}
  
  sides = unique(dice_vec)
  number_of_dice = length(dice_vec)
  
  n = c()
  
  for(i in 1:length(sides)){
    
    n[i] = sum(dice_vec == sides[i])
    
  }
  
  p = 1
  
  for (j in seq_along(n)) {
    
    die = rep(1 / sides[j], sides[j])
    
    for (i in seq_len(n[j])) {
      p = convolve(p, rev(die), type = "open")
    }
  }
  
  N = sum(n)
  
  dens = data.frame(sum = N:sum(n * sides), prob = p)
  
  result = c()
  
  for(i in 1:nrow(dens)){
  
    dice_total = dens$sum[i] + per_dice_bonus * number_of_dice + flat_bonus
    
    result[i] = ((dice_total > opposite_test) + (dice_total == opposite_test) * draw_behavior)
    
  }
  
  dens = cbind(dens, result)
  
  names(dens) = c("Value", "Density", "Result")
  
  return(dens)
  
}

times = function(x, k){return(x * k)}

dice_histogram = function(obj, legend = TRUE){
  
  pass_col = "#2E8B57"
  fail_col = "#B22222"
  draw_col = "#E69F00"
  
  pass_perc = obj |> subset(Result == 1) |> select(Density) |> sum() |> times(100) |> round(2)
  fail_perc = obj |> subset(Result == 0) |> select(Density) |> sum() |> times(100) |> round(2)
  draw_perc = obj |> subset(Result == 2) |> select(Density) |> sum() |> times(100) |> round(2)
  
  fig = plot_ly(data = obj |> subset(Result == 1),
                x =~ Value, y =~ Density, marker = list(color = pass_col), name = paste0("Pass (", pass_perc, "%)"), type = "bar",
                hovertemplate = paste(" Value: %{x}<br>", "Density: %{y:.4f}<extra></extra>", "<br> Probability: %{y:.2%}"))
  fig = fig |> add_trace(inherit = F, data = obj |> subset(Result == 2),
                         x =~ Value, y =~ Density, marker = list(color = draw_col), name = paste0("Draw (", draw_perc, "%)"), type = "bar",
                         hovertemplate = paste(" Value: %{x}<br>", "Density: %{y:.4f}<extra></extra>", "<br> Probability: %{y:.2%}"))
  fig = fig |> add_trace(inherit = F, data = obj |> subset(Result == 0),
                         x =~ Value, y =~ Density, marker = list(color = fail_col), name = paste0("Fail (", fail_perc, "%)"), type = "bar",
                         hovertemplate = paste(" Value: %{x}<br>", "Density: %{y:.4f}<extra></extra>", "<br> Probability: %{y:.2%}"))
  
  if(legend){
    fig = fig |> layout(legend = list(orientation = "h", x = 0.5, xanchor = "center",
                                      y = 1.1, yanchor = "bottom", traceorder = "reversed", font = list(size = 18)))
  } else {
    fig = fig |> layout(showlegend = FALSE)
  }
  
  return(fig)
  
}

dice_numbers_to_vec = function(dices, numbers){
  
  if(length(dices) != length(numbers)) stop("Numbers and types of dices are not the same length")
  
  vec = c()
  
  for(i in 1:length(dices)){
    
    vec = c(vec, rep(dices[i], numbers[i]))
    
  }
  
  return(vec)
  
}

pve_test = function(advantages = 1, # How many advantages the player has
                    disavantage = FALSE, # Does the player has advantages or
                                         # disadvantages?
                    flat_bonus = 10, # Flat bonus the player has
                    opposite_test = 15, # Value to beat
                    draw_behavior = 2, # 0 = Draw is a loss
                                       # 1 = Draw is a win
                                       # 2 = Draw is a draw
                    critical = TRUE # If 1 or 20 should be auto win/loss
                    ){
  
  dens = data.frame()
  
  for(i in 1:20){
    
    if(!disavantage){
      chance = (i/20)^(advantages + 1) - ((i - 1)/20)^(advantages + 1)
    } else {
      chance = ((21 - i)/20)^(advantages + 1) - ((20 - i)/20)^(advantages + 1)
    }
    
    dice_total = i + flat_bonus
    
    dens = rbind(dens, c(i, chance, ((dice_total > opposite_test) + (dice_total == opposite_test) * draw_behavior)))
    
  }
  
  names(dens) = c("Value", "Density", "Result")
  
  if(critical){dens[dens$Value == 1, "Result"] = 0}
  if(critical){dens[dens$Value == 20, "Result"] = 1}
  
  return(dens)
  
}

pvp_test = function(advantages = c(0, 1), # How many advantages has p1 and p2 have
                    disavantage = c(FALSE, FALSE), # Do the p1 and p2 have advantages or
                                                   # disadvantages?
                    flat_bonus = c(12, 10), # Flat bonus both players have
                    draw_behavior = 2, # 0 = Draw is a p2 win
                                       # 1 = Draw is a p1 win
                                       # 2 = Draw is a draw
                    critical = TRUE # If 1 or 20 should be auto win/loss
                    ){
  
  dens1 = data.frame()
  dens2 = data.frame()
  
  for(i in 1:20){
    
    if(!disavantage[1]){
      chance1 = (i/20)^(advantages[1] + 1) - ((i - 1)/20)^(advantages[1] + 1)
    } else {
      chance1 = ((21 - i)/20)^(advantages[1] + 1) - ((20 - i)/20)^(advantages[1] + 1)
    }
    
    if(!disavantage[2]){
      chance2 = (i/20)^(advantages[2] + 1) - ((i - 1)/20)^(advantages[2] + 1)
    } else {
      chance2 = ((21 - i)/20)^(advantages[2] + 1) - ((20 - i)/20)^(advantages[2] + 1)
    }
    
    dens1 = rbind(dens1, c(i, chance1, i + flat_bonus[1]))
    dens2 = rbind(dens2, c(i, chance2, i + flat_bonus[2]))
    
  }
  
  names(dens1) = c("Value", "Density", "Total")
  names(dens2) = c("Value", "Density", "Total")
  
  outcome = expand.grid(p1 = seq_len(nrow(dens1)), p2 = seq_len(nrow(dens2)))
  
  outcome$P1_Roll = dens1$Value[outcome$p1]
  outcome$P2_Roll = dens2$Value[outcome$p2]
  
  outcome$P1_Total = dens1$Total[outcome$p1]
  outcome$P2_Total = dens2$Total[outcome$p2]
  
  outcome$Density = dens1$Density[outcome$p1] * dens2$Density[outcome$p2]
  
  outcome$Result = -1
  
  for(i in 1:nrow(outcome)){
    
    outcome$Result[i] = (outcome$P1_Total[i] > outcome$P2_Total[i]) + (outcome$P1_Total[i] == outcome$P2_Total[i]) * draw_behavior
    
  }
  
  # # Mudar isso pra incorporar draw behavior
  # outcome$Result = factor(outcome$Result,
  #                         levels = c(0, 1, 2),
  #                         labels = c("P2 Win", "P1 Win", "Draw"))
  
  for(i in 1:nrow(outcome)){
    
    if(critical == FALSE){break}
    
    if(outcome$P1_Roll[i] == 20){
      if(outcome$P2_Roll[i] == 20){outcome$Result[i] = draw_behavior} else {outcome$Result[i] = 1}
    }
    
    if(outcome$P2_Roll[i] == 20){
      if(outcome$P1_Roll[i] == 20){outcome$Result[i] = draw_behavior} else {outcome$Result[i] = 0}
    }
    
    if(outcome$P1_Roll[i] == 1){
      if(outcome$P2_Roll[i] == 1){outcome$Result[i] = draw_behavior} else {outcome$Result[i] = 0}
    }
    
    if(outcome$P2_Roll[i] == 1){
      if(outcome$P1_Roll[i] == 1){outcome$Result[i] = draw_behavior} else {outcome$Result[i] = 1}
    }
    
  }
  
  return(outcome)
  
}

pvp_matrix = function(outcome){
  
  outcome$Result <- factor(
    outcome$Result,
    levels = c(0, 1, 2),
    labels = c("P2 Win", "P1 Win", "Draw")
  )
  
  outcome$Alpha = scales::rescale(outcome$Density, to = c(0.15, 1))
  
  heatmap = ggplot(outcome, aes(P2_Roll, P1_Roll)) +
              geom_tile(aes(fill = Result, alpha = Alpha),color = "white", linewidth = 0.3) +
              scale_fill_manual(values = c("P2 Win" = "#B22222", "Draw"   = "#E69F00", "P1 Win" = "#2255B2")) +
              scale_alpha_identity() +
              scale_x_continuous(breaks = 1:20, expand = c(0, 0)) +
              scale_y_continuous(breaks = 1:20, expand = c(0, 0)) +
              coord_equal() +
              labs(x = "Player 2 Roll", y = "Player 1 Roll", fill = NULL,
                   caption = "Tile opacity is proportional to the joint probability \n of the corresponding pair of rolls.") +
              theme_minimal(base_size = 13) +
              theme(panel.grid = element_blank(), legend.position = "top",
                    plot.caption = element_text(hjust = 0.5, size = 10, colour = "grey40", margin = margin(t = 10)))
  
  return(heatmap)
  
}

diff_plot = function(dens1, dens2){
  
  pass_col_1 = "#2E8B57"
  fail_col_1 = "#B22222"
  draw_col_1 = "#E69F00"
  
  pass_col_2 = "#00B894"
  fail_col_2 = "#FF4D4D"
  draw_col_2 = "#FDCB6E"
  
  pass_perc_1 = dens1 |> subset(Result == 1) |> select(Density) |> sum() |> times(100) |> round(2)
  
  pass_perc_2 = dens2 |> subset(Result == 1) |> select(Density) |> sum() |> times(100) |> round(2)
  
  diff = dens1
  diff$Density = dens1$Density - dens2$Density
  
  fig = plot_ly(data = dens1 |> subset(Result == 1),
                x =~ Value, y =~ Density, marker = list(color = pass_col_1),
                name = paste0("Pass Exact (", pass_perc_1, "%)"), type = "bar",
                hovertemplate = paste(" Value: %{x}<br>", "Density: %{y:.4f}<extra></extra>", "<br> Probability: %{y:.2%}"))
  fig = fig |> add_trace(data = dens2 |> subset(Result == 1), opacity = 0.5,
                         x =~ Value, y =~ Density, marker = list(color = pass_col_2),
                         name = paste0("Pass Approx. (", pass_perc_2, "%)"), type = "bar",
                         hovertemplate = paste(" Value: %{x}<br>", "Density: %{y:.4f}<extra></extra>", "<br> Probability: %{y:.2%}"))
  fig = fig |> add_trace(inherit = F, data = diff,
                         x =~ Value, y =~ Density, marker = list(color = "#4169E1"),
                         name = paste0("Exact - Approx."), type = "bar",
                         hovertemplate = paste(" Value: %{x}<br>", "Diff: %{y:.4f}<extra></extra>", "<br> Percent Diff.: %{y:.2%}"))
  
  fig = fig |> layout(barmode = "overlay")
  fig = fig |> layout(legend = list(orientation = "h", x = 0.5, xanchor = "center",
                                    y = 1.1, yanchor = "bottom", traceorder = "reversed", font = list(size = 18)))
  
  fig
  
}

teste1 = dice_density_exaustive(c(6, 6, 6))
teste2 = dice_density_norm_approx(c(6, 6, 6))

diff_plot(teste1, teste2)
