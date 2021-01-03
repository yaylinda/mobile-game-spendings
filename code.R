setwd("~/Developer/mobile-game-spendings")

library(ggplot2)
library(ggthemes)
library(reshape2)

raw = read.csv("raw.csv")
raw$date = as.Date(raw$date, format = "%m/%d/%y")
raw$yearmon = format(raw$date, "%Y-%m")
raw$quarter = paste(format(raw$date, "%Y"), quarters(raw$date), sep = " ")
games = raw[which(raw$is_game == TRUE), ]

#==========================================================
# Aggregate spending per game
#==========================================================

# Spending per game
spendings_per_game = aggregate(
  games$price,
  by = list(
    game = games$game
  ),
  sum
)

# Order by spent amount
spendings_per_game = spendings_per_game[order(-spendings_per_game$x), ]

# Get list of top N games
NUMBER_OF_GAMES = 7
top_games = spendings_per_game$game[1:NUMBER_OF_GAMES]
top_games_with_other = c(top_games, "Other")

# Label games not in top N
games$label = ifelse(
  games$game %in% top_games,
  games$game,
  "Other"
)

# Re-aggregate spending for each label, for each month
games = aggregate(
  games$price,
  by = list(
    label = games$label,
    yearmon = games$yearmon,
    quarter = games$quarter
  ),
  sum
)

# Total spending per label
spendings_per_label = aggregate(
  games$x,
  by = list(
    label = games$label
  ),
  sum
)
games_order = c(unique(games$label)[2:length(unique(games$label))], "Other")
spendings_per_label = spendings_per_label[match(games_order, spendings_per_label$label),]

#==========================================================
# Spending on games monthly
#==========================================================

#--------------------------------------
# Prepare data
#--------------------------------------

# Create DF containing each month from 2014 - 2020
all_months = data.frame(
  date = seq(
    as.Date("2014-04-01"), 
    as.Date("2020-12-31"), 
    by = "1 month"
  )
)
all_months$yearmon = format(all_months$date, "%Y-%m")

# Create DF containing monthly data for each month, for each game
monthly_game_spendings = data.frame()
monthly_game_spendings[["yearmon"]] = as.character()
for (label in top_games_with_other) monthly_game_spendings[[label]] = as.numeric()

# Loop through raw data to fill in DF
for (yearmon in all_months$yearmon) {
  
  game_price_list = c()
  
  for (game_label in top_games_with_other) {
    game_price_result = games[which(games$yearmon == yearmon), ]
    
    game_price = 0
    if (game_label %in% game_price_result$label) {
      game_price = game_price_result[which(game_price_result$label == game_label),]$x
    }
    
    game_price_list = c(game_price_list, game_price)
  }
  
  new_row = data.frame()
  new_row[["yearmon"]] = as.character()
  for (label in top_games_with_other) new_row[[label]] = as.numeric()
  
  max_col_index = length(top_games_with_other) + 1
  new_row[1, 1] = yearmon
  new_row[1, 2:max_col_index] = game_price_list
  
  monthly_game_spendings = rbind(monthly_game_spendings, new_row)
}

# Cumulative monthly game spending
monthly_cumulative_game_spendings = monthly_game_spendings
monthly_cumulative_game_spendings[top_games_with_other] = lapply(
  monthly_cumulative_game_spendings[top_games_with_other], 
  function(x) cumsum(x)
)

#--------------------------------------
# Melt and plot data
#--------------------------------------

melted = melt(monthly_game_spendings, id.vars = "yearmon")
melted_cumulative = melt(monthly_cumulative_game_spendings, id.vars = "yearmon")

# Cumulative plot
ggplot(
  data = melted_cumulative, 
  aes(
    x = yearmon,
    y = value
  )
) +
  geom_area(
    aes(
      color = variable, 
      fill = variable,
      group = variable
    ), 
    color = "white",
    position = "stack",
    size = 1.5
  ) +
  scale_x_discrete(
    breaks = all_months$yearmon[seq(1, length(all_months$date), 4)]
  )

#==========================================================
# Spending on games quarterly
#==========================================================

# Quarterly spending per game
quarter_game_spending = aggregate(
  games$x,
  by = list(
    label = games$label,
    quarter = games$quarter
  ),
  sum
)

names(quarter_game_spending) = c("Game", "Quarter", "Price")

quarter_game_spending$Game = factor(
  quarter_game_spending$Game, 
  levels = games_order
)

avg_quarter_spending = round(mean(quarter_game_spending$Price), digits = 2)
avg_quarter_spending_label = paste("avg / qtr:\n$", avg_quarter_spending, sep = "")

ggplot(
  data = quarter_game_spending, 
  aes(
    x = Quarter,
    y = Price
  )
) +
  geom_col(
    aes(
      color = Game, 
      fill = Game,
      group = Game
    ), 
    color = "white",
    position = "stack",
    size = 1.5
  ) +
  geom_hline(
    yintercept = avg_quarter_spending,
    linetype = "dashed",
    size = 1.5,
    color = "darkgray"
  ) +
  geom_label(
    data = data.frame(
      x = "2014 Q1",
      y = avg_quarter_spending
    ),
    aes(x, y),
    label = avg_quarter_spending_label,
    vjust = 0.5,
    hjust = 0,
    family = "mono",
    size = 5,
    position = position_nudge(x = -1, y = 20)
  ) +
  labs(
    x = "Quarter",
    y = "Amount Spent (USD)",
    title = "Mobile Game Spendings 2014-2020",
    subtitle = paste("Total Spent: $", round(sum(games$x), digits = 2), sep = ""),
    fill = "",
    caption = ""
  ) +
  scale_fill_hue(
    labels = paste(spendings_per_label$label, "\n$", round(spendings_per_label$x, digits = 2), sep = "")
  ) +
  theme_economist() +
  theme(
    text = element_text(family = "mono"),
    plot.title = element_text(size = rel(3)),
    plot.subtitle = element_text(size = rel(1.5), margin = margin(t = 10, b = 20), hjust = 0),
    plot.margin = margin(t = 20, b=10, l = 20, r = 20),
    axis.text.x = element_text(size = rel(1.5), angle = 90),
    axis.text.y = element_text(size = rel(1.5)),
    axis.title.x = element_text(size = rel(1.5), margin = margin(t = 20), face = "bold"),
    axis.title.y = element_text(size = rel(1.5), margin = margin(r = 20), face = "bold"),
    legend.position = "right",
    legend.text = element_text(size = rel(1.5))
  )




