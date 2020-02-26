
plotly_donut<- function(df, xvalue, yvalue, hole, ...){
  p <- df %>%
    plot_ly(labels = df[[xvalue]], values = df[[yvalue]]) %>%
    add_pie(hole = hole) %>%
    layout(showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  p
}
