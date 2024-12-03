# AGU figures

#1- Plot pollutants data
library(ggplot2)


plot_pesticide_ridges <- function(data, group_var) {
 
  df <- as.data.frame(data)
 
  sample_size <- df |>
    group_by({{group_var}}) |>
    summarize(num = n(), bd = sum(left_censored)/length(left_censored) * 100)
 
  p <- df |>
    left_join(sample_size) |>
    mutate(myaxis = paste0({{group_var}}, "\n", "n=", num," ","percentBD=",round(bd,1))) |> #nolint
    group_by({{group_var}}) |>
    ggplot() +
    geom_density_ridges(aes(x = concentration,y = as.factor(myaxis), fill = as.factor(left_censored))) + #nolint
    scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
    scale_fill_viridis_d(option = "D") +
    labs(y = "Chemical",x = "Concentration (ug/L)",fill = "Censored")
 
  return(p)
 
}