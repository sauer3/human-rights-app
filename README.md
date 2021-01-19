# Mapping Human Rights Cases

This repo contains the scripts to scrape human rights cases from the European court of human rights and then visualize them with an interactive shiny app. 

This repo is meant for updating new cases for an already existing app. 

Some data is not provided due to space constraints. See readme file in data folder. 

[Check out the app](https://auerdatascience.shinyapps.io/shiny_app/)

```{r, echo = FALSE}
shinyAppDir(
  system.file("/Users/AuerPower/Dropbox/Research Projects/Court_of_Human_Rights/Shiny_app", package="shiny"),
  options = list(width = "100%", height = 700)
)
```