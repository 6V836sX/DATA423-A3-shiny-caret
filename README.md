# DATA423 Assignment 3 — Shiny + caret Regression Model Selection

This repo hosts a runnable Shiny app (`app/`) plus notebooks and docs for DATA423 A3 (regression model selection with caret + recipes).

## Quick start
1. Place `Ass3Data.csv` into `data/raw/`.
2. In R:
```r
install.packages("renv"); renv::init()
install.packages(c("shiny","caret","recipes","rsample","glmnet","rpart","rpart.plot","ggplot2"))
shiny::runApp("app", launch.browser = TRUE)
