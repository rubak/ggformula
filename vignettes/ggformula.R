## ----include = FALSE-----------------------------------------------------
library(ggformula)
library(dplyr)
library(ggplot2)
library(mosaic)

## ------------------------------------------------------------------------
library(ggformula)
gf_point(mpg ~ hp, data = mtcars)

## ------------------------------------------------------------------------
gf_point(mpg ~ hp + color:cyl + size:carb + alpha:0.75, data = mtcars) +
  ggplot2::ylab("Miles per gallon") +
  ggplot2::xlab("Horsepower")

## ------------------------------------------------------------------------
gf_point(mpg ~ hp + color:cyl, data = mtcars)
gf_point(mpg ~ hp + color:cyl + size:carb, alpha=0.75, data = mtcars, verbose = TRUE)
gf_point(mpg ~ hp + color:cyl + size:carb + alpha:0.75, data = mtcars)

