# Example for `file_outline()` -------------
# Load packages -----
library(ggplot2)

# Wrangle + visualize data --------
ggplot(mtcars, aes(x = vs, y = mpg)) +
  geom_point() +
  labs(
    title = "A great title"
  )
# TODO improve this Vizual!

# file_outline() is a workaround for rstudio/rstudioapi#153
f_example <- function(x) {
  x ** 2
}
if (FALSE) {
  f2_example <- function(x) {
    x ** 2
  }
  # f_commented_example <- function(x) {
  #   x ** 2
  # }
}
