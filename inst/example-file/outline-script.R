# Load packages -----
library(ggplot2)

# Wrangle + visualize data ------
mtcars |>
  ggplot(aes(x = vs, y = mpg)) +
  geom_point() +
  labs(
    title = "A great title"
  )
# TODO improve this Viz!
