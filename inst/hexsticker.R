
library(ggplot2)
library(hexSticker)

# Create ordinal simulation data
set.seed(123)
df <- data.frame(
  category = factor(rep(1:5, each = 20), ordered = TRUE),
  x = runif(100),
  y = rep(1:5, each = 20) + rnorm(100, sd = 0.1)
)

# Base plot for sticker
p <- ggplot(df, aes(x = x, y = category, color = category)) +
  geom_jitter(height = 0.2, width = 0.02, size = 1.8, alpha = 0.8) +
  scale_color_brewer(palette = "YlOrRd", direction = 1) +
  theme_void() +
  theme(legend.position = "none")

# Create the hex sticker
sticker(
  p,
  package = "ordinalsimr",
  p_size = 8,
  s_x = 1,
  s_y = 1,
  s_width = 1.3,
  s_height = 1.3,
  h_fill = "#000000",
  h_color = "#c44e00",
  filename = "inst/app/www/favicon.png"
)



golem::use_favicon("inst/app/www/favicon.png")
