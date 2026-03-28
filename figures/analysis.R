library(ggplot2)
# data
popper_data <- data.frame(
  Type = c("Happy", "Sad"),
  Mean = c(1.72, 1.55),
  SD   = c(0.26, 0.16)
)


# plotting and formatting
ggplot(popper_data, aes(x = Type, y = Mean, fill = Type)) +
  geom_col(color = "black", linewidth = 0.4) +   # bars
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.15, linewidth = 0.7) +
  scale_fill_manual(values = c("Happy" = "#4472C4", "Sad" = "#C0504D")) +
  scale_y_continuous(limits = c(0, 2.2), breaks = seq(0, 2, 0.5)) +
  labs(
    title = "Popper Type vs. Max Height",
    x = "Popper Type",
    y = "Max Height (m)",
    caption = "Error bars = ±1 SD (n = 5 per group)"
  ) +
  theme_classic(base_size = 13, base_family = "serif") + 	theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", family = "serif"),
    axis.title = element_text(family = "serif"),
    axis.text = element_text(family = "serif"),
    plot.caption = element_text(hjust = 0.5, size = 10, color = "gray40", family = "serif"),
    panel.grid.major.y = element_line(color = "gray88", linewidth = 0.4)
  )


# saving graph as svg
ggsave("myplot.svg", width = 3.4167, height = 2, dpi = 300)


