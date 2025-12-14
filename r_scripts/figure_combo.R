library(tidyverse)
library(patchwork)
options(pillar.print_min = 35)

source("r_scripts/choice_plots.R")
source("r_scripts/latency_plots.R")

fig_combo <- choice_lc +
  plot_spacer() +
  lat_lc_3 +
  plot_layout(widths = c(4, .25, 4)) +
  plot_annotation(
    tag_levels = 'A'
  ) &
  theme(plot.tag = element_text(size = 18, face = "bold"))

fig_combo

ggsave(
  "outputs/plots/fig_2.svg",
  plot = fig_combo,
  units = "cm",
  width = 30,
  height = 18
)
ggsave(
  "outputs/plots/fig_2.pdf",
  plot = fig_combo,
  units = "cm",
  width = 30,
  height = 18
)
ggsave(
  "outputs/plots/fig_2.png",
  plot = fig_combo,
  units = "cm",
  width = 30,
  height = 18,
  dpi = 400
)
