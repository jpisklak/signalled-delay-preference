library(tidyverse)
library(WRS2)
options(
  pillar.print_min = 35,
  pillar.sigfig = 2
)

# Install/Load SiGN Function
# install.packages("pak")
# pak::pak("https://github.com/SiGN-R/SiGN")
library(SiGN)

# Load and organize data
data <- read_csv("data/harmonized_studies_supplement.csv") |>
  mutate(study = paste0(study, " (", year, ")"))
# View(data)

params <- do.call(choice_params, as.list(data[8:23]))

# Generate model predictions

preds <- SiGN(params)

# Descriptive stats
stats <- choice_mod_eval(observed = data$cp, predicted = preds$cp)
stats$desc_stats

# Save output
sink("outputs/supplemental_stats.txt")
cat("=== Descriptive Stats for Model Fit ===\n")
round(stats$desc_stats, 2)
sink()


# Plot
#-------------------------------------------------------------------------------
# Linear model observed ~ predicted
reg <- lm(data$cp ~ preds$cp)

# Equation annotation
reg_txt <- sprintf(
  "y = %.2f + %.2f x\n",
  reg$coefficients[1],
  reg$coefficients[2]
)

ggplot(data, aes(x = preds$cp, y = cp)) +
  geom_abline(intercept = 0, slope = 1, linetype = 3) +
  geom_point(size = 3, stroke = 0.75, aes(shape = study, fill = study)) +
  scale_shape_manual(values = 21:24) +
  scale_fill_manual(values = palette.colors(palette = "Dark 2")) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(xlim = c(0.5, 1), ylim = c(0.5, 1)) +
  labs(x = "Predicted", y = "Obtained", shape = "Study", fill = "Study") +
  annotate("text", x = 0.65, y = 0.95, label = reg_txt, size = 5) +
  theme_bw(base_size = 12) +
  theme(
    axis.text = element_text(size = 12, colour = "black"),
    axis.title = element_text(size = 14, colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA),
    # legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )


ggsave(
  "outputs/plots/fig_supp.svg",
  units = "cm",
  width = 18,
  height = 10
)

ggsave(
  "outputs/plots/fig_supp.pdf",
  units = "cm",
  width = 18,
  height = 10
)

ggsave(
  "outputs/plots/fig_supp.png",
  units = "cm",
  width = 18,
  height = 10,
  dpi = 300
)
