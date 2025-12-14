library(tidyverse)
options(pillar.print_min = 35)

# Load Data
data <- read_csv("data/choice_data.csv") |>
  select(
    subject,
    session,
    phase_session,
    phase,
    condition,
    order,
    ch_il_sig_resp,
    ch_il_unsig_resp
  ) |>
  mutate(
    subject = factor(subject),
    condition = factor(condition, levels = c("Short", "Long")),
    phase = factor(phase, labels = c("Phase 1", "Phase 2")),
    order = factor(
      order,
      levels = c("S-L", "L-S"),
      labels = c("Short-Long", "Long-Short")
    ),
    cp = ch_il_sig_resp / (ch_il_sig_resp + ch_il_unsig_resp)
  )

# Plotting Variables
#-------------------------------------------------------------------------------
pal <- palette.colors(palette = "Polychrome 36")[-c(2)]
y_lab <- "Preference for Signalled Alternative"

# Plot: By Condition
#-------------------------------------------------------------------------------
choice_lc <- ggplot(data, aes(x = phase_session, y = cp)) +
  geom_hline(yintercept = 0.5, linetype = 3) +
  geom_line(
    aes(group = subject),
    alpha = 0.25
  ) +
  # geom_hline(aes(yintercept = SiGN_pred), linetype = 3) +
  stat_summary(
    geom = "line",
    fun = mean,
    #linewidth = 1
  ) +
  stat_summary(
    geom = "point",
    fun = mean,
    size = 1
  ) +
  facet_wrap(~condition, ncol = 1) +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  # scale_colour_manual(values = pal, guide = "none") +
  labs(
    x = "Session",
    y = y_lab
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12, colour = "black"),
    axis.title = element_text(size = 14, colour = "black"),
    strip.text = element_text(size = 14, colour = "black"),
    strip.background = element_blank(),
    panel.spacing = unit(0.75, "lines"),
    panel.border = element_rect(colour = "black", fill = NA),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )

choice_lc

# ggsave("outputs/plots/choice_lc.svg", units = "cm", width = 13, height = 11)
# ggsave(
#   "outputs/plots/choice_lc.png",
#   units = "cm",
#   width = 13,
#   height = 11,
#   dpi = 300
# )

# Plot: By Condition and Phase
#-------------------------------------------------------------------------------
choice_lc_2 <- ggplot(data, aes(x = session, y = cp)) +
  geom_line(
    aes(group = subject, colour = condition),
    linewidth = 0.25
  ) +
  stat_summary(
    geom = "line",
    fun = mean,
    aes(colour = condition),
    linewidth = 0.75
  ) +
  stat_summary(
    geom = "point",
    fun = mean,
    aes(shape = condition, colour = condition),
    size = 2
  ) +
  scale_color_manual(values = pal) +
  facet_grid(order ~ phase, scales = "free_x") +
  scale_x_continuous(breaks = seq(0, 60, 5)) +
  labs(
    x = "Session",
    y = y_lab,
    colour = "Condition:",
    shape = "Condition:"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12, colour = "black"),
    axis.title = element_text(size = 14, colour = "black"),
    strip.text = element_text(size = 14, colour = "black"),
    strip.background = element_blank(),
    panel.spacing = unit(0.75, "lines"),
    panel.border = element_rect(colour = "black", fill = NA),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )

choice_lc_2
