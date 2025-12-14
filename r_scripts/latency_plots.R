library(tidyverse)
options(
  pillar.print_min = 35,
  pillar.sigfig = 2
)

s_l <- c(6163, 6159, 100)
l_s <- c(13, 1269, 822)

# Load Data
data <- read_csv("data/latency_data.csv") |>
  pivot_longer(
    cols = c(p1_1:last_col()),
    names_to = "phase_session",
    values_to = "latency_ms"
  ) |>
  mutate(
    #subject = factor(subject),
    session = sub(".*_", "", phase_session),
    session = as.numeric(session),
    phase = sub("_.*", "", phase_session),
    phase = factor(phase, labels = c("Phase 1", "Phase 2")),
    order = case_when(
      subject %in% s_l ~ "Short-Long",
      subject %in% l_s ~ "Long-Short"
    ),
    order = factor(order, levels = c("Short-Long", "Long-Short")),
    condition = case_when(
      order == "Long-Short" & phase == "Phase 1" ~ "Long",
      order == "Long-Short" & phase == "Phase 2" ~ "Short",
      order == "Short-Long" & phase == "Phase 1" ~ "Short",
      order == "Short-Long" & phase == "Phase 2" ~ "Long",
    ),
    condition = factor(condition, levels = c("Short", "Long")),
  ) |>
  select(-c(phase_session))

sess_data <- data |>
  group_by(subject, phase, order, condition, session, il_fun) |>
  summarise(
    m_tr = mean(latency_ms, tr = 0.2, na.rm = TRUE)
  )

# Plot Variables and functions
#-------------------------------------------------------------------------------
pal <- palette.colors(palette = "Dark 2")[-c(1)]
y_lab <- "20% Trimmed Mean Initial-Link\nLatency (ms)"

trimmed_mean <- function(x, tr = 0.2) {
  mean(x, trim = tr)
}

#Plot: By Condition and Subject and IL
#-------------------------------------------------------------------------------
# By Condition and Subject and Phase
lat_lc_1 <- ggplot(sess_data, aes(x = session, y = m_tr)) +
  geom_line(aes(colour = il_fun, linetype = il_fun)) +
  facet_grid(subject ~ condition) +
  labs(
    x = "Session",
    y = y_lab
  ) +
  scale_colour_manual(values = pal) +
  # scale_y_continuous(breaks = seq(0, 3, 0.5)) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12, colour = "black"),
    axis.title = element_text(size = 14, colour = "black"),
    strip.text = element_text(size = 10, colour = "black"),
    panel.spacing = unit(0.75, "lines"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )

#Plot: By Condition and Phase and IL
#-------------------------------------------------------------------------------
lat_lc_2 <- ggplot(sess_data, aes(x = session, y = m_tr)) +
  stat_summary(
    geom = "line",
    fun = trimmed_mean,
    aes(colour = condition, linetype = il_fun),
    linewidth = 0.75
  ) +
  facet_grid(order ~ phase) +
  labs(
    x = "Session",
    y = y_lab,
    colour = "Condition:",
    linetype = "Initial Link:"
  ) +
  scale_colour_manual(values = pal) +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12, colour = "black"),
    axis.title = element_text(size = 14, colour = "black"),
    strip.text = element_text(size = 14, colour = "black"),
    strip.background = element_blank(),
    panel.spacing = unit(0.75, "lines"),
    panel.border = element_rect(colour = "black", fill = NA),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.spacing.y = unit(0.1, "lines"),
    legend.key.height = unit(0.6, "lines")
  ) +
  guides(
    colour = guide_legend(nrow = 1),
    linetype = guide_legend(nrow = 1)
  )

#Plot: By Condition and IL
#-------------------------------------------------------------------------------
sess_data$subj_il <- paste(sess_data$subject, sess_data$il_fun, sep = "_")

lat_lc_3 <- latency_condition <- ggplot(
  sess_data,
  aes(x = session, y = m_tr, colour = il_fun)
) +
  geom_line(aes(group = subj_il, linetype = il_fun), alpha = 0.35) +
  stat_summary(
    geom = "line",
    fun = trimmed_mean,
    aes(linetype = il_fun)
  ) +

  stat_summary(
    geom = "point",
    fun = trimmed_mean,
    aes(group = il_fun),
    size = 1
  ) +
  facet_wrap(~condition, nrow = 2) +
  labs(
    x = "Session",
    y = y_lab,
    linetype = "",
    colour = ""
  ) +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  scale_y_continuous(trans = 'log10') +
  scale_colour_manual(values = pal) +
  guides(colour = guide_legend(override.aes = list(shape = NA))) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 12, colour = "black"),
    axis.title = element_text(size = 14, colour = "black"),
    strip.text = element_text(size = 14, colour = "black"),
    strip.background = element_blank(),
    panel.spacing = unit(0.75, "lines"),
    panel.border = element_rect(colour = "black", fill = NA),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.spacing.y = unit(0.1, "lines"),
    legend.key.height = unit(0.6, "lines")
  )
