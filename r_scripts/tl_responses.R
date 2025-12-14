library(tidyverse)
options(
  pillar.print_min = 35,
  pillar.sigfig = 2
)

# Load Data
#-------------------------------------------------------------------------------
data <- read_csv("data/choice_data.csv") |>
  select(
    -c(date, time, exp, box, comment, 13:22)
  ) |>
  mutate(
    subject = factor(subject),
    phase = factor(phase, labels = c("Phase 1", "Phase 2")),
    order = factor(
      order,
      levels = c("S-L", "L-S"),
      labels = c("Short-Long", "Long-Short")
    ),
    tl_a_dur = case_when(condition == "Short" ~ 2, condition == "Long" ~ 20),
    tl_b_dur = case_when(condition == "Short" ~ 4, condition == "Long" ~ 40),

    #Rates
    fe_tl_a_rate = fe_tl_sig_a_resp / ((fe_il_sig_resp / 2) * tl_a_dur),
    fe_tl_b_rate = fe_tl_sig_a_resp / ((fe_il_sig_resp / 2) * tl_b_dur),
    fe_tl_sh_rate = fe_tl_sh_resp / ((fe_il_unsig_resp / 2) * tl_a_dur),
    fe_tl_lo_rate = fe_tl_lo_resp / ((fe_il_unsig_resp / 2) * tl_b_dur),
  ) |>
  select(c(1:6, 20:23)) |>
  pivot_longer(
    cols = c(fe_tl_a_rate, fe_tl_b_rate, fe_tl_sh_rate, fe_tl_lo_rate),
    names_to = "tl",
    values_to = "rate"
  ) |>
  mutate(
    tl_dur = case_when(
      (tl == "fe_tl_a_rate" | tl == "fe_tl_sh_rate") & condition == "Short" ~ 2,
      (tl == "fe_tl_a_rate" | tl == "fe_tl_sh_rate") & condition == "Long" ~ 20,
      (tl == "fe_tl_b_rate" | tl == "fe_tl_lo_rate") & condition == "Short" ~ 4,
      (tl == "fe_tl_b_rate" | tl == "fe_tl_lo_rate") & condition == "Long" ~ 40,
    ),
    tl_fun = case_when(
      (tl == "fe_tl_a_rate" | tl == "fe_tl_b_rate") ~ "Signalled",
      (tl == "fe_tl_sh_rate" | tl == "fe_tl_lo_rate") ~ "Unsignalled"
    ),
    tl_dur = factor(tl_dur)
  )

df_short <- data |> filter(condition == "Short")
df_long <- data |> filter(condition == "Long")

# Plot Variables
#-------------------------------------------------------------------------------
pal <- palette.colors(palette = "Dark2")[-c(1:2)]
y_lab <- "Responses / Second"

# Individual Plots
ggplot(data, aes(x = phase_session, y = rate)) +
  geom_line(aes(colour = tl_fun, linetype = order)) +
  facet_grid(subject ~ tl_dur) +
  labs(
    x = "Session",
    y = y_lab,
    colour = "Terminal Link:",
    linetype = "Condition Order:"
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
    legend.box = "vertical",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.spacing.y = unit(0.1, "lines"),
    legend.key.height = unit(0.6, "lines")
  ) +
    guides(
    colour   = guide_legend(nrow = 1),
    linetype = guide_legend(nrow = 1)
  )

# Mean Plots
ggplot(data, aes(x = phase_session, y = rate)) +
  stat_summary(
    geom = "line",
    fun = mean,
    # colour = "black",
    aes(linetype = tl_fun, colour = tl_fun)
  ) +
  facet_grid(order ~ tl_dur) +
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
