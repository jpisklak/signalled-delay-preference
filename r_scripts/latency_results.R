library(tidyverse)
library(WRS2)
options(pillar.print_min = 35, pillar.sigfig = 2)

# Load and organize data
#-------------------------------------------------------------------------------
# Pigeon Condition Orders
s_l <- c(6163, 6159, 100)
l_s <- c(13, 1269, 822)

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
    )
  ) |>
  select(-c(phase_session))

# Summary stats for last three sessions
#-------------------------------------------------------------------------------
# Pigeon Latencies (aggregated across last three sessions)
indiv_lat <- data |> 
  filter(session %in% c(28:30)) |> 
  group_by(subject, condition, il_fun) |> 
  summarise(
    n = sum(!is.na(latency_ms)),
    mean = mean(latency_ms, na.rm = TRUE),
    sd = sd(latency_ms, na.rm = TRUE),
    med = median(latency_ms, na.rm = TRUE),
    madn = mad(latency_ms, na.rm = TRUE),
    IQR = IQR(latency_ms, na.rm = TRUE),
    G = 0.2,
    m_tr = mean(latency_ms, tr = G, na.rm = TRUE),
    s_win = sqrt(winvar(latency_ms, tr = G, na.rm = TRUE))
  ) |> 
  arrange(subject, condition)

lat_stats <- indiv_lat |> 
  group_by(condition, il_fun) |> 
  summarise(
    n = length(m_tr),
    G = 0.2,
    m_trim = mean(m_tr, tr = G, na.rm = TRUE),
    s_win = sqrt(winvar(m_tr, tr = G, na.rm = TRUE))
  )

# Trimmed Paired (i.e., one-sample) T-Test
#-------------------------------------------------------------------------------
# Convert to wide
indiv_lat_wide <- indiv_lat |>
  select(subject, condition, il_fun, n, G, m_tr) |> 
  pivot_wider(names_from = il_fun, values_from = m_tr) |> 
  mutate(
    diff_unsig_sig = Unsignalled - Signalled
  )

diff_scores <- 
  indiv_lat_wide$diff_unsig_sig[indiv_lat_wide$condition == "Long"] - 
  indiv_lat_wide$diff_unsig_sig[indiv_lat_wide$condition == "Short"]

G <- 0.2
N <- length(diff_scores)
m_t <- mean(diff_scores, tr = G)
s_w <- sqrt(winvar(diff_scores, tr = G))
se <- s_w / ((1 - 2 * G)*sqrt(N))
h <- N - 2 * floor(G * N)
df <- h - 1

t_stat <- (m_t - 0) / se
p <- pt(abs(t_stat), df = df, lower.tail = FALSE) * 2

# Save output
#-------------------------------------------------------------------------------
options(pillar.sigfig = 2)
sink("outputs/stats_latency.txt")
cat("=== Condition x IL ===\n")
print(lat_stats)
cat("\n\n=== Trimmed T-test ===\n")
cat(paste0("Test-Stat = ", round(t_stat, 2), "\n"))
cat(paste0("df = ", df, "\n"))
cat(paste0("p = ", round(p, 3)))
sink()