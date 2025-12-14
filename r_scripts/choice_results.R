library(tidyverse)
library(effsize)
library(BayesFactor)

options(pillar.print_min = 35, pillar.sigfig = 2)

# Load Data
#-------------------------------------------------------------------------------
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
    phase = factor(phase, labels = c("Phase 1", "Phase 2")),
    order = factor(order, labels = c("Long-Short", "Short-Long")),
    cp = ch_il_sig_resp / (ch_il_sig_resp + ch_il_unsig_resp)
  ) |>
  filter(phase_session %in% c(28:30))


# Summary Stats'
#-------------------------------------------------------------------------------

# Individual Pigeons
indv_birds <- data |>
  group_by(subject, condition, order) |>
  summarise(
    # Total sig responses
    tot_sig_resp = sum(ch_il_sig_resp),
    # Total Responses
    tot_resp = sum(ch_il_sig_resp + ch_il_unsig_resp),
    # Choice Prop
    cp = tot_sig_resp / tot_resp
  )

# Condition and Order
cond_order <- indv_birds |>
  group_by(condition, order) |>
  summarise(
    n = length(cp),
    m = mean(cp),
    s = sd(cp)
  )

# Condition Only
cond_only <- indv_birds |>
  group_by(condition) |>
  summarise(
    n = length(cp),
    m = mean(cp),
    s = sd(cp)
  )

# Note: A logit transform is applied as a robustness check on the arcsine 
# transform

# Subject level data
cond_data <- data |>
  group_by(subject, condition) |>
  summarise(
    # Total sig responses
    tot_sig_resp = sum(ch_il_sig_resp),
    # Total Responses
    tot_resp = sum(ch_il_sig_resp + ch_il_unsig_resp),
    # Choice Prop
    cp = tot_sig_resp / tot_resp,
    # Arcsine Transformation
    cp_arc = asin(sqrt(cp)),
    # Logit Transform
    p_cc = (tot_sig_resp + 0.5) / (tot_resp + 1), # continuity correction (cc)
    logit_p = log(p_cc / (1 - p_cc))
  ) |>
  arrange(condition, subject)

cond_data

# Difference Scores
diff_scores <-
  cond_data$cp[cond_data$condition == "Long"] -
  cond_data$cp[cond_data$condition == "Short"]

diff_scores_arc <-
  cond_data$cp_arc[cond_data$condition == "Long"] -
  cond_data$cp_arc[cond_data$condition == "Short"]

diff_scores_logit <-
  cond_data$logit_p[cond_data$condition == "Long"] -
  cond_data$logit_p[cond_data$condition == "Short"]

hist(diff_scores)
hist(diff_scores_arc)
hist(diff_scores_logit)

qqnorm(diff_scores)
qqline(diff_scores)

qqnorm(diff_scores_arc)
qqline(diff_scores_arc)

qqnorm(diff_scores_logit)
qqline(diff_scores_logit)

test_arc <- t.test(diff_scores_arc)
test_arc

test_logit <- t.test(diff_scores_logit)
test_logit

# Effect Size
g_arc <- cohen.d(
  cp_arc ~ condition | Subject(subject),
  paired = TRUE,
  hedges.correction = TRUE,
  data = cond_data
)
g_arc

g_logit <- cohen.d(
  logit_p ~ condition | Subject(subject),
  paired = TRUE,
  hedges.correction = TRUE,
  data = cond_data
)
g_logit

# Bayes Factor
bf_arc <- ttestBF(x = diff_scores_arc)
bf_logit <- ttestBF(x = diff_scores_logit)

# Save output
options(pillar.sigfig = 2)

sink("outputs/stats_choice.txt")
cat("=== Individual Birds ===\n")
print(indv_birds)
cat("\n\n=== Condition x Order ===\n")
print(cond_order)
cat("\n\n=== Condition Only ===\n")
print(cond_only)
cat("\n\n=== Test ===\n")
print(test_arc)
cat("\n\n=== Effect Size ===\n")
print(g_arc)
cat("\n\n=== Bayes Factor ===\n")
print(bf_arc)
sink()
