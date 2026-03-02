# Script: 01_adqs_derivation.R -------------------------------------
# Purpose: Simulate SDTM QS data and derive ADaM ADQS for PHQ-9
# Author: Tiago Fragoso


library(dplyr)
library(tidyr)
library(purrr)
purrr::walk(.x = "src/F01_phq9_total_score.R",.f = source)

#SIMULATE SDTM QS (Questionnaire) DOMAIN -------------------------

set.seed(2026) # Reproducibility

n_subjects <- 500
visits <- c("Baseline", "Week 2", "Week 12")
items <- paste0("PHQ010", 1:9)

# Create a realistic base dataframe
sdtm_qs <- expand_grid(
  USUBJID = paste0("SUBJ-", sprintf("%04d", 1:n_subjects)),
  VISIT = visits,
  QSTESTCD = items
) %>%
  # Simulate scores (0 to 3) with a slight right skew for a clinical population
  mutate(
    QSSTRESN = sample(0:3, size = n(), replace = TRUE, prob = c(0.1, 0.3, 0.4, 0.2)),
    QSCAT = "PHQ-9"
  )

# Inject some realistic missing data (MCAR for this simulation)
missing_indices <- sample(1:nrow(sdtm_qs), size = floor(0.02 * nrow(sdtm_qs)))
sdtm_qs$QSSTRESN[missing_indices] <- NA

# DERIVE ADaM ADQS -------------------------------------------------------

# Step A: Pivot wider to calculate the total score per subject/visit
adqs_wide <- sdtm_qs %>%
  select(USUBJID, VISIT, QSTESTCD, QSSTRESN) %>%
  pivot_wider(
    names_from = QSTESTCD, 
    values_from = QSSTRESN,
    names_prefix = "ITEM_"
  ) %>%
  # Apply our toolbox function row-wise
  rowwise() %>%
  mutate(
    AVAL_TOT = phq9_total_score(c_across(starts_with("ITEM_")))
  ) %>%
  ungroup()

# Step B: Restructure into standard ADaM format (One record per parameter per visit)
adqs_param <- adqs_wide %>%
  select(USUBJID, VISIT, AVAL = AVAL_TOT) %>%
  mutate(
    PARAMCD = "PHQ9TOT",
    PARAM = "PHQ-9 Total Score",
    PARAMN = 1,
    AVALU = "points"
  ) %>%
  filter(!is.na(AVAL)) # Standard to drop non-evaluable records in ADaM AVAL

# Step C: Derive Baseline (BASE) and Change from Baseline (CHG)
adqs_final <- adqs_param %>%
  group_by(USUBJID, PARAMCD) %>%
  mutate(
    # Isolate the baseline value
    BASE = AVAL[VISIT == "Baseline"],
    # Calculate change from baseline
    CHG = if_else(VISIT != "Baseline", AVAL - BASE, NA_real_),
    # Percentage change (optional but often requested)
    PCHG = if_else(VISIT != "Baseline" & BASE != 0, (CHG / BASE) * 100, NA_real_)
  ) %>%
  ungroup() %>%
  arrange(USUBJID, PARAMCD, VISIT)

write.csv(x = adqs_final,file = "data/phq9_ADaM_final.csv")
