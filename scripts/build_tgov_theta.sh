#!/usr/bin/env bash
# scripts/build_tgov_theta.sh  ─────────────────────────────────────────────
set -euo pipefail
echo "🛠  TGOV: building theta summary…"

# 0.  minimal OS deps ----------------------------------------------------------
sudo apt-get update -qq
sudo apt-get install -y --no-install-recommends \
     r-base build-essential libcurl4-openssl-dev

# 1.  R packages & CmdStanR ----------------------------------------------------
Rscript - <<'RS'
install.packages(c("cmdstanr","tidyverse","countrycode"), repos="https://cloud.r-project.org")
cmdstanr::install_cmdstan()                       # downloads & compiles CmdStan
RS

# 2.  Fit DCPO model on raw survey file ----------------------------------------
Rscript - <<'RS'
library(cmdstanr); library(tidyverse); library(countrycode)

## ── read raw survey file ──
raw <- read_csv("data/dcpo_input_raw_gov.csv")    # already in the TGOV repo

## ── helper to build DCPO data list (minimal) ──
build_dcpo <- function(df, item="item", iso="iso3", year="year",
                       y="response", n="n"){
  df <- df %>% mutate(ii=row_number())
  list(
    J = nrow(df),
    K = length(unique(df[[item]])),
    T = length(unique(df[[year]])),
    jj = df[[ii]],
    kk = as.integer(as.factor(df[[item]])),
    tt = as.integer(as.factor(df[[year]])),
    y  = df[[y]],
    n  = df[[n]]
  )
}

dcpo_dat <- build_dcpo(raw)

## ── compile & sample Stan model ──
model <- cmdstan_model("models/dcpo_ordinal.stan")
fit   <- model$sample(data = dcpo_dat,
                      chains = 4, iter_warmup = 1000, iter_sampling = 1000,
                      adapt_delta = .95, show_messages = FALSE)

## ── extract posterior means & sd ──
theta_draws <- fit$draws("theta", format="df")   # 1000 draws × J rows
theta_summary <- theta_draws %>%
  pivot_longer(cols = starts_with("theta["), names_to = "param",
               values_to = "draw") %>%
  separate(param, into = c("theta","iso_idx","year_idx","draw_idx"),
           sep = "\\[|,|\\]", convert = TRUE) %>%
  group_by(iso_idx, year_idx) %>%
  summarise(mean = mean(draw), sd = sd(draw), .groups = "drop") %>%
  left_join(raw %>% distinct(iso_idx=row_number(), iso3), by="iso_idx") %>%
  left_join(raw %>% distinct(year_idx=as.integer(as.factor(year)), year),
            by="year_idx") %>%
  transmute(iso3,
            year,
            trust_pct = (mean*25) + 50,      # rescale −2…+2 → 0…100
            me_sd     =  sd  * 25)

write_csv(theta_summary, "tgov_theta_summary.csv")
RS

echo "✅  Done.  File written to $(pwd)/tgov_theta_summary.csv"
