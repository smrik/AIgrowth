# ============================================================================
# AIgrowth Project - Setup and Configuration
# ============================================================================
# This script contains common setup, theme configuration, and data loading
# utilities used across all visualizations.

library(readr)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(readxl)

# Set consistent theme across all plots
theme_set(ggthemes::theme_fivethirtyeight(base_size = 14))

# Load Tableau color palette for consistency
tableau_colors <- ggthemes_data[["tableau"]][["color-palettes"]][["regular"]][["Tableau 10"]]
tableau <- setNames(tableau_colors$value, tableau_colors$name)

# Output directory for all generated plots
output_dir <- "outputs"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# ============================================================================
# Helper Functions
# ============================================================================

# Calculate Compound Annual Growth Rate (CAGR)
calc_cagr <- function(df, value_col = "log_compute", date_col = "Publication date") {
  df <- df %>% arrange(!!sym(date_col))
  n_years <- as.numeric(difftime(
    max(df[[date_col]], na.rm = TRUE),
    min(df[[date_col]], na.rm = TRUE),
    units = "days"
  )) / 365.25

  cagr <- (exp(last(df[[value_col]], na.rm = TRUE) - first(df[[value_col]], na.rm = TRUE)))^(1/n_years) - 1
  return(scales::percent(cagr, accuracy = 0.01))
}

# Get CAGR position and angle for annotations
get_cagr_position_and_angle <- function(df, era_name, era_col = "era") {
  era_df <- df %>% filter(!!sym(era_col) == era_name)
  n <- nrow(era_df)

  mid_idx <- round(n / 2)
  idx1 <- max(1, mid_idx - 1)
  idx2 <- min(n, mid_idx + 1)

  x1 <- as.numeric(era_df$pub_date_numeric[idx1])
  y1 <- era_df$log_compute[idx1]
  x2 <- as.numeric(era_df$pub_date_numeric[idx2])
  y2 <- era_df$log_compute[idx2]

  angle_rad <- atan2(y2 - y1, x2 - x1)
  angle_deg <- angle_rad * 180 / pi

  fit <- lm(log_compute ~ pub_date_numeric, data = era_df)
  x_mid <- as.numeric(era_df$pub_date_numeric[mid_idx])
  y_mid <- predict(fit, newdata = data.frame(pub_date_numeric = x_mid))
  y_label <- y_mid - 10

  list(x = era_df$`Publication date`[mid_idx] - 300, y = y_label, angle = angle_deg)
}

# Format computation axis with smart label selection
format_compute_axis <- function(values) {
  finite_vals <- values[is.finite(values)]
  if (!length(finite_vals)) {
    return(rep(NA_character_, length(values)))
  }
  max_val <- max(finite_vals, na.rm = TRUE)
  log_formatter <- function(x) {
    paste0("10^", round(x))
  }
  if (max_val < 1e3) {
    return(log_formatter(finite_vals))
  }
  scales::label_number(
    accuracy = 0.1,
    scale_cut = scales::cut_short_scale()
  )(values)
}

# Blend between log and linear positions for animation
blend_position <- function(values, mix, log_range, lin_range) {
  safe_vals <- pmax(values, 1e-8)
  log_norm <- (log10(safe_vals) - log_range[1]) / diff(log_range)
  lin_norm <- (safe_vals - lin_range[1]) / diff(lin_range)
  blended <- (1 - mix) * log_norm + mix * lin_norm
  pmin(pmax(blended, 0), 1)
}

# Load all AI models data and consolidate
load_all_ai_models <- function() {
  cols_to_keep <- c("Model", "Publication date", "Training compute (FLOP)")

  models1 <- read_csv("AI Models public table.csv", show_col_types = FALSE) %>%
    dplyr::select(all_of(cols_to_keep))
  models2 <- read_csv("Epoch Database - Notable Models.csv", show_col_types = FALSE) %>%
    dplyr::select(all_of(cols_to_keep))
  models3 <- read_csv("Frontier models.csv", show_col_types = FALSE) %>%
    dplyr::select(all_of(cols_to_keep))
  models4 <- read_csv("Known and unconfirmed compute-intensive models (public).csv",
                      show_col_types = FALSE) %>%
    dplyr::select(all_of(cols_to_keep))

  bind_rows(models1, models2, models3, models4) %>%
    distinct()
}
