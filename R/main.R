#
# AI Training Compute Growth Analysis & Visualization ----
#
# This script generates visualizations of AI model training compute growth
# across different eras and time periods.

# Set working directory to project root
if (!dir.exists("data")) {
  setwd(dirname(getwd()))
}

library(readr)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(gganimate)
library(gifski)
library(scales)

# Setup theme and color palette
theme_set(ggthemes::theme_fivethirtyeight(base_size = 14))
tableau_colors <- ggthemes_data[["tableau"]][["color-palettes"]][["regular"]][["Tableau 10"]]
tableau <- setNames(tableau_colors$value, tableau_colors$name)

# Load styling configuration
source("R/plot_styles.R")

Frontier_models <- read_csv("data/Epoch Database - Notable Models.csv")
View(Frontier_models)

models <- Frontier_models %>%
  mutate(era = case_when(
    `Publication date` <= "2010-01-01" ~ "Pre-Deep Learning",
    TRUE ~ "Deep Learning Era"),
    log_compute = log(`Training compute (FLOP)`),
    pub_date_numeric = as.numeric(`Publication date`)
  )


#
## SECTION 1: Historical AI Compute Growth (All Eras) ----
#
# Compares Pre-Deep Learning Era vs Deep Learning Era growth rates

# Set year break
break_year <- as.Date("2010-01-01")
xmin <- min(models$`Publication date`)
xmax <- max(models$`Publication date`)
ymax <- max(models$log_compute, na.rm = TRUE)

# Calculate CAGR for each era
calc_cagr <- function(df) {
  df <- df %>% arrange(`Publication date`)
  n_years <- as.numeric(difftime(max(df$`Publication date`), min(df$`Publication date`), units = "days")) / 365.25
  cagr <- (exp(last(df$log_compute, na_rm = TRUE) - first(df$log_compute, na_rm = TRUE)))^(1/n_years) - 1
  return(scales::percent(cagr, accuracy = 0.01))
}
cagr_pre <- calc_cagr(filter(models, era == "Pre-Deep Learning"))
cagr_post <- calc_cagr(filter(models, era == "Deep Learning Era"))


get_cagr_position_and_angle <- function(df, era_name) {
  era_df <- df %>% filter(era == era_name)
  n <- nrow(era_df)

  # Midpoint index
  mid_idx <- round(n / 2)

  # Points around the midpoint
  idx1 <- max(1, mid_idx - 1)
  idx2 <- min(n, mid_idx + 1)

  x1 <- as.numeric(era_df$pub_date_numeric[idx1])
  y1 <- era_df$log_compute[idx1]
  x2 <- as.numeric(era_df$pub_date_numeric[idx2])
  y2 <- era_df$log_compute[idx2]

  # Calculate angle in degrees - corrected
  angle_rad <- atan2(y2 - y1, x2 - x1)
  angle_deg <- angle_rad * 180 / pi  # Degrees, can be positive or negative, no modulo

  # Predict y at midpoint for placement
  fit <- lm(log_compute ~ pub_date_numeric, data = era_df)
  x_mid <- as.numeric(era_df$pub_date_numeric[mid_idx])
  y_mid <- predict(fit, newdata = data.frame(pub_date_numeric = x_mid))
  y_label <- y_mid - 10

  list(x = era_df$`Publication date`[mid_idx]-300, y = y_label, angle = angle_deg)
}

pos_pre <- get_cagr_position_and_angle(models, "Pre-Deep Learning")
pos_post <- get_cagr_position_and_angle(models, "Deep Learning Era")

pos_pre$angle=17
pos_post$angle=41

plot_all <- ggplot(models, aes(x = `Publication date`, y = log_compute)) +
  coord_cartesian(clip = "off") +
  annotate(
    "rect",
    xmin = break_year,
    xmax = xmax,
    ymin = 0,
    ymax = 65,
    fill = COLORS$shade_light, alpha = ALPHA$shade
  ) +
  geom_point(aes(color = era), alpha = ALPHA$points_all, size = SIZES$point) +
  geom_smooth(data = filter(models, era == "Pre-Deep Learning"),
              aes(color = "Pre-Deep Learning"),
              se = FALSE,
              method = "lm",
              linewidth = SIZES$line_trend) +
  geom_smooth(data = filter(models, era == "Deep Learning Era"),
              aes(color = "Deep Learning Era"),
              se = FALSE,
              method = "lm",
              linewidth = SIZES$line_trend) +
  geom_vline(xintercept = break_year, linetype = "dashed", color = COLORS$line_dark, linewidth = SIZES$line_divider) +
  # Era captions above shaded region
  annotate("text",
           x = as.Date("1970-06-01"), y = 65,
           label = "Pre-Deep Learning Era", color = COLORS$secondary, fontface = "bold", size = FONT_SIZES$annotation, hjust = 0) +
  annotate("text",
           x = as.Date("2011-01-01"), y = 65,
           label = "Deep Learning Era", color = COLORS$primary, fontface = "bold", size = FONT_SIZES$annotation, hjust = 0) +
  # CAGR labels above lines at the midpoint
  annotate(
    "text",
    x = pos_pre$x,
    y = pos_pre$y,
    label = paste("CAGR:", cagr_pre),
    color = COLORS$secondary,
    angle = 0,
    fontface = "bold",
    size = FONT_SIZES$annotation,
    hjust = 0.5,
    vjust = 0.5
  ) +
  annotate(
    "text",
    x = pos_post$x - 1000,
    y = (pos_post$y - 10),
    label = paste("CAGR:", cagr_post),
    color = COLORS$primary,
    angle = 0,
    fontface = "bold",
    size = FONT_SIZES$annotation,
    hjust = 0.5,
    vjust = 0.5
  ) +
  ylim(c(0, 65)) +
  labs(
    x = NULL,
    y = "Log(Training Compute, FLOP)",
    title = "AI Compute: Pre-Deep Learning vs Deep Learning Era",
    subtitle = "Shaded region: Deep Learning Era (>2010). CAGR shows annualized growth in each era.",
    caption = "Source: Epoch AI, Stanford AI Index",
  ) +
  scale_colour_tableau() +
  apply_aigrowth_theme()

print(plot_all)
save_plot(plot_all, "AI_Compute_Growth.png")


#
## SECTION 2: Modern Models Growth - Log Scale (2010+) ----
#
# Shows all notable models (443 total) on logarithmic scale

# Assuming 'models' dataset is already loaded
# Filter to only include models with training compute data from 2010 onwards
models_filtered <- models %>%
  filter(!is.na(`Training compute (FLOP)`),
         !is.na(`Publication date`),
         `Publication date` >= as.Date("2010-01-01"))

# Calculate the trend line (using log-linear regression)
# Convert dates to numeric for regression
models_filtered <- models_filtered %>%
  mutate(
    year_numeric = as.numeric(format(`Publication date`, "%Y")) +
      as.numeric(format(`Publication date`, "%j")) / 365.25,
    log_flop = log10(`Training compute (FLOP)`)
  )

# Fit linear model
lm_model <- lm(log_flop ~ year_numeric, data = models_filtered)

# Create prediction data for trend line
pred_data <- data.frame(
  year_numeric = seq(2010, 2026, by = 0.1)
)
pred_data$log_flop <- predict(lm_model, newdata = pred_data)
pred_data$flop <- 10^pred_data$log_flop
pred_data$date <- as.Date(paste0(pred_data$year_numeric, "-01-01"), format = "%Y-%m-%d")

# Calculate growth rate from slope
slope <- coef(lm_model)[2]
growth_per_year <- 10^slope  # This gives the multiplication factor per year

# Set theme
theme_set(theme_fivethirtyeight(base_size = 14))

# Create label data for key models
label_models <- models_filtered %>%
  filter(
    Model %in% c("AlexNet") |
      grepl("AlphaGo Zero", Model) |
      (grepl("GPT-3", Model) & !grepl("GPT-4", Model)) |
      (grepl("GPT-4", Model) & !grepl("GPT-5", Model)) |
      (grepl("^PaLM$", Model)) |
      grepl("Gemini Ultra", Model) |
      grepl("Grok.*4", Model)
  ) %>%
  mutate(
    label_text = case_when(
      Model == "AlexNet" ~ "AlexNet",
      grepl("AlphaGo Zero", Model) ~ "AlphaGo Zero",
      grepl("GPT-3", Model) & !grepl("GPT-4", Model) ~ "GPT-3",
      grepl("GPT-4", Model) & !grepl("GPT-5", Model) ~ "GPT-4",
      grepl("^PaLM$", Model) ~ "PaLM",
      grepl("Gemini Ultra", Model) ~ "Gemini Ultra",
      grepl("Grok.*4", Model) ~ "Grok 4",
      TRUE ~ Model
    )
  ) %>%
  group_by(label_text) %>%
  slice(1) %>%
  ungroup()

# Create the plot
p2 <- ggplot() +
  # Add all model points with lower alpha
  geom_point(data = models_filtered,
             aes(x = `Publication date`, y = `Training compute (FLOP)`),
             color = COLORS$primary,
             alpha = ALPHA$points_all,
             size = SIZES$point) +
  # Add trend line
  geom_line(data = pred_data,
            aes(x = date, y = flop),
            color = COLORS$primary,
            size = SIZES$line_trend) +
  # Add labels using ggrepel for automatic positioning
  geom_text_repel(
    data = label_models,
    aes(x = `Publication date`,
        y = `Training compute (FLOP)`,
        label = label_text,
        segment.shape  = c(-1, -0.5, 0, 0.5, 1),
        segment.square = FALSE),
    size = FONT_SIZES$label,
    nudge_y = 2,
    color = FONT_COLORS$label,
    fontface = "bold",
    # Configure the appearance of arrows
    segment.color = FONT_COLORS$segment,
    segment.size = 0.5,
    arrow = arrow(length = unit(2, "mm")),
    # Control label positioning
    box.padding = REPEL_CONFIG$box_padding,
    point.padding = REPEL_CONFIG$point_padding,
    min.segment.length = REPEL_CONFIG$min_segment_length,
    max.overlaps = REPEL_CONFIG$max_overlaps,
    # Nudge labels away from dense areas
    force = REPEL_CONFIG$force,
    force_pull = REPEL_CONFIG$force_pull,
    direction = "both",
    segment.curvature = REPEL_CONFIG$segment_curvature,
    segment.ncp = REPEL_CONFIG$segment_ncp,
    segment.angle = REPEL_CONFIG$segment_angle,
    xlim = c(NA, as.Date("2026-06-30")),
    seed = 1
  ) +
  # Add trend line annotation
  annotate("text",
           x = as.Date("2022-06-01"),
           y = 10^14,
           label = paste0("Growth:\n", round(growth_per_year, 1), "x/year"),
           color = COLORS$primary,
           size = FONT_SIZES$annotation,
           fontface = "bold",
           lineheight = 0.9) +
  # Scales
  scale_y_log10(
    breaks = 10^seq(13, 27, by = 2),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_x_date(
    breaks = seq(as.Date("2010-01-01"), as.Date("2026-01-01"), by = "2 years"),
    date_labels = "%Y",
    limits = c(as.Date("2010-01-01"), as.Date("2026-06-30"))
  ) +
  # Labels
  labs(
    x = "Publication date",
    y = "Training compute (FLOP)",
    title = "AI Training Compute Growth: All Notable Models",
    subtitle = "Training compute of all notable models (443 total)",
    caption = "Source: Epoch AI"
  ) +
  scale_colour_tableau() +
  apply_aigrowth_theme()

print(p2)
save_plot(p2, "training_compute_modern_models.png")



#
## SECTION 3: Frontier Models Growth - Linear Scale (90th Percentile) ----
#
# Shows frontier models (90th percentile per year) on linear scale

All_models <- read_csv("data/AI Models public table.csv")



# Assuming 'models' dataset is already loaded
# Filter to only include models with training compute data from 2010 onwards
models_filtered <- All_models %>%
  filter(!is.na(`Training compute (FLOP)`),
         !is.na(`Publication date`),
         `Publication date` >= as.Date("2010-01-01"))

# Convert dates to numeric for regression
models_filtered <- models_filtered %>%
  mutate(
    year_numeric = as.numeric(format(`Publication date`, "%Y")) +
      as.numeric(format(`Publication date`, "%j")) / 365.25,
    log_flop = log10(`Training compute (FLOP)`)
  )

# FRONTIER MODEL SELECTION: Get top models per year (95th percentile or higher)
# This focuses the trend line on the cutting-edge models, not the average
frontier_models <- models_filtered %>%
  group_by(year = floor(year_numeric)) %>%
  filter(`Training compute (FLOP)` >= quantile(`Training compute (FLOP)`, 0.90, na.rm = TRUE)) %>%
  ungroup()

# Alternative: Get the maximum model per 6-month period
# This ensures we track the absolute frontier
frontier_models_alt <- models_filtered %>%
  mutate(half_year = floor(year_numeric * 2) / 2) %>%
  group_by(half_year) %>%
  slice_max(`Training compute (FLOP)`, n = 3, with_ties = FALSE) %>%
  ungroup()

# Use the alternative approach (tracking max models every 6 months)
frontier_models <- frontier_models_alt

# Fit linear model in log space to FRONTIER models only
lm_model <- lm(log_flop ~ year_numeric, data = frontier_models)

# Create prediction data for trend line extending into future
pred_data <- data.frame(
  year_numeric = seq(2010, 2026, by = 0.01)
)
pred_data$log_flop <- predict(lm_model, newdata = pred_data)
pred_data$flop <- 10^pred_data$log_flop  # Convert back to linear scale - EXPONENTIAL
pred_data$date <- as.Date(paste0(floor(pred_data$year_numeric), "-01-01"), format = "%Y-%m-%d") +
  (pred_data$year_numeric - floor(pred_data$year_numeric)) * 365.25

# Calculate growth rate from slope
slope <- coef(lm_model)[2]
growth_per_year <- 10^slope  # This gives the multiplication factor per year

# Set theme
theme_set(theme_fivethirtyeight(base_size = 14))

# Create label data for key models
label_models <- models_filtered %>%
  filter(
    grepl("Grok.*4", Model) |
      (grepl("GPT-4", Model) & !grepl("GPT-5", Model)) |
      grepl("Gemini Ultra", Model) |
      (grepl("^PaLM$", Model)) |
      (grepl("GPT-3", Model) & !grepl("GPT-4", Model))
  ) %>%
  mutate(
    label_text = case_when(
      grepl("Grok.*4", Model) ~ "Grok 4",
      grepl("GPT-4", Model) & !grepl("GPT-5", Model) ~ "GPT-4",
      grepl("Gemini Ultra", Model) ~ "Gemini Ultra",
      grepl("^PaLM$", Model) ~ "PaLM",
      grepl("GPT-3", Model) & !grepl("GPT-4", Model) ~ "GPT-3",
      TRUE ~ Model
    )
  ) %>%
  group_by(label_text) %>%
  slice(1) %>%
  ungroup()

# Get max FLOP value for positioning
max_flop <- max(models_filtered$`Training compute (FLOP)`, na.rm = TRUE)

# Create the plot with LINEAR y-axis showing EXPONENTIAL curve tracking frontier
p1 <- ggplot() +
  # Add all model points with lower alpha
  geom_point(data = models_filtered,
             aes(x = `Publication date`, y = `Training compute (FLOP)`),
             color = COLORS$primary,
             alpha = ALPHA$points_all,
             size = SIZES$point) +
  # Highlight frontier models used for trend line
  geom_point(data = frontier_models,
             aes(x = `Publication date`, y = `Training compute (FLOP)`),
             color = COLORS$secondary,
             alpha = ALPHA$points_frontier,
             size = SIZES$point_frontier_small,
             shape = 1,
             stroke = SIZES$stroke_frontier) +
  # Add EXPONENTIAL trend line following frontier models
  geom_line(data = pred_data,
            aes(x = date, y = flop),
            color = COLORS$secondary,
            size = SIZES$line_trend,
            lineend = "round") +
  # Add labels using ggrepel - only for top models
  geom_text_repel(
    data = label_models,
    aes(x = `Publication date`,
        y = `Training compute (FLOP)`,
        label = label_text),
    size = FONT_SIZES$label,
    color = FONT_COLORS$label,
    fontface = "bold",
    # Configure the appearance of arrows
    segment.color = FONT_COLORS$segment,
    segment.size = 0.5,
    arrow = arrow(length = unit(2, "mm")),
    # Control label positioning
    box.padding = REPEL_CONFIG$box_padding,
    point.padding = REPEL_CONFIG$point_padding,
    min.segment.length = REPEL_CONFIG$min_segment_length,
    max.overlaps = REPEL_CONFIG$max_overlaps,
    # Nudge labels away from dense areas
    force = REPEL_CONFIG$force,
    force_pull = REPEL_CONFIG$force_pull,
    direction = "both",
    segment.curvature = REPEL_CONFIG$segment_curvature,
    segment.ncp = REPEL_CONFIG$segment_ncp,
    segment.angle = REPEL_CONFIG$segment_angle,
    xlim = c(NA, as.Date("2027-03-01")),
    seed = 42
  ) +
  # Add annotation with better positioning
  annotate("text",
           x = as.Date("2023-06-01"),
           y = max_flop * 0.45,
           label = paste0("Frontier growth:\n", round(growth_per_year, 1), "x/year"),
           color = COLORS$secondary,
           size = FONT_SIZES$annotation,
           fontface = "bold",
           lineheight = 0.9) +
  # Add context annotation about early models
  # LINEAR y-axis scale showing exponential curve
  scale_y_continuous(
    labels = scales::label_scientific(digits = 1),
    expand = expansion(mult = c(0.01, 0.15))
  ) +
  scale_x_date(
    breaks = seq(as.Date("2010-01-01"), as.Date("2026-01-01"), by = "2 years"),
    date_labels = "%Y",
    limits = c(as.Date("2010-01-01"), as.Date("2027-03-01"))
  ) +
  # Labels
  labs(
    x = "Publication date",
    y = "Training compute (FLOP)",
    title = "AI Training Compute Growth: Notable Models (90th Percentile)",
    subtitle = "Training compute of 90th percentile models per year",
    caption = "Source: Epoch AI. Orange circles indicate 90th percentile models."
  ) +
  scale_colour_tableau() +
  apply_aigrowth_theme()

print(p1)
save_plot(p1, "training_compute_notable_models_linear_frontier.png")

#
## SECTION 4: Absolute Frontier Models Growth - Linear Scale ----
#
# Shows the single highest-compute model per quarter on linear scale



models1 <- read_csv("data/AI Models public table.csv")
models2 <- read_csv("data/Epoch Database - Notable Models.csv")
models3 <- read_csv("data/Frontier models.csv")
models4 <- read_csv("data/Known and unconfirmed compute-intensive models (public).csv")

cols_to_keep <- c("Model", "Publication date", "Training compute (FLOP)")

models1 <- models1 %>% dplyr::select(all_of(cols_to_keep))
models2 <- models2 %>% dplyr::select(all_of(cols_to_keep))
models3 <- models3 %>% dplyr::select(all_of(cols_to_keep))
models4 <- models4 %>% dplyr::select(all_of(cols_to_keep))

models_merge1 <- rbind2(models1, models2)
models_merge3 <- rbind2(models3, models4)
models_merge <- rbind2(models_merge1, models_merge3)

models_merge <- models_merge %>% dplyr::distinct()

yr = 2010
yr_str = paste(as.character(yr), "-01-01", sep = "")

# Assuming 'models' dataset is already loaded
# Filter to only include models with training compute data from 2010 onwards
models_filtered <- models_merge %>%
  filter(!is.na(`Training compute (FLOP)`),
         !is.na(`Publication date`),
         `Publication date` >= as.Date(yr_str))

# Convert dates to numeric for regression
models_filtered <- models_filtered %>%
  mutate(
    year_numeric = as.numeric(format(`Publication date`, "%Y")) +
      as.numeric(format(`Publication date`, "%j")) / 365.25,
    log_flop = log10(`Training compute (FLOP)`)
  )

# FRONTIER SELECTION: Get the SINGLE top model every 3 months
# This tracks the absolute cutting edge at each point in time
frontier_models <- models_filtered %>%
  mutate(quarter = floor(year_numeric * 1) / 1) %>%  # 3-month periods
  group_by(quarter) %>%
  slice_max(`Training compute (FLOP)`, n = 1, with_ties = FALSE) %>%  # TOP 1 only
  ungroup() %>%
  arrange(year_numeric)

# Fit linear model in log space to the absolute top models
lm_model <- lm(log_flop ~ year_numeric, data = frontier_models)

# Create prediction data for trend line extending into future
pred_data <- data.frame(
  year_numeric = seq(yr, 2027, by = 0.001)
)
pred_data$log_flop <- predict(lm_model, newdata = pred_data)
pred_data$flop <- 10^pred_data$log_flop  # Convert back to linear scale - EXPONENTIAL
pred_data$date <- as.Date(paste0(floor(pred_data$year_numeric), "-01-01"), format = "%Y-%m-%d") +
  (pred_data$year_numeric - floor(pred_data$year_numeric)) * 365.25

# Calculate growth rate from slope
slope <- coef(lm_model)[2]
growth_per_year <- 10^slope  # This gives the multiplication factor per year

# Set theme
theme_set(theme_fivethirtyeight(base_size = 14))

# Create label data for key models
label_models <- models_filtered %>%
  filter(
    grepl("Grok.*4", Model) |
      (grepl("GPT-4", Model) & !grepl("GPT-5", Model)) |
      grepl("Gemini Ultra", Model) |
      (grepl("^PaLM$", Model)) |
      (grepl("GPT-3", Model) & !grepl("GPT-4", Model))
  ) %>%
  mutate(
    label_text = case_when(
      grepl("Grok.*4", Model) ~ "Grok 4",
      grepl("GPT-4", Model) & !grepl("GPT-5", Model) ~ "GPT-4",
      grepl("Gemini Ultra", Model) ~ "Gemini Ultra",
      grepl("^PaLM$", Model) ~ "PaLM",
      grepl("GPT-3", Model) & !grepl("GPT-4", Model) ~ "GPT-3",
      TRUE ~ Model
    )
  ) %>%
  group_by(label_text) %>%
  slice(1) %>%
  ungroup()

# Get max FLOP value for positioning
max_flop <- max(models_filtered$`Training compute (FLOP)`, na.rm = TRUE)

# Create the plot with LINEAR y-axis showing EXPONENTIAL curve tracking absolute frontier
p <- ggplot() +
  # Add all model points with lower alpha
  geom_point(data = models_filtered,
             aes(x = `Publication date`, y = `Training compute (FLOP)`),
             color = COLORS$primary,
             alpha = ALPHA$points_all,
             size = SIZES$point) +
  # Add EXPONENTIAL trend line following absolute top models
  geom_line(data = pred_data,
            aes(x = date, y = flop),
            color = COLORS$frontier,
            size = SIZES$line_trend,
            lineend = "round") +
  # Add labels using ggrepel - only for top models
  geom_text_repel(
    data = label_models,
    aes(x = `Publication date`,
        y = `Training compute (FLOP)`,
        label = label_text),
    size = FONT_SIZES$label,
    color = FONT_COLORS$label,
    fontface = "bold",
    # Configure the appearance of arrows
    segment.color = FONT_COLORS$segment,
    segment.size = 0.5,
    arrow = arrow(length = unit(2, "mm")),
    # Control label positioning
    box.padding = REPEL_CONFIG$box_padding,
    point.padding = REPEL_CONFIG$point_padding,
    min.segment.length = REPEL_CONFIG$min_segment_length,
    max.overlaps = REPEL_CONFIG$max_overlaps,
    # Nudge labels away from dense areas
    force = REPEL_CONFIG$force,
    force_pull = REPEL_CONFIG$force_pull,
    direction = "both",
    segment.curvature = REPEL_CONFIG$segment_curvature,
    segment.ncp = REPEL_CONFIG$segment_ncp,
    segment.angle = REPEL_CONFIG$segment_angle,
    xlim = c(NA, as.Date("2027-03-01")),
    seed = 42
  ) +
  # Add annotation with better positioning
  annotate("text",
           x = as.Date("2023-06-01"),
           y = max_flop * 0.45,
           label = "Frontier growth:\n4.3x/year",
           color = COLORS$frontier,
           size = FONT_SIZES$annotation,
           fontface = "bold",
           lineheight = 0.9) +
  # LINEAR y-axis scale showing exponential curve
  scale_y_continuous(
    labels = scales::label_scientific(digits = 1),
    expand = expansion(mult = c(0.01, 0.15)),
    limits = c(0, max_flop)
  ) +
  scale_x_date(
    breaks = seq(as.Date("2015-01-01"), as.Date("2026-01-01"), by = "2 years"),
    date_labels = "%Y",
    limits = c(as.Date(yr_str), as.Date("2026-03-01"))
  ) +
  # Labels
  labs(
    x = "Publication date",
    y = "Training compute (FLOP)",
    title = "AI Training Compute Growth: Frontier Models",
    subtitle = "Training compute of frontier models (highest per quarter)",
    caption = "Source: Epoch AI"
  ) +
  scale_colour_tableau() +
  apply_aigrowth_theme()

print(p)
save_plot(p, "training_compute_notable_models_linear_absolute_frontier.png")



#
## SECTION 5: Animation - Log to Linear Scale Transition ----
#
# Interactive animation showing the transition from log to linear scale

### Helpers ---------------------------------------------------------------

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

blend_position <- function(values, mix, log_range, lin_range) {
  safe_vals <- pmax(values, 1e-8)
  # Normalize both scales to 0-1 range
  log_norm <- (log10(safe_vals) - log_range[1]) / diff(log_range)
  lin_norm <- (safe_vals - lin_range[1]) / diff(lin_range)
  # Blend between normalized positions using smooth easing
  blended <- (1 - mix) * log_norm + mix * lin_norm
  # Clamp to valid range
  pmin(pmax(blended, 0), 1)
}

theme_set(ggthemes::theme_fivethirtyeight(base_size = 14))

### Load and prepare data ------------------------------------------------

cols_to_keep <- c("Model", "Publication date", "Training compute (FLOP)")

models1 <- read_csv("data/AI Models public table.csv", show_col_types = FALSE) %>%
  dplyr::select(all_of(cols_to_keep))
models2 <- read_csv("data/Epoch Database - Notable Models.csv", show_col_types = FALSE) %>%
  dplyr::select(all_of(cols_to_keep))
models3 <- read_csv("data/Frontier models.csv", show_col_types = FALSE) %>%
  dplyr::select(all_of(cols_to_keep))
models4 <- read_csv("data/Known and unconfirmed compute-intensive models (public).csv", show_col_types = FALSE) %>%
  dplyr::select(all_of(cols_to_keep))

models_merge <- bind_rows(models1, models2, models3, models4) %>%
  distinct()

yr <- 2010
yr_start <- as.Date(sprintf("%s-01-01", yr))

models_filtered <- models_merge %>%
  filter(
    !is.na(`Publication date`),
    !is.na(`Training compute (FLOP)`),
    `Training compute (FLOP)` > 0,
    `Publication date` >= yr_start
  ) %>%
  mutate(
    year_numeric = as.numeric(format(`Publication date`, "%Y")) +
      as.numeric(format(`Publication date`, "%j")) / 365.25,
    log_flop = log10(`Training compute (FLOP)`)
  )

frontier_models <- models_filtered %>%
  mutate(period = floor(year_numeric * 4) / 4) %>% # approx quarter
  group_by(period) %>%
  slice_max(`Training compute (FLOP)`, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(year_numeric)

lm_model_frontier <- lm(log_flop ~ year_numeric, data = frontier_models)
lm_model_all <- lm(log_flop ~ year_numeric, data = models_filtered)

# Calculate ranges BEFORE creating prediction data
log_range <- range(models_filtered$log_flop, na.rm = TRUE)
lin_range <- range(models_filtered$`Training compute (FLOP)`, na.rm = TRUE)
max_flop <- max(models_filtered$`Training compute (FLOP)`, na.rm = TRUE)

pred_data_frontier <- tibble(year_numeric = seq(yr, 2026, by = 0.01)) %>%
  mutate(
    log_flop = predict(lm_model_frontier, newdata = .),
    flop = 10^log_flop,
    date = as.Date(paste0(floor(year_numeric), "-01-01"), format = "%Y-%m-%d") +
      (year_numeric - floor(year_numeric)) * 365.25
  )

pred_data_all <- tibble(year_numeric = seq(yr, 2026, by = 0.01)) %>%
  mutate(
    log_flop = predict(lm_model_all, newdata = .),
    flop = 10^log_flop,
    date = as.Date(paste0(floor(year_numeric), "-01-01"), format = "%Y-%m-%d") +
      (year_numeric - floor(year_numeric)) * 365.25
  )

### Hybrid frames --------------------------------------------------------

frame_schedule <- tibble(frame_id = 1:210) %>%
  mutate(
    mix = case_when(
      frame_id <= 40 ~ 0,
      frame_id <= 150 ~ scales::rescale(frame_id, to = c(0, 1), from = c(41, 150)),
      TRUE ~ 1
    ),
    phase = case_when(
      frame_id <= 40 ~ "Log axis focus",
      frame_id <= 150 ~ "Morphing log → linear",
      TRUE ~ "Linear axis focus"
    ),
    frame_label = sprintf("%s | frame %03d", phase, frame_id),
    pred_source = if_else(mix < 0.55, "all", "frontier")
  )

palette_fn <- grDevices::colorRampPalette(c(tableau["Blue"], tableau["Red"]))
frame_schedule <- frame_schedule %>%
  mutate(trend_color = palette_fn(n()))

expand_with_pred <- function(schedule_df, pred_df) {
  if (nrow(schedule_df) == 0) {
    return(tibble())
  }
  schedule_df %>%
    mutate(join_id = 1) %>%
    inner_join(pred_df %>% mutate(join_id = 1), by = "join_id") %>%
    select(-join_id)
}

models_combined <- models_filtered %>%
  crossing(frame_schedule) %>%
  mutate(
    y_pos = blend_position(`Training compute (FLOP)`, mix, log_range, lin_range)
  )

frontier_combined <- frontier_models %>%
  crossing(frame_schedule) %>%
  mutate(
    y_pos = blend_position(`Training compute (FLOP)`, mix, log_range, lin_range)
  )

pred_combined <- bind_rows(
  expand_with_pred(filter(frame_schedule, pred_source == "all"), pred_data_all),
  expand_with_pred(filter(frame_schedule, pred_source == "frontier"), pred_data_frontier)
) %>%
  mutate(
    y_pos = blend_position(flop, mix, log_range, lin_range)
  )

axis_values <- unique(c(10^seq(13, 23, by = 2), max_flop))
short_label <- scales::label_number(accuracy = 0.1, scale_cut = scales::cut_short_scale())

axis_guides <- frame_schedule %>%
  crossing(axis_value = axis_values) %>%
  mutate(
    axis_value = pmin(axis_value, max_flop),
    y_pos = blend_position(axis_value, mix, log_range, lin_range),
    label = if_else(
      mix <= 0.05,
      paste0("10^", round(log10(axis_value))),
      short_label(axis_value)
    )
  )

phase_labels <- frame_schedule %>%
  group_by(phase) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(frame_label, phase_text = phase)

### Animation ------------------------------------------------------------

base_date_left <- as.Date("2009-06-01")
text_offset <- as.Date("2008-09-01")

p_anim <- ggplot(
  models_combined,
  aes(x = `Publication date`, y = y_pos, group = interaction(Model, frame_id))
) +
  geom_segment(
    data = axis_guides,
    aes(
      x = base_date_left,
      xend = as.Date("2025-06-01"),
      y = y_pos,
      yend = y_pos
    ),
    inherit.aes = FALSE,
    color = COLORS$grid_light,
    linewidth = SIZES$line_grid
  ) +
  geom_text(
    data = axis_guides,
    aes(
      x = text_offset,
      y = y_pos,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = 1,
    size = FONT_SIZES$axis_label_small,
    color = FONT_COLORS$annotation
  ) +
  geom_point(
    color = COLORS$primary,
    alpha = ALPHA$points_all,
    size = SIZES$point
  ) +
  geom_point(
    data = frontier_combined,
    aes(x = `Publication date`, y = y_pos),
    inherit.aes = FALSE,
    color = COLORS$frontier,
    size = SIZES$point_frontier,
    shape = 8,
    stroke = SIZES$stroke_frontier
  ) +
  geom_line(
    data = pred_combined,
    aes(x = date, y = y_pos, color = trend_color, group = interaction(frame_id, pred_source)),
    inherit.aes = FALSE,
    linewidth = SIZES$line_trend,
    lineend = "round"
  ) +
  geom_text(
    data = phase_labels,
    aes(
      x = as.Date("2010-06-01"),
      y = 1.02,
      label = phase_text
    ),
    inherit.aes = FALSE,
    size = FONT_SIZES$annotation,
    color = FONT_COLORS$annotation
  ) +
  annotate("text",
           x = as.Date("2023-06-01"),
           y = 0.5,
           label = "Frontier growth:\n4.3x/year",
           color = COLORS$frontier,
           size = FONT_SIZES$annotation,
           fontface = "bold",
           lineheight = 0.9,
           alpha = ALPHA$annotation) +
  scale_color_identity() +
  scale_y_continuous(
    limits = c(0, 1.05),
    expand = expansion(mult = c(0, 0.02)),
    breaks = NULL,
    labels = NULL
  ) +
  scale_x_date(
    breaks = seq(as.Date("2010-01-01"), as.Date("2028-01-01"), by = "1 year"),
    date_labels = "%Y",
    limits = c(as.Date("2008-06-01"), as.Date("2027-01-01"))
  ) +
  labs(
    x = "Publication date",
    y = "Normalized axis (0 = log emphasis, 1 = linear emphasis)",
    title = "AI Training Compute: Log to Linear Scale Transition",
    subtitle = "{current_frame}",
    caption = "Source: Epoch AI"
  ) +
  apply_aigrowth_theme() +
  transition_manual(frame_label) +
  ease_aes("cubic-in-out") +
  enter_fade() +
  exit_fade()

anim <- animate(
  p_anim,
  nframes = nrow(frame_schedule),
  fps = 30,
  width = 1100,
  height = 600,
  renderer = gifski_renderer()
)
anim_save("outputs/training_compute_transition.gif", animation = anim)


