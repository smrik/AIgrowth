# ============================================================================
# Frontier Analysis: Linear Scale with Exponential Trend
# ============================================================================
# Shows AI compute growth on linear scale with two variants:
# 1. Top 90th percentile models per year (p1)
# 2. Absolute top model per quarter (p)

source("R/01_setup.R")

# Load all models data
all_models <- load_all_ai_models()

models_filtered <- all_models %>%
  filter(!is.na(`Training compute (FLOP)`),
         !is.na(`Publication date`),
         `Publication date` >= as.Date("2010-01-01")) %>%
  mutate(
    year_numeric = as.numeric(format(`Publication date`, "%Y")) +
      as.numeric(format(`Publication date`, "%j")) / 365.25,
    log_flop = log10(`Training compute (FLOP)`)
  )

# ============================================================================
# VARIANT 1: Top 90th percentile per year
# ============================================================================

frontier_models_v1 <- models_filtered %>%
  group_by(year = floor(year_numeric)) %>%
  filter(`Training compute (FLOP)` >= quantile(`Training compute (FLOP)`, 0.90, na.rm = TRUE)) %>%
  ungroup()

lm_model_v1 <- lm(log_flop ~ year_numeric, data = frontier_models_v1)

pred_data_v1 <- data.frame(
  year_numeric = seq(2010, 2026, by = 0.01)
)
pred_data_v1$log_flop <- predict(lm_model_v1, newdata = pred_data_v1)
pred_data_v1$flop <- 10^pred_data_v1$log_flop
pred_data_v1$date <- as.Date(paste0(floor(pred_data_v1$year_numeric), "-01-01"), format = "%Y-%m-%d") +
  (pred_data_v1$year_numeric - floor(pred_data_v1$year_numeric)) * 365.25

slope_v1 <- coef(lm_model_v1)[2]
growth_per_year_v1 <- 10^slope_v1

# Select key models for labeling
label_models_v1 <- models_filtered %>%
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

max_flop <- max(models_filtered$`Training compute (FLOP)`, na.rm = TRUE)

# Create visualization - Frontier variant (90th percentile)
p1 <- ggplot() +
  geom_point(data = models_filtered,
             aes(x = `Publication date`, y = `Training compute (FLOP)`),
             color = tableau["Blue"],
             alpha = 0.4,
             size = 3) +
  # Highlight frontier models
  geom_point(data = frontier_models_v1,
             aes(x = `Publication date`, y = `Training compute (FLOP)`),
             color = tableau["Orange"],
             alpha = 0.6,
             size = 2,
             shape = 1,
             stroke = 1.5) +
  # Exponential trend line
  geom_line(data = pred_data_v1,
            aes(x = date, y = flop),
            color = tableau["Orange"],
            size = 1.5,
            lineend = "round") +
  # Model labels
  geom_text_repel(
    data = label_models_v1,
    aes(x = `Publication date`,
        y = `Training compute (FLOP)`,
        label = label_text),
    size = 4.5,
    color = "#666666",
    fontface = "bold",
    segment.color = "#666666",
    segment.size = 0.5,
    arrow = arrow(length = unit(2, "mm")),
    box.padding = 1.5,
    point.padding = 0.8,
    min.segment.length = 0,
    max.overlaps = Inf,
    force = 5,
    force_pull = 0.02,
    direction = "both",
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    xlim = c(NA, as.Date("2027-03-01")),
    seed = 42
  ) +
  # Growth annotation
  annotate("text",
           x = as.Date("2023-06-01"),
           y = max_flop * 0.45,
           label = paste0(round(growth_per_year_v1, 1), "x/year\n(frontier models)"),
           color = tableau["Orange"],
           size = 5.5,
           fontface = "bold",
           lineheight = 0.9) +
  scale_y_continuous(
    labels = scales::label_scientific(digits = 1),
    expand = expansion(mult = c(0.01, 0.15))
  ) +
  scale_x_date(
    breaks = seq(as.Date("2010-01-01"), as.Date("2026-01-01"), by = "2 years"),
    date_labels = "%Y",
    limits = c(as.Date("2010-01-01"), as.Date("2027-03-01"))
  ) +
  labs(
    x = "Publication date",
    y = "Training compute (FLOP)",
    title = "Training compute of modern models has been increasing\nat a rate of 4.3 times per year",
    subtitle = "Training compute of notable models, 443 models total (exponential trend follows frontier models)",
    caption = "Source: Epoch AI. Orange circles indicate frontier models used for trend line calculation."
  ) +
  scale_colour_tableau() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(margin = margin(b = 6), size = 12, color = "#999999"),
    plot.caption = element_text(hjust = 0, size = 10, color = "#666666"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )

print(p1)

ggsave(
  file.path(output_dir, "training_compute_notable_models_linear_frontier.png"),
  p1,
  width = 11,
  height = 6,
  dpi = 300
)

cat("✓ Saved: training_compute_notable_models_linear_frontier.png\n")

# ============================================================================
# VARIANT 2: Absolute top model per quarter
# ============================================================================

frontier_models_abs <- models_filtered %>%
  mutate(quarter = floor(year_numeric * 1) / 1) %>%
  group_by(quarter) %>%
  slice_max(`Training compute (FLOP)`, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(year_numeric)

lm_model_abs <- lm(log_flop ~ year_numeric, data = frontier_models_abs)

pred_data_abs <- data.frame(
  year_numeric = seq(2010, 2027, by = 0.001)
)
pred_data_abs$log_flop <- predict(lm_model_abs, newdata = pred_data_abs)
pred_data_abs$flop <- 10^pred_data_abs$log_flop
pred_data_abs$date <- as.Date(paste0(floor(pred_data_abs$year_numeric), "-01-01"), format = "%Y-%m-%d") +
  (pred_data_abs$year_numeric - floor(pred_data_abs$year_numeric)) * 365.25

slope_abs <- coef(lm_model_abs)[2]
growth_per_year_abs <- 10^slope_abs

# Create visualization - Absolute frontier (top model per quarter)
p_abs <- ggplot() +
  geom_point(data = models_filtered,
             aes(x = `Publication date`, y = `Training compute (FLOP)`),
             color = "#3FB8AF",
             alpha = 0.3,
             size = 3) +
  # Highlight absolute top models with red stars
  geom_point(data = frontier_models_abs,
             aes(x = `Publication date`, y = `Training compute (FLOP)`),
             color = "#FF6B6B",
             size = 4,
             shape = 8,
             stroke = 1.5) +
  # Exponential trend line
  geom_line(data = pred_data_abs,
            aes(x = date, y = flop),
            color = "#FF6B6B",
            size = 1.5,
            lineend = "round") +
  # Model labels
  geom_text_repel(
    data = label_models_v1,
    aes(x = `Publication date`,
        y = `Training compute (FLOP)`,
        label = label_text),
    size = 4.5,
    color = "#333333",
    fontface = "bold",
    segment.color = "#555555",
    segment.size = 0.5,
    arrow = arrow(length = unit(2, "mm")),
    box.padding = 1.5,
    point.padding = 0.8,
    min.segment.length = 0,
    max.overlaps = Inf,
    force = 5,
    force_pull = 0.02,
    direction = "both",
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    xlim = c(NA, as.Date("2027-03-01")),
    seed = 42
  ) +
  scale_y_continuous(
    labels = scales::label_scientific(digits = 1),
    expand = expansion(mult = c(0.01, 0.15)),
    limits = c(0, max_flop)
  ) +
  scale_x_date(
    breaks = seq(as.Date("2015-01-01"), as.Date("2026-01-01"), by = "2 years"),
    date_labels = "%Y",
    limits = c(as.Date("2010-01-01"), as.Date("2026-03-01"))
  ) +
  labs(
    x = "Publication date",
    y = "Training compute (FLOP)",
    title = "Training compute of modern models has been increasing\nat a rate of 4.3 times per year",
    subtitle = "Training compute of notable models, 443 models total (trend follows top model each quarter)",
    caption = "Source: Epoch AI. Red stars indicate the highest-compute model in each 3-month period."
  ) +
  scale_colour_tableau() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(margin = margin(b = 6), size = 12, color = "#999999"),
    plot.caption = element_text(hjust = 0, size = 10, color = "#666666"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )

print(p_abs)

ggsave(
  file.path(output_dir, "training_compute_notable_models_linear_absolute_frontier.png"),
  p_abs,
  width = 11,
  height = 6,
  dpi = 300
)

cat("✓ Saved: training_compute_notable_models_linear_absolute_frontier.png\n")
