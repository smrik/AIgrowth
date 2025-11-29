# ============================================================================
# Training Compute of Modern Models (Log Scale)
# ============================================================================
# Visualizes AI models from 2010 onwards on logarithmic scale with exponential
# trend line showing ~4.3x annual growth.

source("R/01_setup.R")

# Load and filter data
Frontier_models <- read_csv("Epoch Database - Notable Models.csv", show_col_types = FALSE)

models_filtered <- Frontier_models %>%
  filter(!is.na(`Training compute (FLOP)`),
         !is.na(`Publication date`),
         `Publication date` >= as.Date("2010-01-01")) %>%
  mutate(
    year_numeric = as.numeric(format(`Publication date`, "%Y")) +
      as.numeric(format(`Publication date`, "%j")) / 365.25,
    log_flop = log10(`Training compute (FLOP)`)
  )

# ============================================================================
# Fit trend line
# ============================================================================

lm_model <- lm(log_flop ~ year_numeric, data = models_filtered)

pred_data <- data.frame(
  year_numeric = seq(2010, 2026, by = 0.1)
)
pred_data$log_flop <- predict(lm_model, newdata = pred_data)
pred_data$flop <- 10^pred_data$log_flop
pred_data$date <- as.Date(paste0(pred_data$year_numeric, "-01-01"), format = "%Y-%m-%d")

slope <- coef(lm_model)[2]
growth_per_year <- 10^slope

# ============================================================================
# Select key models for labeling
# ============================================================================

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

# ============================================================================
# Create visualization
# ============================================================================

p2 <- ggplot() +
  # All model points
  geom_point(data = models_filtered,
             aes(x = `Publication date`, y = `Training compute (FLOP)`),
             color = tableau["Blue"],
             alpha = 0.4,
             size = 3) +
  # Trend line
  geom_line(data = pred_data,
            aes(x = date, y = flop),
            color = tableau["Blue"],
            size = 1.2) +
  # Model labels with repulsion
  geom_text_repel(
    data = label_models,
    aes(x = `Publication date`,
        y = `Training compute (FLOP)`,
        label = label_text),
    size = 4,
    color = "#333333",
    fontface = "bold",
    segment.color = "#555555",
    segment.size = 0.5,
    arrow = arrow(length = unit(2, "mm")),
    box.padding = 2,
    point.padding = 1.0,
    min.segment.length = 0,
    max.overlaps = 5,
    force = 3,
    force_pull = 0.05,
    direction = "both",
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    xlim = c(NA, as.Date("2026-06-30")),
    seed = 1
  ) +
  # Growth rate annotation
  annotate("text",
           x = as.Date("2022-06-01"),
           y = 10^14,
           label = paste0(round(growth_per_year, 1), "x/year"),
           color = tableau["Blue"],
           size = 5,
           fontface = "bold") +
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
    title = "Training compute of modern models has been increasing\nat a rate of 4.3 times per year",
    subtitle = "Training compute of notable models, 443 models total in graph",
    caption = "Source: Epoch AI"
  ) +
  scale_colour_tableau() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(margin = margin(b = 6), size = 12, color = "#999999"),
    plot.caption = element_text(hjust = 0, size = 10, color = "#666666"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

print(p2)

# Save the plot
ggsave(
  file.path(output_dir, "training_compute_modern_models.png"),
  p2,
  width = 11,
  height = 6,
  dpi = 300
)

cat("✓ Saved: training_compute_modern_models.png\n")
