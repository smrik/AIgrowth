# ============================================================================
# Animation: Log ↔ Linear Scale Transition
# ============================================================================
# Creates a smooth animated transition between logarithmic and linear scales
# showing AI compute growth. The animation morphs between two visual spaces
# while maintaining data continuity.
#
# Animation phases:
# 1. Frames 1-40:   Focus on log scale representation
# 2. Frames 41-150: Smoothly morph log → linear
# 3. Frames 151-210: Focus on linear scale representation

source("R/01_setup.R")

library(gganimate)
library(gifski)

# ============================================================================
# Load and prepare data
# ============================================================================

models_merge <- load_all_ai_models()

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

# Get frontier models (top model per quarter)
frontier_models <- models_filtered %>%
  mutate(period = floor(year_numeric * 4) / 4) %>%
  group_by(period) %>%
  slice_max(`Training compute (FLOP)`, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(year_numeric)

# Fit trend lines to both frontier and all models
lm_model_frontier <- lm(log_flop ~ year_numeric, data = frontier_models)
lm_model_all <- lm(log_flop ~ year_numeric, data = models_filtered)

# Generate prediction data for trend lines
pred_data_frontier <- tibble(year_numeric = seq(yr, 2027, by = 0.01)) %>%
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

# Calculate value ranges for blending calculation
log_range <- range(models_filtered$log_flop, na.rm = TRUE)
lin_range <- range(models_filtered$`Training compute (FLOP)`, na.rm = TRUE)
max_flop <- max(models_filtered$`Training compute (FLOP)`, na.rm = TRUE)

# ============================================================================
# Create frame schedule
# ============================================================================

frame_schedule <- tibble(frame_id = 1:210) %>%
  mutate(
    # mix: 0 = pure log space, 1 = pure linear space
    mix = case_when(
      frame_id <= 40 ~ 0,
      frame_id <= 150 ~ scales::rescale(frame_id, to = c(0, 1), from = c(41, 150)),
      TRUE ~ 1
    ),
    # Phase description for viewer
    phase = case_when(
      frame_id <= 40 ~ "Log axis focus",
      frame_id <= 150 ~ "Morphing log → linear",
      TRUE ~ "Linear axis focus"
    ),
    frame_label = sprintf("%s | frame %03d", phase, frame_id),
    # Switch prediction source when past midpoint of transition
    pred_source = if_else(mix < 0.55, "all", "frontier")
  )

# Create color palette transitioning from cyan to red
palette_fn <- grDevices::colorRampPalette(c("#3FB8AF", "#FF6B6B"))
frame_schedule <- frame_schedule %>%
  mutate(trend_color = palette_fn(n()))

# ============================================================================
# Prepare combined data for animation
# ============================================================================

# Helper function to expand frame schedule with prediction data
expand_with_pred <- function(schedule_df, pred_df) {
  if (nrow(schedule_df) == 0) {
    return(tibble())
  }
  schedule_df %>%
    mutate(join_id = 1) %>%
    inner_join(pred_df %>% mutate(join_id = 1), by = "join_id") %>%
    select(-join_id)
}

# Combine models with frames and calculate blended y positions
models_combined <- models_filtered %>%
  crossing(frame_schedule) %>%
  mutate(
    y_pos = blend_position(`Training compute (FLOP)`, mix, log_range, lin_range)
  )

# Frontier models with same blending
frontier_combined <- frontier_models %>%
  crossing(frame_schedule) %>%
  mutate(
    y_pos = blend_position(`Training compute (FLOP)`, mix, log_range, lin_range)
  )

# Prediction lines with blending
pred_combined <- bind_rows(
  expand_with_pred(filter(frame_schedule, pred_source == "all"), pred_data_all),
  expand_with_pred(filter(frame_schedule, pred_source == "frontier"), pred_data_frontier)
) %>%
  mutate(
    y_pos = blend_position(flop, mix, log_range, lin_range)
  )

# Create axis guide lines and labels
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

# Phase text for display
phase_labels <- frame_schedule %>%
  transmute(frame_label, phase_text = phase)

# ============================================================================
# Create animation
# ============================================================================

base_date_left <- as.Date("2009-06-01")
text_offset <- as.Date("2008-09-01")

p_anim <- ggplot(
  models_combined,
  aes(x = `Publication date`, y = y_pos, group = interaction(Model, frame_id))
) +
  # Background axis guides
  geom_segment(
    data = axis_guides,
    aes(
      x = base_date_left,
      xend = as.Date("2027-03-01"),
      y = y_pos,
      yend = y_pos
    ),
    inherit.aes = FALSE,
    color = "grey92",
    linewidth = 0.2
  ) +
  # Axis labels
  geom_text(
    data = axis_guides,
    aes(
      x = text_offset,
      y = y_pos,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = 1,
    size = 3,
    color = "#6e6e6e"
  ) +
  # All model points
  geom_point(
    color = "#3FB8AF",
    alpha = 0.28,
    size = 3
  ) +
  # Frontier models highlighted
  geom_point(
    data = frontier_combined,
    aes(x = `Publication date`, y = y_pos),
    inherit.aes = FALSE,
    color = "#FF6B6B",
    size = 4,
    shape = 8,
    stroke = 1.2
  ) +
  # Trend line
  geom_line(
    data = pred_combined,
    aes(x = date, y = y_pos, color = trend_color, group = interaction(frame_id, pred_source)),
    inherit.aes = FALSE,
    linewidth = 1.4,
    lineend = "round"
  ) +
  # Phase text label
  geom_text(
    data = phase_labels,
    aes(
      x = as.Date("2010-06-01"),
      y = 1.02,
      label = phase_text
    ),
    inherit.aes = FALSE,
    size = 5,
    color = "#444444"
  ) +
  # Visual settings
  scale_color_identity() +
  scale_y_continuous(
    limits = c(0, 1.05),
    expand = expansion(mult = c(0, 0.02)),
    breaks = NULL,
    labels = NULL
  ) +
  scale_x_date(
    breaks = seq(as.Date("2010-01-01"), as.Date("2026-01-01"), by = "2 years"),
    date_labels = "%Y",
    limits = c(as.Date("2008-06-01"), as.Date("2027-06-01"))
  ) +
  labs(
    x = "Publication date",
    y = "Normalized axis (0 = log emphasis, 1 = linear emphasis)",
    title = "Training compute: log ↔ linear scale transition",
    subtitle = "{current_frame}",
    caption = "Source: Epoch AI"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(margin = margin(b = 6), size = 12, color = "#777777"),
    plot.caption = element_text(hjust = 0, size = 10, color = "#666666"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  ) +
  transition_manual(frame_label) +
  ease_aes("cubic-in-out") +
  enter_fade() +
  exit_fade()

# ============================================================================
# Render animation
# ============================================================================

cat("Rendering animation... this may take a minute or two.\n")

anim <- animate(
  p_anim,
  nframes = nrow(frame_schedule),
  fps = 30,
  width = 1100,
  height = 600,
  renderer = gifski_renderer(),
  verbose = FALSE
)

anim_save(
  file.path(output_dir, "training_compute_transition.gif"),
  animation = anim
)

cat("✓ Saved: training_compute_transition.gif\n")
