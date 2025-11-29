# ============================================================================
# AI Compute Growth: Pre-Deep Learning vs Deep Learning Era
# ============================================================================
# Compares compute growth before and after 2010, showing the inflection point
# when deep learning emerged and compute requirements began accelerating.

source("R/01_setup.R")

# Load and prepare data
Frontier_models <- read_csv("Epoch Database - Notable Models.csv", show_col_types = FALSE)

models <- Frontier_models %>%
  mutate(
    era = case_when(
      `Publication date` <= "2010-01-01" ~ "Pre-Deep Learning",
      TRUE ~ "Deep Learning Era"
    ),
    log_compute = log(`Training compute (FLOP)`),
    pub_date_numeric = as.numeric(`Publication date`)
  )

# ============================================================================
# Calculate statistics
# ============================================================================

break_year <- as.Date("2010-01-01")
xmin <- min(models$`Publication date`)
xmax <- max(models$`Publication date`)
ymax <- max(models$log_compute, na.rm = TRUE)

cagr_pre <- calc_cagr(filter(models, era == "Pre-Deep Learning"))
cagr_post <- calc_cagr(filter(models, era == "Deep Learning Era"))

pos_pre <- get_cagr_position_and_angle(models, "Pre-Deep Learning")
pos_post <- get_cagr_position_and_angle(models, "Deep Learning Era")

# Manual angle adjustments for better visual positioning
pos_pre$angle <- 17
pos_post$angle <- 41

# ============================================================================
# Create visualization
# ============================================================================

plot_all <- ggplot(models, aes(x = `Publication date`, y = log_compute)) +
  coord_cartesian(clip = "off") +
  # Shaded region highlighting Deep Learning Era
  annotate(
    "rect",
    xmin = break_year,
    xmax = xmax,
    ymin = 0,
    ymax = 65,
    fill = "gray80", alpha = 0.7
  ) +
  # Data points colored by era
  geom_point(aes(color = era), alpha = 0.5, size = 2) +
  # Trend lines for each era
  geom_smooth(data = filter(models, era == "Pre-Deep Learning"),
              aes(color = "Pre-Deep Learning"),
              se = FALSE,
              method = "lm",
              linewidth = 1.25) +
  geom_smooth(data = filter(models, era == "Deep Learning Era"),
              aes(color = "Deep Learning Era"),
              se = FALSE,
              method = "lm",
              linewidth = 1.25) +
  # Vertical line marking the era transition
  geom_vline(xintercept = break_year, linetype = "dashed", color = "gray30", linewidth = 1) +
  # Era labels
  annotate("text",
           x = as.Date("1970-06-01"), y = 65,
           label = "Pre-Deep Learning Era", color = tableau["Orange"],
           fontface = "bold", size = 5, hjust = 0) +
  annotate("text",
           x = as.Date("2011-01-01"), y = 65,
           label = "Deep Learning Era", color = tableau["Blue"],
           fontface = "bold", size = 5, hjust = 0) +
  # CAGR annotations
  annotate(
    "text",
    x = pos_pre$x,
    y = pos_pre$y,
    label = paste("CAGR:", cagr_pre),
    color = tableau["Orange"],
    angle = 0,
    fontface = "bold",
    size = 5,
    hjust = 0.5,
    vjust = 0.5
  ) +
  annotate(
    "text",
    x = pos_post$x - 1000,
    y = (pos_post$y - 10),
    label = paste("CAGR:", cagr_post),
    color = tableau["Blue"],
    angle = 0,
    fontface = "bold",
    size = 5,
    hjust = 0.5,
    vjust = 0.5
  ) +
  ylim(c(0, 65)) +
  labs(
    x = NULL,
    y = "Log(Training Compute, FLOP)",
    title = "AI Compute: Pre-Deep Learning vs Deep Learning Era",
    subtitle = "Shaded region: Deep Learning Era (>2010). CAGR shows annualized growth in each era.",
    caption = "Source: Epoch AI, Stanford AI Index"
  ) +
  ggthemes::scale_colour_tableau() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(margin = margin(b = 6), size = 12, color = "#999999"),
    plot.caption = element_text(hjust = 0, size = 10, color = "#666666"),
    axis.title.y = element_text(size = 12)
  )

print(plot_all)

# Save the plot
ggsave(
  file.path(output_dir, "AI_Compute_Growth.png"),
  plot_all,
  width = 11,
  height = 6,
  dpi = 300
)

cat("✓ Saved: AI_Compute_Growth.png\n")
