# ============================================================================
# Moore's Law: CPU and GPU Transistor Count Analysis
# ============================================================================
# Visualizes transistor count growth for CPUs and GPUs, comparing actual
# growth rates to Moore's Law predictions (2x every 2 years).

source("R/01_setup.R")

# ============================================================================
# SECTION 1: CPU Transistor Counts (Moore's Law)
# ============================================================================

moore <- read_excel("moore.xlsx", sheet = "transistors",
                    col_types = c("text", "numeric", "numeric",
                                  "text", "text", "text", "text"))

moore <- moore %>%
  mutate(
    Yearnum = as.numeric(Year),
    Year = as.Date(paste0(Year, "-01-01")),
    t_count = `Transistor count`,
    t_log = log10(`Transistor count`)
  )

# Fit trend line
lm_model_cpu <- lm(t_log ~ Year, data = moore)

min_x <- min(moore$Year)
max_x <- max(moore$Year)
min_y <- min(moore$t_count, na.rm = TRUE)
max_y <- max(moore$t_count, na.rm = TRUE)

pred_data_cpu <- data.frame(
  Year = seq(min_x, max_x, by = 30)
)
pred_data_cpu$t_log <- predict(lm_model_cpu, newdata = pred_data_cpu)
pred_data_cpu$t_count <- 10^pred_data_cpu$t_log
pred_data_cpu$date <- as.Date(paste0(pred_data_cpu$Year, "-01-01"), format = "%Y-%m-%d")

slope_cpu <- coef(lm_model_cpu)[2]
growth_per_year_cpu <- 10^slope_cpu
cpu_cagr <- ((max_y / min_y)^(1 / (max(moore$Yearnum) - min(moore$Yearnum))) - 1) * 100

# Create CPU visualization
plot_moore <- ggplot() +
  geom_point(data = moore,
             aes(x = Year, y = t_count),
             color = tableau["Blue"],
             alpha = 0.4,
             size = 3) +
  geom_line(data = pred_data_cpu,
            aes(x = date, y = t_count),
            color = tableau["Blue"],
            size = 1.2) +
  scale_y_log10(
    breaks = 10^seq(floor(log10(min_y)), ceiling(log10(max_y)), by = 1),
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    limits = c(floor(min_y), ceiling(max_y))
  ) +
  scale_x_date(
    breaks = seq(as.Date(min_x), as.Date(max_x), by = "5 years"),
    date_labels = "%Y",
    limits = c(as.Date(min_x), as.Date(max_x))
  ) +
  labs(
    x = "Year",
    y = "Transistor counts",
    title = "Transistor counts in CPUs seem to be following Moore's Law and\ndoubling nearly every 2 years, as forecasted",
    subtitle = "Implied CAGR: 39.53%",
    caption = "Source: Wikipedia"
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

print(plot_moore)

ggsave(
  file.path(output_dir, "moore.png"),
  plot_moore,
  width = 11,
  height = 6,
  dpi = 300
)

cat("✓ Saved: moore.png\n")

# ============================================================================
# SECTION 2: GPU Transistor Counts by Designer
# ============================================================================

gpu <- read_excel("moore.xlsx", sheet = "GPUs")

gpu <- gpu %>%
  mutate(
    Yearnum = as.numeric(Year),
    Year = as.Date(paste0(Year, "-01-01")),
    t_count = `Transistor count`,
    t_log = log10(`Transistor count`),
    desi = ifelse(`Designer(s)` %in% c("Nvidia", "AMD"), `Designer(s)`, "others"),
    manu = ifelse(`Fab(s)` %in% c("TSMC"), `Fab(s)`, "others")
  )

# Fit trend line (using all GPU data)
lm_model_gpu <- lm(t_log ~ Year, data = gpu)

min_x_gpu <- min(gpu$Year)
max_x_gpu <- max(gpu$Year)
min_y_gpu <- min(gpu$t_count, na.rm = TRUE)
max_y_gpu <- max(gpu$t_count, na.rm = TRUE)

pred_data_gpu <- data.frame(
  Year = seq(min_x_gpu, max_x_gpu, by = 30)
)
pred_data_gpu$t_log <- predict(lm_model_gpu, newdata = pred_data_gpu)
pred_data_gpu$t_count <- 10^pred_data_gpu$t_log
pred_data_gpu$date <- as.Date(paste0(pred_data_gpu$Year, "-01-01"), format = "%Y-%m-%d")

gpu_cagr <- ((max_y_gpu / min_y_gpu)^(1 / (max(gpu$Yearnum) - min(gpu$Yearnum))) - 1) * 100

# Create GPU by designer visualization
plot_gpu <- ggplot() +
  geom_point(
    data = gpu,
    aes(x = Year, y = t_count, color = desi, fill = desi, shape = desi),
    size = 3,
    stroke = 1.2
  ) +
  geom_line(
    data = pred_data_gpu,
    aes(x = date, y = t_count),
    color = tableau["Blue"],
    size = 1.2
  ) +
  scale_shape_manual(values = c("Nvidia" = 21, "AMD" = 21, "others" = 21)) +
  scale_fill_manual(values = c("Nvidia" = NA, "AMD" = NA, "others" = NA)) +
  scale_color_manual(values = c("Nvidia" = "#59A14F", "AMD" = "#F28E2B", "others" = "#BAB0AC")) +
  scale_y_log10(
    breaks = 10^seq(floor(log10(min_y_gpu)), ceiling(log10(max_y_gpu)), by = 1),
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    limits = c(floor(min_y_gpu), ceiling(max_y_gpu))
  ) +
  scale_x_date(
    breaks = seq(as.Date(min_x_gpu), as.Date(max_x_gpu), by = "5 years"),
    date_labels = "%Y",
    limits = c(as.Date(min_x_gpu), as.Date(max_x_gpu))
  ) +
  labs(
    x = "Year",
    y = "Transistor counts",
    title = "Transistor counts in GPUs are growing faster than Moore's Law\nwould predict and the GPU market is dominated by Nvidia and AMD",
    subtitle = "Implied CAGR: 45.44%, exceeding Moore's Law by a small amount",
    caption = "Source: Wikipedia",
    color = "GPU designer", fill = "GPU designer", shape = "GPU designer"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(margin = margin(b = 6), size = 12, color = "#999999"),
    plot.caption = element_text(hjust = 0, size = 10, color = "#666666"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  ) +
  scale_fill_manual(values = c("Nvidia" = "#59A14F", "AMD" = "#F28E2B", "others" = rgb(1, 1, 1, alpha = 0)))

print(plot_gpu)

ggsave(
  file.path(output_dir, "gpu.png"),
  plot_gpu,
  width = 11,
  height = 6,
  dpi = 300
)

cat("✓ Saved: gpu.png\n")

# ============================================================================
# SECTION 3: GPU Transistor Counts by Manufacturer
# ============================================================================

plot_tsmc <- ggplot() +
  geom_point(
    data = gpu,
    aes(x = Year, y = t_count, color = manu, fill = manu, shape = manu),
    size = 3,
    stroke = 1.2
  ) +
  geom_line(
    data = pred_data_gpu,
    aes(x = date, y = t_count),
    color = tableau["Blue"],
    size = 1.2
  ) +
  scale_shape_manual(values = c("TSMC" = 21, "others" = 21)) +
  scale_fill_manual(values = c("TSMC" = NA, "others" = NA)) +
  scale_color_manual(values = c("TSMC" = "#F28E2B", "others" = "#BAB0AC")) +
  scale_y_log10(
    breaks = 10^seq(floor(log10(min_y_gpu)), ceiling(log10(max_y_gpu)), by = 1),
    labels = scales::trans_format("log10", scales::math_format(10^.x)),
    limits = c(floor(min_y_gpu), ceiling(max_y_gpu))
  ) +
  scale_x_date(
    breaks = seq(as.Date(min_x_gpu), as.Date(max_x_gpu), by = "5 years"),
    date_labels = "%Y",
    limits = c(as.Date(min_x_gpu), as.Date(max_x_gpu))
  ) +
  labs(
    x = "Year",
    y = "Transistor counts",
    title = "If we look at the manufacturer of the semiconductors, the market\nconcentration is even higher and dominated by TSMC",
    subtitle = "Implied CAGR: 45.44%, exceeding Moore's Law by a small amount",
    caption = "Source: Wikipedia",
    color = "GPU manufacturer", fill = "GPU manufacturer", shape = "GPU manufacturer"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(margin = margin(b = 6), size = 12, color = "#999999"),
    plot.caption = element_text(hjust = 0, size = 10, color = "#666666"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  ) +
  scale_fill_manual(values = c("TSMC" = "#F28E2B", "others" = rgb(1, 1, 1, alpha = 0)))

print(plot_tsmc)

ggsave(
  file.path(output_dir, "gpu_tsmc.png"),
  plot_tsmc,
  width = 11,
  height = 6,
  dpi = 300
)

cat("✓ Saved: gpu_tsmc.png\n")
