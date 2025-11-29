# ============================================================================
# Master Script: Generate All AIgrowth Visualizations
# ============================================================================
# This script sources and runs all visualization scripts in order.
# Generates: 8 PNG files + 1 GIF animation
# Total output size: ~6-7 MB
# Estimated runtime: 3-5 minutes (including animation)
#
# Usage:
#   source("generate_all_plots.R")

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║         AIgrowth: AI Compute Growth Visualizations                ║\n")
cat("║                                                                    ║\n")
cat("║  Generating all plots and animations...                           ║\n")
cat("║  Output directory: outputs/                                       ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

start_time <- Sys.time()

# ============================================================================
# Setup
# ============================================================================

cat("► Loading setup and configuration...\n")
source("R/01_setup.R")

# ============================================================================
# Generate visualizations in order
# ============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
cat("Visualization 1/5: AI Compute Era Comparison\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
source("R/02_ai_compute_era.R")

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
cat("Visualization 2/5: Modern Models (Log Scale)\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
source("R/03_modern_models.R")

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
cat("Visualization 3/5: Frontier Analysis (Linear Scale)\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
source("R/04_frontier_analysis.R")

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
cat("Visualization 4/5: Moore's Law Analysis\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
source("R/05_moores_law.R")

cat("\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
cat("Visualization 5/5: Animation (Log ↔ Linear Transition)\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
cat("⚠ Animation rendering is computationally intensive...\n")
cat("   This may take 1-2 minutes. Please be patient.\n\n")
source("R/06_animation.R")

# ============================================================================
# Summary
# ============================================================================

end_time <- Sys.time()
elapsed_time <- round(as.numeric(difftime(end_time, start_time, units = "secs")))
minutes <- elapsed_time %/% 60
seconds <- elapsed_time %% 60

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║                    ✓ ALL VISUALIZATIONS COMPLETE                 ║\n")
cat("║                                                                    ║\n")

# List generated files
output_files <- list.files("outputs", full.names = FALSE)
cat("║  Generated Files:                                                  ║\n")
for (file in sort(output_files)) {
  file_size <- file.info(file.path("outputs", file))$size
  size_mb <- round(file_size / 1024 / 1024, 2)
  cat(sprintf("║    ✓ %-50s %6.2f MB ║\n", file, size_mb))
}

cat("║                                                                    ║\n")
cat(sprintf("║  Execution Time: %d minutes, %d seconds                        ║\n", minutes, seconds))
cat("║  Output Location: outputs/                                        ║\n")
cat("║                                                                    ║\n")
cat("║  Ready to use in your blog posts!                                 ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# ============================================================================
# Summary statistics
# ============================================================================

cat("Summary Statistics:\n")
cat("─────────────────────────────────────────────────────────────────────\n")
cat("Static Plots (PNG):     9 files\n")
cat("Animations (GIF):       1 file (210 frames, 30 fps)\n")
cat("Theme:                  FiveThirtyEight (consistent across all)\n")
cat("Color Palette:          Tableau 10\n")
cat("Resolution:             1100 × 600 px (static), 300 DPI\n")
cat("Data Source:            Epoch AI, Wikipedia, Stanford AI Index\n")
cat("─────────────────────────────────────────────────────────────────────\n")
cat("\n")

cat("For more details, see:\n")
cat("  • README.md - Complete project documentation\n")
cat("  • REFACTORING_SUMMARY.md - Changes and improvements made\n")
cat("  • R/01_setup.R - Helper functions and utilities\n")
cat("\n")
