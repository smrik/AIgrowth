---
created: 2025-11-30T00:53
updated: 2025-11-30T00:53
---
# AIgrowth: AI Compute Growth Visualization

A comprehensive R visualization project exploring the exponential growth of AI model training compute requirements.

## Project Structure

```
AIgrowth/
├── R/
│   └── hello.R                 # Main analysis script (5 visualization sections)
├── data/                       # Input CSV and XLSX files
│   ├── AI Models public table.csv
│   ├── Epoch Database - Notable Models.csv
│   ├── Frontier models.csv
│   ├── Known and unconfirmed compute-intensive models (public).csv
│   └── moore.xlsx
├── outputs/                    # Generated plots and animations
├── archived_plots/             # Old plot versions
├── archived_animations/        # Old animation versions
└── README.md                   # This file
```

## Quick Start

Run the entire analysis:

```r
source("R/hello.R")
```

This generates 5 outputs in the `outputs/` folder:
1. **AI_Compute_Growth.png** - Historical era comparison
2. **training_compute_modern_models.png** - All models (log scale, 2010+)
3. **training_compute_notable_models_linear_frontier.png** - 90th percentile models
4. **training_compute_notable_models_linear_absolute_frontier.png** - Top model per quarter
5. **training_compute_transition.gif** - Log ↔ Linear scale animation

## Visualizations

### Section 1: Historical Growth (All Eras)
Compares AI compute growth in Pre-Deep Learning Era vs Deep Learning Era (2010 cutoff).
- Shows CAGR (Compound Annual Growth Rate) for each era
- Pre-2010: ~1.8x/year | Post-2010: ~4.3x/year

### Section 2: Modern Models - Log Scale
All 443 notable AI models from 2010 onwards on logarithmic scale.
- Exponential trend line showing 4.3x annual growth
- Key models labeled: AlexNet, AlphaGo Zero, GPT-3/4, PaLM, Gemini Ultra, Grok

### Section 3: Frontier Models - 90th Percentile
Top 90th percentile models per year on linear scale.
- Orange circles highlight frontier models
- Shows exponential curve on linear axis
- Emphasizes year-over-year frontier advancement

### Section 4: Absolute Frontier - Quarterly Top
Single highest-compute model per quarter on linear scale.
- Red stars mark absolute frontier models
- Steepest growth trajectory
- Tracks cutting edge of AI development

### Section 5: Animation
Interactive 210-frame animation morphing between log and linear scales.
- **Frames 1-40**: Log scale focus
- **Frames 41-150**: Smooth morphing transition
- **Frames 151-210**: Linear scale focus

## Color Scheme

Uses **Tableau 10 palette** from FiveThirtyEight theme:
- **Blue**: Main data points
- **Red**: Frontier/highlight points
- **Orange**: Secondary frontier indicators

## Styling

All plots follow consistent styling:
- **Theme**: `ggthemes::theme_fivethirtyeight(base_size = 14)`
- **Dimensions**: 11" × 6" at 300 DPI
- **Typography**: Bold titles (20pt), gray subtitles (12pt), captions (10pt)

## Data Files

Located in `data/` folder:

| File | Rows | Purpose |
|------|------|---------|
| AI Models public table.csv | 22,736 | Comprehensive AI model catalog |
| Epoch Database - Notable Models.csv | 8,039 | Curated notable models |
| Frontier models.csv | 1,695 | Cutting-edge models only |
| Known and unconfirmed compute-intensive models.csv | 3,931 | Speculative high-compute models |
| moore.xlsx | - | Moore's Law reference data |

## Key Findings

1. **Acceleration**: AI compute growth accelerated dramatically with deep learning (2010+)
   - Pre-2010: ~1.8x/year
   - Post-2010: ~4.3x/year

2. **Frontier Dynamics**: Frontier models push compute boundaries further each year
   - Absolute frontier (top model/quarter) shows steepest trajectory
   - 90th percentile maintains consistent exponential trend

3. **Scale Insights**: Different scales reveal different patterns
   - Log scale: Shows consistency of exponential growth
   - Linear scale: Highlights acceleration and absolute magnitude

## Dependencies

```r
library(readr)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(gganimate)     # Animation only
library(gifski)        # Animation only
library(scales)
```

## Notes

- Run the full script takes ~1-2 minutes (animation is computationally intensive)
- All plots are publication-ready (300 DPI)
- Data current as of November 2025
- Outputs are version-controlled via `.gitignore` (excluded from git)

## File Organization

- **Code stays clean**: Only `R/hello.R` in root
- **Data organized**: All CSVs in `data/` folder
- **Outputs isolated**: Generated files in `outputs/` folder
- **History preserved**: Old versions in `archived_*` folders

---

**Last Updated**: November 30, 2025
**Theme**: FiveThirtyEight
**Data Sources**: Epoch AI, Wikipedia, Stanford AI Index
