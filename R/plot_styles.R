# ==============================================================================
# AUTOMATED R PROJECT GENERATOR (Fixed)
# ==============================================================================

# 1. SETUP CONFIGURATION
# ------------------------------------------------------------------------------
# Set the base directory where you want new projects to live
$BaseDir = "C:\Projects\RScripts\Blog files"

# Set the location of your "Master Theme" file
# Ensure this path is correct based on your previous setup
$ThemeSource = "C:\Projects\RScripts\Blog files\Sample Directory\R\theme.R"

# ------------------------------------------------------------------------------

# 2. GET USER INPUT
# ------------------------------------------------------------------------------
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "   NEW R DATA JOURNALISM PROJECT WIZARD   " -ForegroundColor Cyan
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host ""

$ProjectName = Read-Host "Enter new project name (e.g., AI_Safety_Trends)"
if ([string]::IsNullOrWhiteSpace($ProjectName)) {
    Write-Error "Project name cannot be empty."
    exit
}

$ProjectRoot = Join-Path -Path $BaseDir -ChildPath $ProjectName

# Check if it already exists
if (Test-Path $ProjectRoot) {
    Write-Warning "Directory '$ProjectName' already exists. Aborting to prevent overwrite."
    pause
    exit
}

# 3. CREATE SCAFFOLDING
# ------------------------------------------------------------------------------
Write-Host "Creating folder structure..." -ForegroundColor Green

# Create directories
$Folders = @(
    "data\raw",
    "data\processed",
    "R",
    "analysis",
    "output\plots",
    "output\tables"
)

foreach ($Folder in $Folders) {
    $Path = Join-Path -Path $ProjectRoot -ChildPath $Folder
    New-Item -ItemType Directory -Path $Path -Force | Out-Null
}

# 4. COPY ASSETS
# ------------------------------------------------------------------------------
$ThemeDest = Join-Path -Path $ProjectRoot -ChildPath "R\theme.R"

if (Test-Path $ThemeSource) {
    Copy-Item -Path $ThemeSource -Destination $ThemeDest
    Write-Host "  [+] Theme copied successfully." -ForegroundColor Gray
} else {
    Write-Warning "  [!] Could not find theme.R at $ThemeSource. Check the path in the script."
}

# 5. GENERATE FILES
# ------------------------------------------------------------------------------

# A. Create .Rproj file
$RprojContent = @"
Version: 1.0
RestoreWorkspace: Default
SaveWorkspace: Default
AlwaysSaveHistory: Default
EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 2
Encoding: UTF-8
RnwWeave: Sweave
LaTeX: pdfLaTeX
"@
Set-Content -Path "$ProjectRoot\$ProjectName.Rproj" -Value $RprojContent

# B. Create _quarto.yml
$QuartoYml = @"
project:
  type: website
  output-dir: _site

website:
  title: "$ProjectName"
  navbar:
    left:
      - href: analysis/index.qmd
        text: Home

format:
  html:
    theme: cosmo
    css: styles.css
    toc: true
"@
Set-Content -Path "$ProjectRoot\_quarto.yml" -Value $QuartoYml

# C. Create .gitignore
$GitIgnore = @"
.Rproj.user
.Rhistory
.RData
.Ruserdata
_site/
.quarto/
data/raw/
"@
Set-Content -Path "$ProjectRoot\.gitignore" -Value $GitIgnore

# D. Create the Analysis File (Fixed Indentation)
# Note: The closing "@ must be completely unindented.
$IndexQmd = @"
---
title: "Analysis: $ProjectName"
format: html
---

```{r setup}
#| warning: false
#| message: false
library(tidyverse)
library(here)
library(scales) 
library(ggrepel)

# Load the Project Theme
source(here("R", "theme.R"))