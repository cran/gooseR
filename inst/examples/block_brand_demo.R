#' Block Brand Demo - Light and Dark Themes
#' 
#' This script demonstrates the official Block brand themes
#' with minimal black and white aesthetic

# Load required libraries
library(gooseR)
library(ggplot2)
library(dplyr)
library(tidyr)

# Set up demo directory
demo_dir <- file.path(getwd(), "demo_output", "block_brand")
if (!dir.exists(demo_dir)) {
  dir.create(demo_dir, recursive = TRUE)
}

message("=== Block Brand Demo - Official Minimal Design ===\n")

# -----------------------------------------------------------------------------
# 1. Load Block brand
# -----------------------------------------------------------------------------
message("1. Loading Block brand configuration...")
block_config <- load_brand("block")
message(sprintf("   Brand: %s v%s", 
                block_config$brand$name, 
                block_config$brand$version))
message("   Themes: Light and Dark\n")

# -----------------------------------------------------------------------------
# 2. Create sample data
# -----------------------------------------------------------------------------
message("2. Preparing sample data...")

# Revenue data (minimal, clean)
revenue_data <- data.frame(
  quarter = factor(c("Q1", "Q2", "Q3", "Q4"), levels = c("Q1", "Q2", "Q3", "Q4")),
  revenue = c(245, 268, 295, 320),
  growth = c(12, 15, 18, 22)
)

# Time series data
time_data <- data.frame(
  month = 1:12,
  metric = c(100, 105, 108, 115, 122, 128, 135, 142, 148, 155, 162, 170)
)

# Category comparison
category_data <- data.frame(
  category = c("Square", "Cash App", "Bitcoin", "Tidal"),
  value = c(45, 35, 15, 5)
)

message("   ✓ Data prepared\n")

# -----------------------------------------------------------------------------
# 3. Light Theme Visualizations
# -----------------------------------------------------------------------------
message("3. Creating Block Light Theme visualizations...")

# Get monochrome palette
mono_colors <- brand_palette("block", "monochrome")

# Plot 1: Bar chart (Light)
p1_light <- ggplot(revenue_data, aes(x = quarter, y = revenue)) +
  geom_col(fill = mono_colors[1], width = 0.6) +
  geom_text(aes(label = paste0("$", revenue, "M")), 
            vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     breaks = seq(0, 350, 50)) +
  labs(
    title = "Q4 2025 Revenue Performance",
    subtitle = "Quarterly revenue in millions",
    x = NULL,
    y = "Revenue ($M)"
  ) +
  theme_brand("block", variant = "light") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave(file.path(demo_dir, "block_bar_light.png"), p1_light, 
       width = 8, height = 5, dpi = 300, bg = "white")
message("   ✓ Light bar chart saved")

# Plot 2: Line chart (Light)
p2_light <- ggplot(time_data, aes(x = month, y = metric)) +
  geom_line(size = 1.5, color = mono_colors[1]) +
  geom_point(size = 2.5, color = mono_colors[1]) +
  scale_x_continuous(breaks = 1:12, 
                     labels = month.abb) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  labs(
    title = "Monthly Performance Trend",
    subtitle = "Key metric tracking over 2025",
    x = NULL,
    y = "Performance Index"
  ) +
  theme_brand("block", variant = "light")

ggsave(file.path(demo_dir, "block_line_light.png"), p2_light, 
       width = 8, height = 5, dpi = 300, bg = "white")
message("   ✓ Light line chart saved")

# Plot 3: Horizontal bar chart (Light)
p3_light <- ggplot(category_data, aes(x = reorder(category, value), y = value)) +
  geom_col(fill = mono_colors[1], width = 0.5) +
  geom_text(aes(label = paste0(value, "%")), 
            hjust = -0.2, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Revenue Distribution by Product",
    subtitle = "Percentage of total revenue",
    x = NULL,
    y = "Percentage (%)"
  ) +
  theme_brand("block", variant = "light") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

ggsave(file.path(demo_dir, "block_horizontal_light.png"), p3_light, 
       width = 8, height = 5, dpi = 300, bg = "white")
message("   ✓ Light horizontal bar chart saved\n")

# -----------------------------------------------------------------------------
# 4. Dark Theme Visualizations
# -----------------------------------------------------------------------------
message("4. Creating Block Dark Theme visualizations...")

# Plot 1: Bar chart (Dark)
p1_dark <- ggplot(revenue_data, aes(x = quarter, y = revenue)) +
  geom_col(fill = "#FFFFFF", width = 0.6) +
  geom_text(aes(label = paste0("$", revenue, "M")), 
            vjust = -0.5, size = 3.5, fontface = "bold", color = "#FFFFFF") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     breaks = seq(0, 350, 50)) +
  labs(
    title = "Q4 2025 Revenue Performance",
    subtitle = "Quarterly revenue in millions",
    x = NULL,
    y = "Revenue ($M)"
  ) +
  theme_brand("block", variant = "dark") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave(file.path(demo_dir, "block_bar_dark.png"), p1_dark, 
       width = 8, height = 5, dpi = 300, bg = "black")
message("   ✓ Dark bar chart saved")

# Plot 2: Line chart (Dark)
p2_dark <- ggplot(time_data, aes(x = month, y = metric)) +
  geom_line(size = 1.5, color = "#FFFFFF") +
  geom_point(size = 2.5, color = "#FFFFFF") +
  scale_x_continuous(breaks = 1:12, 
                     labels = month.abb) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  labs(
    title = "Monthly Performance Trend",
    subtitle = "Key metric tracking over 2025",
    x = NULL,
    y = "Performance Index"
  ) +
  theme_brand("block", variant = "dark")

ggsave(file.path(demo_dir, "block_line_dark.png"), p2_dark, 
       width = 8, height = 5, dpi = 300, bg = "black")
message("   ✓ Dark line chart saved")

# Plot 3: Horizontal bar chart (Dark)
p3_dark <- ggplot(category_data, aes(x = reorder(category, value), y = value)) +
  geom_col(fill = "#FFFFFF", width = 0.5) +
  geom_text(aes(label = paste0(value, "%")), 
            hjust = -0.2, size = 3.5, fontface = "bold", color = "#FFFFFF") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Revenue Distribution by Product",
    subtitle = "Percentage of total revenue",
    x = NULL,
    y = "Percentage (%)"
  ) +
  theme_brand("block", variant = "dark") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

ggsave(file.path(demo_dir, "block_horizontal_dark.png"), p3_dark, 
       width = 8, height = 5, dpi = 300, bg = "black")
message("   ✓ Dark horizontal bar chart saved\n")

# -----------------------------------------------------------------------------
# 5. Grayscale variations (Light theme with multiple series)
# -----------------------------------------------------------------------------
message("5. Creating grayscale multi-series visualization...")

# Multi-series data
multi_data <- expand.grid(
  quarter = c("Q1", "Q2", "Q3", "Q4"),
  product = c("Square", "Cash App", "Bitcoin")
) %>%
  mutate(
    revenue = c(
      100, 110, 120, 135,  # Square
      80, 95, 110, 125,    # Cash App
      20, 25, 30, 35       # Bitcoin
    )
  )

p4_light <- ggplot(multi_data, aes(x = quarter, y = revenue, fill = product)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = mono_colors[c(1, 3, 5)]) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Product Revenue Comparison",
    subtitle = "Quarterly performance by product line",
    x = NULL,
    y = "Revenue ($M)",
    fill = "Product"
  ) +
  theme_brand("block", variant = "light") +
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank()
  )

ggsave(file.path(demo_dir, "block_multi_series_light.png"), p4_light, 
       width = 8, height = 5, dpi = 300, bg = "white")
message("   ✓ Multi-series chart saved\n")

# -----------------------------------------------------------------------------
# 6. Generate showcase HTML
# -----------------------------------------------------------------------------
message("6. Creating Block brand showcase HTML...")

showcase_html <- '<!DOCTYPE html>
<html>
<head>
    <title>Block Brand Showcase - Official Minimal Design</title>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body { 
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Helvetica, Arial, sans-serif;
            line-height: 1.5;
        }
        .container { max-width: 1200px; margin: 0 auto; padding: 40px 20px; }
        h1 { font-size: 32px; font-weight: 700; margin-bottom: 8px; }
        h2 { font-size: 24px; font-weight: 600; margin: 40px 0 20px; }
        h3 { font-size: 18px; font-weight: 600; margin: 20px 0 10px; }
        p { margin-bottom: 16px; color: #666; }
        .theme-section { margin: 40px 0; }
        .visualization { margin: 20px 0; }
        .visualization img { 
            width: 100%; 
            max-width: 800px; 
            border: 1px solid #e5e5e5;
            display: block;
        }
        .dark-section { 
            background: #000; 
            color: #fff; 
            padding: 40px 0;
            margin: 40px -20px;
        }
        .dark-section h2, .dark-section h3 { color: #fff; }
        .dark-section p { color: #ccc; }
        .dark-section .visualization img { border-color: #333; }
        .grid { 
            display: grid; 
            grid-template-columns: repeat(auto-fit, minmax(350px, 1fr));
            gap: 20px;
            margin: 20px 0;
        }
        .principle {
            padding: 20px;
            background: #f5f5f5;
            border-left: 4px solid #000;
        }
        .dark-section .principle {
            background: #1a1a1a;
            border-left-color: #fff;
        }
        code {
            background: #f5f5f5;
            padding: 2px 6px;
            font-family: "SF Mono", Monaco, monospace;
            font-size: 14px;
        }
        .dark-section code {
            background: #333;
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>Block Brand Design System</h1>
        <p>Official minimal black and white design language for Block Inc.</p>
        
        <div class="theme-section">
            <h2>Design Principles</h2>
            <div class="grid">
                <div class="principle">
                    <h3>Minimal</h3>
                    <p>Clean aesthetic with no unnecessary decoration</p>
                </div>
                <div class="principle">
                    <h3>Monochrome</h3>
                    <p>Black and white as primary colors</p>
                </div>
                <div class="principle">
                    <h3>Clear Hierarchy</h3>
                    <p>Size and weight create visual structure</p>
                </div>
                <div class="principle">
                    <h3>Generous Space</h3>
                    <p>White space enhances readability</p>
                </div>
            </div>
        </div>
        
        <div class="theme-section">
            <h2>Light Theme</h2>
            <p>White background with black elements - the primary Block aesthetic</p>
            
            <div class="visualization">
                <h3>Bar Chart</h3>
                <img src="block_bar_light.png" alt="Block Light Bar Chart">
            </div>
            
            <div class="visualization">
                <h3>Line Chart</h3>
                <img src="block_line_light.png" alt="Block Light Line Chart">
            </div>
            
            <div class="visualization">
                <h3>Horizontal Bar Chart</h3>
                <img src="block_horizontal_light.png" alt="Block Light Horizontal Bar">
            </div>
            
            <div class="visualization">
                <h3>Multi-Series Comparison</h3>
                <img src="block_multi_series_light.png" alt="Block Multi-Series Chart">
            </div>
        </div>
    </div>
    
    <div class="dark-section">
        <div class="container">
            <h2>Dark Theme</h2>
            <p>Black background with white elements - for dark contexts</p>
            
            <div class="visualization">
                <h3>Bar Chart</h3>
                <img src="block_bar_dark.png" alt="Block Dark Bar Chart">
            </div>
            
            <div class="visualization">
                <h3>Line Chart</h3>
                <img src="block_line_dark.png" alt="Block Dark Line Chart">
            </div>
            
            <div class="visualization">
                <h3>Horizontal Bar Chart</h3>
                <img src="block_horizontal_dark.png" alt="Block Dark Horizontal Bar">
            </div>
        </div>
    </div>
    
    <div class="container">
        <div class="theme-section">
            <h2>Usage in R</h2>
            <pre><code># Light theme (default)
library(gooseR)
library(ggplot2)

ggplot(data, aes(x, y)) +
  geom_col() +
  theme_brand("block", variant = "light")

# Dark theme
ggplot(data, aes(x, y)) +
  geom_col(fill = "white") +
  theme_brand("block", variant = "dark")

# Get Block colors
mono_palette <- brand_palette("block", "monochrome")</code></pre>
        </div>
        
        <div class="theme-section">
            <p style="text-align: center; color: #999; margin-top: 60px;">
                © 2025 Block Inc. | Built with gooseR
            </p>
        </div>
    </div>
</body>
</html>'

writeLines(showcase_html, file.path(demo_dir, "block_brand_showcase.html"))
message("   ✓ Brand showcase HTML saved\n")

# -----------------------------------------------------------------------------
# 7. Summary
# -----------------------------------------------------------------------------
message("=== Block Brand Demo Complete ===")
message(sprintf("All outputs saved to: %s", demo_dir))
message("\nGenerated files:")
message("  Light Theme:")
message("    • block_bar_light.png")
message("    • block_line_light.png")
message("    • block_horizontal_light.png")
message("    • block_multi_series_light.png")
message("  Dark Theme:")
message("    • block_bar_dark.png")
message("    • block_line_dark.png")
message("    • block_horizontal_dark.png")
message("  Documentation:")
message("    • block_brand_showcase.html")
message("\nNext steps:")
message("  1. Open block_brand_showcase.html in Chrome")
message("  2. Use theme_brand('block', variant='light') for light theme")
message("  3. Use theme_brand('block', variant='dark') for dark theme")
