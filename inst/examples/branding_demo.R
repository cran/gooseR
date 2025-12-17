#' GooseR Branding System Demo
#' 
#' This script demonstrates the universal branding system capabilities
#' of gooseR, using Block's brand as an example

# Load required libraries
library(gooseR)
library(ggplot2)
library(dplyr)
library(tidyr)

# Set up demo directory
demo_dir <- file.path(getwd(), "demo_output")
if (!dir.exists(demo_dir)) {
  dir.create(demo_dir, recursive = TRUE)
}

message("=== GooseR Universal Branding System Demo ===\n")

# -----------------------------------------------------------------------------
# 1. Load and validate Block brand
# -----------------------------------------------------------------------------
message("1. Loading Block brand configuration...")
block_config <- load_brand("block")
is_valid <- validate_brand(block_config)
message(sprintf("   Brand: %s v%s", 
                block_config$brand$name, 
                block_config$brand$version))
message(sprintf("   Valid: %s\n", is_valid))

# -----------------------------------------------------------------------------
# 2. Generate Block-branded ggplot2 visualizations
# -----------------------------------------------------------------------------
message("2. Creating Block-branded visualizations...")

# Get Block color palettes
block_colors <- brand_palette("block", "categorical", 5)
block_sequential <- brand_palette("block", "sequential_blue", 7)

# Create sample data
set.seed(42)
sales_data <- data.frame(
  quarter = rep(c("Q1", "Q2", "Q3", "Q4"), 3),
  year = rep(2023:2025, each = 4),
  revenue = c(
    100, 110, 125, 140,  # 2023
    145, 160, 175, 195,  # 2024
    200, 220, 245, 270   # 2025
  ) * 1000000,
  product = rep(c("Square", "Cash App", "Square", "Cash App"), 6),
  growth = c(
    5, 10, 13, 12,
    15, 18, 20, 22,
    25, 28, 30, 32
  )
)

# Plot 1: Bar chart with Block theme
p1 <- ggplot(sales_data %>% filter(year == 2025), 
             aes(x = quarter, y = revenue/1000000, fill = product)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = block_colors[1:2]) +
  scale_y_continuous(labels = scales::dollar_format(suffix = "M")) +
  labs(
    title = "Block Q4 2025 Revenue by Product",
    subtitle = "Square and Cash App Performance",
    x = "Quarter",
    y = "Revenue",
    fill = "Product",
    caption = "© 2025 Block Inc."
  ) +
  theme_brand("block")

ggsave(file.path(demo_dir, "block_bar_chart.png"), p1, 
       width = 8, height = 6, dpi = 300)
message("   ✓ Bar chart saved: block_bar_chart.png")

# Plot 2: Line chart with Block theme
p2 <- ggplot(sales_data, 
             aes(x = interaction(quarter, year), y = revenue/1000000, 
                 group = product, color = product)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = block_colors[1:2]) +
  scale_y_continuous(labels = scales::dollar_format(suffix = "M")) +
  labs(
    title = "Block Revenue Trend 2023-2025",
    subtitle = "Quarterly Performance by Product Line",
    x = "Quarter",
    y = "Revenue",
    color = "Product"
  ) +
  theme_brand("block") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(demo_dir, "block_line_chart.png"), p2, 
       width = 10, height = 6, dpi = 300)
message("   ✓ Line chart saved: block_line_chart.png")

# Plot 3: Scatter plot with Block theme
mtcars_data <- mtcars %>%
  mutate(efficiency = ifelse(mpg > 20, "High", "Low"))

p3 <- ggplot(mtcars_data, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 0.5) +
  scale_color_manual(values = block_colors[c(1, 3, 5)]) +
  labs(
    title = "Vehicle Performance Analysis",
    subtitle = "Weight vs Fuel Efficiency by Cylinder Count",
    x = "Weight (1000 lbs)",
    y = "Miles per Gallon",
    color = "Cylinders"
  ) +
  theme_brand("block")

ggsave(file.path(demo_dir, "block_scatter_plot.png"), p3, 
       width = 8, height = 6, dpi = 300)
message("   ✓ Scatter plot saved: block_scatter_plot.png")

# Plot 4: Heatmap with Block theme
correlation_data <- cor(mtcars[, c("mpg", "cyl", "disp", "hp", "wt", "qsec")])
cor_df <- as.data.frame(as.table(correlation_data))
names(cor_df) <- c("Var1", "Var2", "Correlation")

p4 <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile() +
  geom_text(aes(label = round(Correlation, 2)), color = "white", size = 3) +
  scale_fill_gradient2(low = block_colors[2], mid = "white", high = block_colors[1],
                       midpoint = 0, limits = c(-1, 1)) +
  labs(
    title = "Correlation Matrix",
    subtitle = "Vehicle Performance Metrics",
    x = "", y = ""
  ) +
  theme_brand("block") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(demo_dir, "block_heatmap.png"), p4, 
       width = 8, height = 7, dpi = 300)
message("   ✓ Heatmap saved: block_heatmap.png\n")

# -----------------------------------------------------------------------------
# 3. Generate Block CSS
# -----------------------------------------------------------------------------
message("3. Generating Block CSS...")
css_file <- file.path(demo_dir, "block_brand.css")
brand_css("block", output_file = css_file)
message(sprintf("   ✓ CSS saved: %s\n", basename(css_file)))

# -----------------------------------------------------------------------------
# 4. Create Block RMarkdown template
# -----------------------------------------------------------------------------
message("4. Creating Block RMarkdown template...")
rmd_file <- file.path(demo_dir, "block_report_template.Rmd")
brand_rmd_template("block", 
                  title = "Block Analytics Report", 
                  output_file = rmd_file)
message(sprintf("   ✓ RMarkdown template saved: %s\n", basename(rmd_file)))

# -----------------------------------------------------------------------------
# 5. Create a custom brand interactively (optional)
# -----------------------------------------------------------------------------
message("5. Custom brand creation (skipping in demo mode)")
message("   To create a custom brand, run: goose_create_brand('your_brand')\n")

# -----------------------------------------------------------------------------
# 6. Generate brand showcase HTML
# -----------------------------------------------------------------------------
message("6. Creating brand showcase HTML...")

showcase_html <- sprintf('
<!DOCTYPE html>
<html>
<head>
    <title>%s Brand Showcase</title>
    <link rel="stylesheet" href="block_brand.css">
    <style>
        body { padding: 40px; max-width: 1200px; margin: 0 auto; }
        .color-swatch { 
            display: inline-block; 
            width: 100px; 
            height: 100px; 
            margin: 10px;
            border: 1px solid #ddd;
            text-align: center;
            line-height: 100px;
            font-size: 10px;
            color: white;
            text-shadow: 0 0 3px rgba(0,0,0,0.5);
        }
        .visualization {
            margin: 30px 0;
            text-align: center;
        }
        .visualization img {
            max-width: 100%%;
            border: 1px solid var(--color-gray-30);
        }
        .section {
            margin: 40px 0;
        }
    </style>
</head>
<body>
    <h1>%s Brand Showcase</h1>
    <p>Generated by gooseR Universal Branding System</p>
    
    <div class="section">
        <h2>Brand Colors</h2>
        <div>
            <div class="color-swatch" style="background: %s;">%s</div>
            <div class="color-swatch" style="background: %s;">%s</div>
            <div class="color-swatch" style="background: %s;">%s</div>
            <div class="color-swatch" style="background: %s;">%s</div>
            <div class="color-swatch" style="background: %s;">%s</div>
        </div>
    </div>
    
    <div class="section">
        <h2>Typography</h2>
        <h1>Heading 1 - %s</h1>
        <h2>Heading 2 - %s</h2>
        <h3>Heading 3 - %s</h3>
        <p>Body text using the brand font family. This demonstrates the standard paragraph styling with proper line height and spacing.</p>
    </div>
    
    <div class="section">
        <h2>Data Visualizations</h2>
        <div class="visualization">
            <h3>Bar Chart</h3>
            <img src="block_bar_chart.png" alt="Bar Chart">
        </div>
        <div class="visualization">
            <h3>Line Chart</h3>
            <img src="block_line_chart.png" alt="Line Chart">
        </div>
        <div class="visualization">
            <h3>Scatter Plot</h3>
            <img src="block_scatter_plot.png" alt="Scatter Plot">
        </div>
        <div class="visualization">
            <h3>Heatmap</h3>
            <img src="block_heatmap.png" alt="Heatmap">
        </div>
    </div>
    
    <div class="section">
        <h2>Usage</h2>
        <pre><code># Apply Block theme to ggplot2
library(gooseR)
library(ggplot2)

ggplot(data, aes(x, y)) +
  geom_point() +
  theme_brand("block")

# Get Block colors
colors <- brand_palette("block", "categorical")</code></pre>
    </div>
    
    <footer>
        <p>%s</p>
    </footer>
</body>
</html>',
  block_config$brand$name,
  block_config$brand$name,
  block_colors[1], block_colors[1],
  block_colors[2], block_colors[2],
  block_colors[3], block_colors[3],
  block_colors[4], block_colors[4],
  block_colors[5], block_colors[5],
  block_config$typography$fonts$primary,
  block_config$typography$fonts$primary,
  block_config$typography$fonts$primary,
  block_config$metadata$copyright
)

writeLines(showcase_html, file.path(demo_dir, "block_brand_showcase.html"))
message("   ✓ Brand showcase HTML saved: block_brand_showcase.html\n")

# -----------------------------------------------------------------------------
# 7. Summary
# -----------------------------------------------------------------------------
message("=== Demo Complete ===")
message(sprintf("All outputs saved to: %s", demo_dir))
message("\nGenerated files:")
message("  • block_bar_chart.png")
message("  • block_line_chart.png") 
message("  • block_scatter_plot.png")
message("  • block_heatmap.png")
message("  • block_brand.css")
message("  • block_report_template.Rmd")
message("  • block_brand_showcase.html")
message("\nNext steps:")
message("  1. Open block_brand_showcase.html in a browser")
message("  2. Try the RMarkdown template: rmarkdown::render('demo_output/block_report_template.Rmd')")
message("  3. Create your own brand: goose_create_brand('my_company')")
