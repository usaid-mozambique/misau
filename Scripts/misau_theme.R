# MISAU Color Palette
misau_colors <- c(
  "primary_red" = "#C8102E",
  "health_green" = "#007A33",
  "sun_yellow" = "#FFD100",
  "sky_blue" = "#00ADEF",
  "midnight_black" = "#1C2826",
  "paper_white" = "#F2F2F2",
  "corn_gold" = "#FFB81C",
  "burnt_brown" = "#8B5E3C"
)

# Function to extract colors by name
get_misau_color <- function(name) {
  misau_colors[[name]]
}

# Custom ggplot2 Theme for MISAU
theme_misau <- function(base_size = 12, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(fill = get_misau_color("paper_white"), color = NA),
      panel.background = element_rect(fill = get_misau_color("paper_white"), color = NA),
      panel.grid.major = element_line(color = "#DDDDDD"),
      panel.grid.minor = element_blank(),
      axis.title = element_text(color = get_misau_color("midnight_black")),
      axis.text = element_text(color = get_misau_color("midnight_black")),
      plot.title = element_text(face = "bold", size = rel(1.2), color = get_misau_color("health_green")),
      plot.subtitle = element_text(size = rel(1.1), color = get_misau_color("burnt_brown")),
      plot.caption = element_text(size = rel(0.9), color = get_misau_color("midnight_black"))
    )
}
