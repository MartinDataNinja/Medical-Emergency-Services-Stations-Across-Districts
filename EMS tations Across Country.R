# Load necessary libraries
library(sf)
library(ggplot2)
library(dplyr)

# Define data for the provided counts by district
district_data <- data.frame(
  District = c("Bugesera", "Burera", "Gakenke", "Gasabo", "Gatsibo", "Gicumbi", 
               "Gisagara", "Huye", "Kamonyi", "Karongi", "Kayonza", "Kicukiro", 
               "Kirehe", "Muhanga", "Musanze", "Ngoma", "Ngororero", "Nyabihu", 
               "Nyagatare", "Nyamagabe", "Nyamasheke", "Nyanza", "Nyarugenge", 
               "Nyaruguru", "Rubavu", "Ruhango", "Rulindo", "Rusizi", "Rutsiro", "Rwamagana"),
  Value = c(1, 0, 1, 6, 1, 1, 
            0, 1, 0, 1, 0, 4, 
            1, 1, 1, 0, 0, 0, 
            1, 1, 1, 0, 4, 0, 
            1, 0, 0, 0, 0, 1)
)

# Path to your shapefile
shapefile_path <- "C:/Users/GK TECH/Downloads/rwa_adm2_2006_NISR_WGS1984_20181002.shp"
shapefile_data <- st_read(shapefile_path)

# Transform CRS to match the desired projection
shapefile_data <- st_transform(shapefile_data, crs = 32736)

# Join district data with shapefile data
map_data <- shapefile_data %>%
  left_join(district_data, by = c("ADM2_EN" = "District"))

# Convert the "Value" column to a factor for categorization
map_data$Value <- factor(map_data$Value, levels = c(0, 1, 4, 6), labels = c("0", "1", "4", "6"))

# Create the plot
gg_map <- ggplot(map_data) +
  geom_sf(aes(fill = Value), color = "black", size = 0.3) +
  geom_sf_text(aes(label = ADM2_EN), size = 9, color = "black") +
  scale_fill_manual(
    values = c("0" = "grey", "1" = "#88CCEE", "4" = "lightblue", "6" = "darkblue"),
    name = "Ambulance Stations"
  ) +
  labs(
    caption = "Source: Emergency Medical Services(EMS), 2024"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.8,0.2),
    legend.direction = "horizontal",
    legend.title = element_text(size = 25, face = "bold"),
    legend.text = element_text(size = 24),
    plot.caption = element_text(size = 24, hjust = 0),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  guides(
    fill = guide_legend(
      title.position = "top", 
      title.hjust = 0.5, 
      keywidth = 1, 
      keyheight = 0.5
    )
  )

# Save the plot
ggsave("C:/Users/GK TECH/Desktop/PNG_Graphs/Stations_by_district.png", plot = gg_map, width = 10, height = 7, dpi = 300)

# Display the plot
print(gg_map)
