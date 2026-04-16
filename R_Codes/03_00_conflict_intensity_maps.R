# Load Packages for Creating Maps
library(sf)
library(ggplot2)
library(haven)

# Load the Nepal Map
nepal_sf <- st_read(file.path(nepal_maps, "gadm41_NPL_3.shp"))

# Load Conflict Data
conflict_data <- read_dta(file.path(modified_data, "conflict_intensity.dta"))

# Checking the conflict data
head(conflict_data)
names(conflict_data)
nrow(conflict_data)

head(conflict_data$incident_district_num, 10)
head(nepal_sf$NAME_3, 10)

# Convert the labeled values in conflict data to actual text
conflict_data$district_name <- as_factor(conflict_data$incident_district_num)
head(conflict_data$district_name, 10)
print(conflict_data$district_name)

# Make names lowercase for comparison
conflict_districts <- tolower(as.character(conflict_data$district_name))
map_districts      <- tolower(nepal_sf$NAME_3)

head(conflict_districts, 10)
head(map_districts, 10)

setdiff(conflict_districts, map_districts)
setdiff(map_districts, conflict_districts)

# Fix mismatched district names
conflict_data$district_fixed <- as.character(conflict_data$district_name)   # ← fixed typo: was disrict_name

conflict_data$district_fixed[conflict_data$district_fixed == "panchathar"]     <- "panchthar"
conflict_data$district_fixed[conflict_data$district_fixed == "udaypur"]        <- "udayapur"
conflict_data$district_fixed[conflict_data$district_fixed == "dhanusha"]       <- "dhanusa"
conflict_data$district_fixed[conflict_data$district_fixed == "makawanpur"]     <- "makwanpur"
conflict_data$district_fixed[conflict_data$district_fixed == "kavre"]          <- "kavrepalanchok"
conflict_data$district_fixed[conflict_data$district_fixed == "sindhupalchowk"] <- "sindhupalchok"
conflict_data$district_fixed[conflict_data$district_fixed == "gorakha"]        <- "gorkha"
conflict_data$district_fixed[conflict_data$district_fixed == "kapilvastu"]     <- "kapilbastu"
conflict_data$district_fixed[conflict_data$district_fixed == "parvat"]         <- "parbat"

conflict_data$district_lower <- tolower(conflict_data$district_fixed)
nepal_sf$district_lower      <- tolower(nepal_sf$NAME_3)

setdiff(conflict_data$district_lower, nepal_sf$district_lower)

# Merge
nepal_conflict <- merge(nepal_sf, conflict_data, by = "district_lower", all.x = TRUE)
names(nepal_conflict)
nrow(nepal_conflict)

# ----- Helper: map theme (avoids repeating theme() every time) -----
map_theme <- theme_minimal() +
  theme(
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

# Map 1: Months of any conflict
ggplot(nepal_conflict) +
  geom_sf(aes(fill = mwar_own_any), color = "white", size = 0.1) +
  scale_fill_gradient(low = "#FFF5E6", high = "#8B0000", na.value = "grey90") +
  labs(title = "Nepal Civil Conflict Intensity: Months of War", fill = "Months of\nconflict") +
  map_theme
ggsave(file.path(figures, "month_of_war_intensity_1.png"), width = 10, height = 6, dpi = 300)

# Map 2: Months of fatal conflict
ggplot(nepal_conflict) +
  geom_sf(aes(fill = mwar_own_fatal), color = "white", size = 0.1) +
  scale_fill_gradient(low = "#FFF5E6", high = "#8B0000", na.value = "grey90") +
  labs(title = "Nepal Civil Conflict Intensity: Months of War with Fatal Casualty", fill = "Months of\nconflict") +
  map_theme
ggsave(file.path(figures, "month_of_war_intensity_2.png"), width = 10, height = 6, dpi = 300)

# Map 3: Total any casualties
ggplot(nepal_conflict) +
  geom_sf(aes(fill = cas_own_any), color = "white", size = 0.1) +
  scale_fill_gradient(low = "#FFF5E6", high = "#8B0000", na.value = "grey90") +
  labs(title = "Nepal Civil Conflict Intensity: Number of Any Casualty", fill = "Number of\ncasualties") +
  map_theme
ggsave(file.path(figures, "month_of_war_intensity_3.png"), width = 10, height = 6, dpi = 300)

# Map 4: Total fatal casualties
ggplot(nepal_conflict) +
  geom_sf(aes(fill = cas_own_fatal), color = "white", size = 0.1) +
  scale_fill_gradient(low = "#FFF5E6", high = "#8B0000", na.value = "grey90") +
  labs(title = "Nepal Civil Conflict Intensity: Number of Fatal Casualty", fill = "Number of\ncasualties") +
  map_theme
ggsave(file.path(figures, "month_of_war_intensity_4.png"), width = 10, height = 6, dpi = 300)

# Map 5: Months of war including neighboring districts (any)
ggplot(nepal_conflict) +
  geom_sf(aes(fill = mwar_nbr_any), color = "white", size = 0.1) +
  scale_fill_gradient(low = "#FFF5E6", high = "#8B0000", na.value = "grey90") +
  labs(title = "Nepal Civil Conflict Intensity: Months of War Including Neighboring Districts", fill = "Months of\nconflict") +
  map_theme
ggsave(file.path(figures, "month_of_war_intensity_5.png"), width = 10, height = 6, dpi = 300)

# Map 6: Casualties including neighboring districts (any)
ggplot(nepal_conflict) +
  geom_sf(aes(fill = cas_nbr_any), color = "white", size = 0.1) +
  scale_fill_gradient(low = "#FFF5E6", high = "#8B0000", na.value = "grey90") +
  labs(title = "Nepal Civil Conflict Intensity: Number of Casualties Including Neighboring Districts", fill = "Number of\ncasualties") +
  map_theme
ggsave(file.path(figures, "month_of_war_intensity_6.png"), width = 10, height = 6, dpi = 300)