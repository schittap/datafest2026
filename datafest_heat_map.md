DataFest
================
Siddharth Chittapuram
2026-05-02

``` r
library(usmap)
library(ggplot2)
library(dplyr)

tigercensuscodes <- read.csv("tigercensuscodes.csv")
tigercensuscodes
social_determinants <- read.csv("social_determinants.csv")
providers <- read.csv("providers.csv")
patients <- read.csv("patients.csv")
encounters <- read.csv("encounters.csv")
diagnosis <- read.csv("diagnosis.csv")
departments <- read.csv("departments.csv")
cdk_df_with_values <- 
read.csv("cdk_df_with_values.csv")
KS <- read.csv("Kansas.csv")

cdk_df_with_values |>
  group_by(FIPS)

encounters[which(!is.na(encounters$AdmissionInstant)), ]

departments
unique(departments$City)
unique(departments$County)

valid_indices <- which(!(departments$County) %in% c("*Deleted", "*Not Applicable", 
"*Unspecified", "*Unknown") | is.na(departments$County))

departments[valid_indices, ]

rownames(encounters)
colnames(encounters)

# # These do not appear to be the same values
# unique(tigercensuscodes$GEOID) 
# unique(departments$CensusTract)


cdk <- cdk_df_with_values |>
  group_by(FIPS)
```

# Heat Map

Think about why rural counties vary heavily between low and high SVI
scores. Notice Wyandotte County in the east, which is part of Kansas
City, has a high SVI but Sedgwick County, which includes Wichita, is not
as extreme. Incorporate the four RPL themes in our explanation.

<http://worldpopulationreview.com/us-cities/kansas>

``` r
# KS <- kansas |>
#   group_by(FIPS)
# 
# rpl_themes <- KS$RPL_THEMES
# 
# counties <- KS |>
#   distinct(COUNTY)
# counties
# 
# df_ks <- data.frame(
#   county = counties$COUNTY, 
#   values = rpl_themes)
# df_ks

# df_ks$fips <- fips(state = "KS", county = df_ks$county)

# Select KS Cities by Population: Wichita, Overland Park, Kansas City, Topeka (one per county max)
cities <- data.frame(city = c("Wichita", "Overland Park", "Kansas City", "Topeka"),
lon = c(-97.3375448, -94.6851702, -94.626497, -95.677556), 
lat = c(37.6922361, 38.9742502, 39.1134562, 39.049011))

updated_cities <- usmap_transform(cities)

df_ks <- KS |> # some RPL values are negative, so filter those out
  filter(RPL_THEMES >= 0) |>
  mutate(fips = substr(as.character(FIPS), 1, 5)) |>
  group_by(fips) |>
  summarize( # If we suspect skew, take the median
    values = median(RPL_THEMES, na.rm = TRUE),
    county_name = first(COUNTY)
  )

# Exclusive to Kansas
# 1. Update the positioning logic
kc_label <- updated_cities %>% filter(city == "Kansas City")
op_label <- updated_cities %>% filter(city == "Overland Park")
topeka_label <- updated_cities %>% filter(city == "Topeka")
wichita_label <- updated_cities %>% filter(city == "Wichita")

# 2. The Plot
p <- plot_usmap(data = df_ks, values = "values", include = "KS", regions = "counties", color = "#D6D2C8") +
  # Updated Scale: Blue for Low SVI, Red for High SVI
  scale_fill_gradient(low = "#4C72B0", high = "#C44E52", name = "SVI (RPL)", na.value = "grey90") +
  
  geom_sf(data = updated_cities, shape = 21, fill = "white", color = "black", stroke = 0.5, size = 2, inherit.aes = FALSE) + 
  
  # Topeka
  geom_sf_label(data = topeka_label, aes(label = city), 
                nudge_x = -57000, nudge_y = 10000,
                fill = alpha("white", 0.8), family = "sans", fontface = "bold", size = 3.5, inherit.aes = FALSE) +
  
  # Kansas City
  geom_sf_label(data = kc_label, aes(label = city), 
                nudge_y = 25000, nudge_x = 15000, 
                fill = alpha("white", 0.8), family = "sans", fontface = "bold", size = 3.5, inherit.aes = FALSE) +
  
  # Overland Park
  geom_sf_label(data = op_label, aes(label = city), 
                nudge_y = -25000, nudge_x = -15000,
                fill = alpha("white", 0.8), family = "sans", fontface = "bold", size = 3.5, inherit.aes = FALSE) +
                
  # Wichita
  geom_sf_label(data = wichita_label, aes(label = city), 
                nudge_y = 25000, 
                fill = alpha("white", 0.8), family = "sans", fontface = "bold", size = 3.5, inherit.aes = FALSE) +

  coord_sf(clip = "off") + 
  labs(
    title = "Median Social Vulnerability Index (SVI) in Kansas by County",
  ) +
  theme_void() +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(t = 60, r = 80, b = 20, l = 50),
    
    # Beige Backgrounds
    plot.background  = element_rect(fill = "#F0EDE6", color = NA),
    panel.background = element_rect(fill = "#F0EDE6", color = NA),
    legend.background = element_rect(fill = "#F0EDE6", color = NA),
    
    # Font changed to "sans" to prevent the error
    text = element_text(family = "sans"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 10)),
    
    legend.position = "right",
    legend.justification = "center",
    legend.margin = margin(t = 25),
    legend.box.margin = margin(l = 30),   
    legend.title = element_text(size = 12.5, face = "bold"),
    legend.text = element_text(size = 12.5)
  )

# Print the plot
print(p)

# Save with the matching beige background
ggsave("Kansas_SVI_Heatmap.png", plot = p, width = 10, height = 6, dpi = 300, bg = "#F0EDE6")
```
