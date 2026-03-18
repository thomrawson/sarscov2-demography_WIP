# Shape file taken from: https://geoportal.statistics.gov.uk/datasets/9d1fd338cdf945b08aabefb8a89e63fa_0/about

ONS_population_projections <- readRDS("ONS_population_projections.rds")
if(population_assumptions %in% c("ONS_NHS_region_principal", "ONS_NHS_region_low_migration", "ONS_NHS_region_high_migration")){
  ONS_population_projections <- filter(ONS_population_projections, scenario == population_assumptions)
} else(
  stop("population_assumptions not recognised")
)
## Sum England total:
age_cols <- grep("[0-9]", colnames(ONS_population_projections), value = TRUE)

england_row <- ONS_population_projections %>% 
  summarise(across(all_of(age_cols), sum, na.rm = TRUE)) %>%
  mutate(AREA = "england",
         CODE = NA,          # or whatever value you want
         scenario = population_assumptions) %>% 
  select(CODE, AREA, all_of(age_cols), scenario)

ONS_population_projections <- rbind(ONS_population_projections, england_row)

## Now create the sircovid age categories
regions <- unique(ONS_population_projections$AREA)
pop_list <- lapply(regions, sircovid:::sircovid_population)
pop_df <- do.call(rbind, pop_list) |> 
  as.data.frame()
colnames(pop_df) <- age_cols   # assign age columns
pop_df$AREA <- regions
pop_df$CODE <- NA            # or region codes 
pop_df$scenario <- "2019_baseline"        # or set something

df <- rbind(pop_df, ONS_population_projections)

## Convert to long format:
df <- df %>%
  pivot_longer(
    cols = matches("[0-9]"),      # selects all age-group columns
    names_to = "age_group",       # new column containing age group labels
    values_to = "population"      # new column containing population values
  )
###########################
#Set factor levels:
pop_long <- df %>%
  select(-CODE) %>%
  mutate(age_group = factor(age_group, levels = age_cols))
pop_long$population <- as.numeric(pop_long$population)
# Pivot wider to compute diffs easily
pop_wide <- pop_long %>%
  pivot_wider(names_from = scenario, values_from = population)

# rename columns to easier names
pop_wide <- pop_wide %>%
  rename(
    baseline = `2019_baseline`,
    ons_proj = population_assumptions
  ) %>%
  mutate(
    diff = ons_proj - baseline,
    pct_change = (ons_proj - baseline) / baseline * 100
  )
###############################################
# Plot ideas...
ggplot(pop_long, aes(x = age_group, y = population, group = scenario, color = scenario)) +
  geom_line(aes(linetype = scenario), size = 0.9) +
  geom_point(size = 1.7) +
  facet_wrap(~ AREA, ncol = 4, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #scale_y_continuous(labels = comma) +
  labs(title = "Population by age group — baseline vs ONS projection",
       x = "Age group", y = "Population",
       color = "Scenario", linetype = "Scenario")

area_to_plot <- "england"

#########################
saveRDS(pop_wide, "pop_wide.rds")
tight_margin <- margin(t = 6, r = 6, b = 6, l = 6)
##############
scenario_cols <- c(
  "2019_baseline" = "#2B83BA",       # softened blue
  "ONS_NHS_region_principal" = "#E69F00",  # orange
  "ONS_NHS_region_low_migration" = "#E69F00",  # orange
  "ONS_NHS_region_high_migration" = "#E69F00"  # orange
)
diff_cols <- c("TRUE" = "#2c7bb6", "FALSE" = "#d7191c")

# common theme
my_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(size = 13, margin = margin(b = 6)),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),     # remove vertical gridlines
    panel.grid.major.y = element_line(color = "gray90"),
    plot.margin = margin(t = 6, r = 8, b = 6, l = 8)
  )

# top plot (population)
p1_mod <- ggplot(filter(pop_long, AREA %in% "england"),
                 aes(x = age_group, y = population/1e6, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.86, colour = NA) +
  scale_fill_manual(values = scenario_cols,
                    labels = c("2019_baseline" = "2019 baseline", 
                               "ONS_NHS_region_principal" = "2047 central projection",
                               "ONS_NHS_region_principal" = "2047 low migration projection",
                               "ONS_NHS_region_principal" = "2047 high migration projection")) +
  scale_y_continuous(labels = label_number(accuracy = 1, suffix = " M"), expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0)) +
  labs(title = "A)    Population by age group - England",
       x = NULL, y = "Population (millions)", fill = "Population Scenario:") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, title.position = "left")) +
  my_theme +
  theme(
    axis.text.x = element_blank(),    # hide x labels on top plot
    axis.ticks.x = element_blank(),
    legend.position = "bottom"
  )

# bottom plot (percent change)
p2_mod <- ggplot(pop_wide %>% filter(AREA == "england"),
                 aes(x = age_group, y = pct_change, fill = pct_change > 0)) +
  geom_col() +
  scale_fill_manual(values = diff_cols,
                    labels = c("FALSE" = "Decrease or no change", "TRUE" = "Increase"),
                    guide = guide_legend(nrow = 1)) +
  geom_hline(yintercept = 0, color = "black", size = 0.4) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.02))) +
  scale_x_discrete(expand = c(0,0)) +
  labs(title = "B)   Population change from 2019 to 2047 - England",
       x = "Age group", y = "Difference in population (%)", fill = "Direction") +
  my_theme +
  theme(
    legend.position = "none",       # turned off here (we rely on top legend)
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot_grid(p1_mod, p2_mod, ncol = 1, rel_heights = c(3, 1.8), align = "v") -> p_england

ggsave(
  filename = "England_population.png",
  plot = p_england,
  width = 10,      # inches
  height = 8,      # inches
  dpi = 320        # high resolution
)

p1_mod <- p1_mod + theme(plot.margin = tight_margin)
p2_mod <- p2_mod + theme(plot.margin = tight_margin)
plot_grid(p1_mod, p2_mod, ncol = 1, rel_heights = c(3, 1.8), align = "v") -> p_england

dir.create("regional_plots")
for(r in regions){
  # top plot (population)
  p1_mod <- ggplot(filter(pop_long, AREA %in% r),
                   aes(x = age_group, y = population/1e6, fill = scenario)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.86, colour = NA) +
    scale_fill_manual(values = scenario_cols,
                      labels = c("2019_baseline" = "2019 baseline", 
                                 "ONS_NHS_region_principal" = "2047 projection",
                                 "ONS_NHS_region_principal" = "2047 low migration projection",
                                 "ONS_NHS_region_principal" = "2047 high migration projection")) +
    scale_y_continuous(labels = label_number(accuracy = 1, suffix = " M"), expand = c(0,0)) +
    scale_x_discrete(expand = c(0,0)) +
    labs(title = paste0("Population by age group — ", r),
         x = NULL, y = "Population (millions)", fill = "Population Scenario:") +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE, title.position = "left")) +
    my_theme +
    theme(
      axis.text.x = element_blank(),    # hide x labels on top plot
      axis.ticks.x = element_blank(),
      legend.position = "bottom"
    )
  
  # bottom plot (percent change)
  p2_mod <- ggplot(pop_wide %>% filter(AREA == r),
                   aes(x = age_group, y = pct_change, fill = pct_change > 0)) +
    geom_col() +
    scale_fill_manual(values = diff_cols,
                      labels = c("FALSE" = "Decrease or no change", "TRUE" = "Increase"),
                      guide = guide_legend(nrow = 1)) +
    geom_hline(yintercept = 0, color = "black", size = 0.4) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.02))) +
    scale_x_discrete(expand = c(0,0)) +
    labs(title = "Population change from 2019 baseline to 2047 projection",
         x = "Age group", y = "Population change (%)", fill = "Direction") +
    my_theme +
    theme(
      legend.position = "none",       # turned off here (we rely on top legend)
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  plot_grid(p1_mod, p2_mod, ncol = 1, rel_heights = c(3, 1.5), align = "v",
            labels = c("A", "B")) -> p
  
  ggsave(
    filename = paste0("regional_plots/",r, "_population.png"),
    plot = p,
    width = 10,      # inches
    height = 8,      # inches
    dpi = 320        # high resolution
  )
}

################################################################################

regions <- sircovid::regions("england")
pretty_region <- function(x) {
    gsub("_", " ", x) |>     # replace underscores with spaces
    tools::toTitleCase()     # Capitalise Each Word
}


# Build p2-style plot function for a single region
make_region_pct_plot <- function(region_name, show_x_labels = FALSE, show_y_labels = FALSE) {
  dat <- pop_wide %>% filter(AREA == region_name)
  
  # build x and y label values depending on show_x_labels
  x_lab <- if (show_x_labels) "Age group" else NULL
  y_lab <- if (show_y_labels) "Difference (%)" else NULL
  
  # theme elements: choose element or blank with standard if(...) else ...
  x_text_el <- if (show_x_labels) element_text(angle = 45, hjust = 1) else element_blank()
  x_ticks_el <- if (show_x_labels) element_line() else element_blank()
  
  y_text_el <- if (show_y_labels) element_text() else element_blank()
  
  ggplot(dat, aes(x = age_group, y = pct_change, fill = pct_change > 0)) +
    geom_col(width = 0.95, colour = NA) +
    scale_fill_manual(values = diff_cols,
                      labels = c("FALSE" = "Decrease or no change", "TRUE" = "Increase")) +
    geom_hline(yintercept = 0, color = "black", size = 0.4) +
    scale_y_continuous(breaks = c(0, 25, 50, 75, 100),
                       labels = function(x) paste0(x, "%"), 
                       limits = c(-20, 102),
                       expand = expansion(mult = c(0, 0.03))) +
    scale_x_discrete(expand = c(0,0)) +
    labs(title = pretty_region(region_name), x = x_lab, y = y_lab, fill = NULL) +
    my_theme +
    theme(
      legend.position = "none",
      axis.text.x = x_text_el,
      axis.text.y = y_text_el,
      axis.ticks.x = x_ticks_el,
      plot.title = element_text(size = 14, face = "bold")
    )
}

# Create individual region plots (7 plots)
region_plots <- lapply(seq_along(regions), function(i) {
  # Only show x axis labels for the bottom-left panel to avoid clutter
  show_x <- FALSE
  show_y <- FALSE
  # If using 4x2 (nrow=4, ncol=2) the bottom row panels are indices 7 and 8:
  # Show x labels for the 7th (left bottom) panel so readers can see age labels
  if(i %in% c(6,7)) show_x <- TRUE
  if(i %in% c(2,4,6)) show_y <- TRUE
  make_region_pct_plot(regions[i], show_x_labels = show_x, show_y_labels = show_y)
})

# Summary panel: total population percent change or absolute diff per region
totals_df <- pop_long %>%
  pivot_wider(names_from = scenario, values_from = population) %>%
  rename(baseline = `2019_baseline`, ons_proj = population_assumptions) %>%
  group_by(AREA) %>%
  summarise(total_baseline = sum(baseline, na.rm = TRUE),
            total_ons = sum(ons_proj, na.rm = TRUE)) %>%
  mutate(diff = total_ons - total_baseline,
         pct_change = 100 * diff / total_baseline) %>%
  filter(AREA %in% regions) %>%
  arrange(pct_change)

# Compute a consistent label anchor (a bit left of the smallest pct_change)
label_x <- min(totals_df$pct_change) - 7.5     # subtract 5% for padding

p_summary <- ggplot(totals_df,
                    aes(x = pct_change,
                        y = factor(AREA, levels = AREA),
                        fill = pct_change > 0)) +
  geom_col(width = 0.8, colour = NA) +
  scale_fill_manual(values = diff_cols, guide = FALSE) +
  geom_vline(xintercept = 0, color = "black", size = 0.4) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0,0.03))) +
  
  # --- PERFECTLY FLUSH LABELS ---
  geom_text(aes(x = label_x, label = pretty_region(AREA)),
            hjust = 0,                      # left-justified on same x pos
            vjust = 0.5,
            size = 4,
            color = "white",
            fontface = "bold") +
  
  labs(title = "Total population % change (2019 → 2047)",
       x = "Percent change", y = NULL) +
  coord_cartesian(clip = "off") +           # allow labels to extend left
  my_theme +
  theme(
    panel.grid.major.x = element_line(color = "gray85", size = 0.4),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = margin(t = 6, r = 10, b = 6, l = 60)   # room for labels
  )


                         
# Combine the 7 region plots + summary into a 4x2 grid.
# The order in the grid is the order of the plot list.
all_plots <- c(list(p_summary), region_plots)

# Arrange into 4 rows, 2 columns
grid_plot <- wrap_plots(all_plots, ncol = 2, nrow = 4) +
  plot_annotation(title = "Regional age group population changes",
                  theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)))

# Print to device
print(grid_plot)

# Save to file
 ggsave("regional_pct_changes_grid.png", grid_plot, width = 14, height = 12, dpi = 300)
 
 
 ################################
 ## Make a map of England NHS regions shaded by total population & change

 # ---- 1) Read spatial file ----
 nhs_sf <- st_read("NHS_region_shp_files/NHSER_DEC_2023_EN_BGC.shp", quiet = FALSE)
 

 # ---- 2) Correct region names in nhs_sf ----
 nhs_sf$AREA <- nhs_sf$NHSER23NM %>%
   tolower() |>                 # convert to lowercase
   gsub(" ", "_", x = _)        # replace spaces with underscores

 
 # ---- 3) Join the data frame to the spatial data ----
 nhs_joined <- nhs_sf %>%
   left_join(totals_df, by = "AREA")
 
 # ---- 4) Compute label points that are guaranteed inside each polygon ----
 # Use st_point_on_surface (safer than centroid for complex polygons)
 label_points <- nhs_joined %>%
   st_point_on_surface() %>%
   #st_centroid() %>%             # optional fallback - but st_point_on_surface is preferred
   st_transform(st_crs(nhs_joined))
 
 # Extract coordinates for plotting labels with geom_text (ggplot's geom_sf_text exists but we'll use coords)
 coords <- st_coordinates(label_points)
 label_df <- data.frame(coords, pct_change = nhs_joined$pct_change, AREA = nhs_joined$AREA)
 
 # A gentle manual nudge of London's
 label_df$X[which(label_df$AREA == "london")] <- label_df$X[which(label_df$AREA == "london")] + 8000
 
 # ---- 5) Simplify the geometry ----
 nhs_simple <- st_simplify(nhs_joined, dTolerance = 800) 
 
 # ---- 6) Plot with ggplot2 ----
 p_map <- 
   ggplot() +
   geom_sf(data = nhs_joined, aes(fill = pct_change/100), color = "grey30", size = 0.3) +
   # labels inside polygons
   # geom_text(data = label_df, aes(X, Y, label = ifelse(is.na(pct_change), "", sprintf("%.1f%%", pct_change))),
   #           size = 3.5, fontface = "bold", family = "sans") +
   geom_label(
     data = label_df,
     aes(X, Y, label = sprintf("%.1f%%", pct_change)),
     size = 3.5,
     fontface = "bold",
     label.size = 0.3,        # removes the border line (optional)
     label.r = unit(0.25, "lines"),  # rounded corners
     fill = "white",
     color = "#4a4a4a"      # soft FT-style charcoal text
   ) +
   #scale_fill_viridis(name = "% Population Change", option = "viridis", na.value = "lightgrey") +
   # scale_fill_gradient(
   #   #low = "#deebf7",      # very light blue
   #   #high = "#2c7bb6",     # main blue
   #   low = "#2c7bb6",
   #   high = "#fdae61",
   #   #low = "#e5e8ef",       # light, editorial
   #   #high = "#5b7fa3",      # muted blue
   #   name = "% Population Change"
   # ) +
   scale_fill_gradient2(
     low = "#5b7fa3",
     mid = "#f4efe6",
     high = "#d39c68",
     midpoint = 13/100,    # adjust to your data
     name = "% Increase",
     labels = scales::percent_format(accuracy = 0.1)
   ) +
   coord_sf(datum = NA) +                # drops axis ticks; keeps proper projection
   theme_minimal() +
   theme(
     legend.position = "right",
     panel.grid = element_blank(),
     axis.text = element_blank(),
     axis.title = element_blank(),
     plot.title = element_text(size = 16, face = "bold", margin = margin(b = 6))
   ) +
   ggtitle("C)   Total population change by NHS regions") +
   #annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering()) +
   #annotation_scale(location = "bl")
   theme(
     legend.position = c(0.25, 0.48),
     legend.background = element_rect(fill = alpha("white", 0.8), colour = NA),
     legend.title = element_text(size = 12),
     legend.text  = element_text(size = 10)
   )
 
 # ---- save the plot ----
 ggsave("NHS_pct_change_map.png", p_map, width = 8, height = 10, dpi = 300)
 
 p_map <- p_map + theme(plot.margin = tight_margin)
# ---- Stitch together the final plot ----
 
 final <- plot_grid(p_england, p_map, ncol = 2, rel_widths = c(1.5, 1), align = "v")
 final <- final + theme(plot.background = element_rect(fill = "white", colour = NA))
 #final_patch <- p_england | p_map + plot_layout(widths = c(5, 2))
 
 ggsave("Fig1.png", final, width = 16, height = 8, dpi = 300)
 
