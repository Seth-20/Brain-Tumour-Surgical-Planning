# ============================================================
# VISUALISATION OF BRAIN TUMOUR FEATURES (Task01_BrainTumour)
# Case: BRATS_001
#
# Input : the CSV produced from feature-extraction using Python
# Output: individual PNG figures plus one combined dashboard
#
# Figures produced:
#   1. Tumour region volumes (ET, TC, WT)
#   2. Tumour composition (enhancing / necrotic core / oedema)
#   3. Tumour extent (bounding-box dimensions)
#   4. MRI intensity profile per modality (median, mean, P10-P90)
#   5. Intensity heterogeneity per modality (coefficient of variation)
#   6. Combined dashboard of all of the above
# ============================================================

# ------------------------------------------------------------
# 1. PACKAGES
# ------------------------------------------------------------

# Install any package that is missing, then load them all.
required_packages <- c(
  "readr",      # read the CSV file
  "dplyr",      # data manipulation
  "tidyr",      # reshape wide data into long data
  "ggplot2",    # plotting
  "forcats",    # control the order of categories
  "scales",     # nicer axis labels
  "patchwork"   # combine several plots into one figure
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_packages, library, character.only = TRUE))

# Exploring BRATS Feature Extracted Data-----------------------------------
#BRATS DATA SETS LOADED
BRATS_001 <- read_csv("./outputs/BRATS_001_features.csv")
BRATS_ALL <- read_csv("./outputs/brain_tumour_features_all.csv")

colnames(BRATS_ALL)
view(BRATS_ALL)

Essential_Features_All <- BRATS_ALL %>% select(Case_ID, WT_volume_cm3, Tumour_core_fraction,TC_volume_cm3,WT_volume_cm3,
                                           ET_volume_cm3, Oedema_fraction, Bounding_box_X_voxels,
                                           Bounding_box_Y_voxels, Bounding_box_Z_voxels, Number_of_components)
view(Essential_Features)

Essential_Features_BRATS_001 <- BRATS_001 %>% select(Case_ID, WT_volume_cm3, Tumour_core_fraction,TC_volume_cm3,WT_volume_cm3,
                                           ET_volume_cm3, Oedema_fraction, Bounding_box_X_voxels,
                                           Bounding_box_Y_voxels, Bounding_box_Z_voxels, Number_of_components)

#Compute Volume of Non-Enhancing Core
Essential_Features_BRATS_001 <- Essential_Features_BRATS_001 %>% mutate(Non_Enhancing_Core = TC_volume_cm3 - ET_volume_cm3)

#Compute volume of Oedema/tumour Infiltrated tissue:
Essential_Features_BRATS_001 <- Essential_Features_BRATS_001 %>% mutate(Infiltrated_Tissue = (47/100)*WT_volume_cm3)



# ------------------------------------------------------------
# 2. CONFIGURATION
# ------------------------------------------------------------

# You can also pass the CSV path when running from a terminal:
#   Rscript Visualise_BraTS_Features.R path/to/BRATS_001_features.csv
args <- commandArgs(trailingOnly = TRUE)

CSV_PATH   <- if (length(args) > 0) args[1] else "./outputs/brain_tumour_features_all.csv"
OUTPUT_DIR <- "./R_plots"

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Colours chosen for segmentation overlay
COL_ET     <- "#D62728"   # enhancing tumour  (red)
COL_TC     <- "#17BECF"   # tumour core       (cyan)
COL_WT     <- "#E6B800"   # whole tumour      (yellow)
COL_NECRO  <- "#2F6F8F"   # necrotic / non-enhancing core (dark blue)
COL_OEDEMA <- "#E6B800"   # oedema / infiltrated tissue   (yellow)


# ------------------------------------------------------------
# 3. LOAD THE FEATURE TABLE
# ------------------------------------------------------------

if (!file.exists(CSV_PATH)) {
  stop("Cannot find the CSV file: ", CSV_PATH,
       "\nCheck CSV_PATH at the top of the script.")
}

features <- read_csv(CSV_PATH, show_col_types = FALSE)

# This script is written for ONE case (one row). If the CSV holds
# several cases, use the first row.
if (nrow(features) > 1) {
  message("CSV contains ", nrow(features),
          " cases. Using the first one: ", features$Case_ID[1])
  features <- features[1, ]
}

case_id <- features$Case_ID[1]

# Reusable theme for all figures
theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title      = element_text(face = "bold"),
      plot.subtitle   = element_text(colour = "grey35"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
)


# ------------------------------------------------------------
# 4. FIGURE 1: TUMOUR REGION VOLUMES
# ------------------------------------------------------------

# ET = enhancing tumour, TC = tumour core, WT = whole tumour
volume_df <- tibble(
  Region = factor(
    c("Enhancing tumour (ET)", "Tumour core (TC)", "Whole tumour (WT)"),
    levels = c("Enhancing tumour (ET)", "Tumour core (TC)", "Whole tumour (WT)")
  ),
  Volume_cm3 = c(
    features$ET_volume_cm3,
    features$TC_volume_cm3,
    features$WT_volume_cm3
  )
)

p_volume <- ggplot(volume_df, aes(x = Region, y = Volume_cm3, fill = Region)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(
    aes(label = sprintf("%.1f cm\u00B3", Volume_cm3)),
    vjust = -0.5, fontface = "bold"
  ) +
  scale_fill_manual(values = c(COL_ET, COL_TC, COL_WT)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Tumour region volumes",
    subtitle = case_id,
    x = NULL, y = "Volume (cm\u00B3)"
  )


# ------------------------------------------------------------
# 5. FIGURE 2: TUMOUR COMPOSITION
# ------------------------------------------------------------

# The CSV stores fractions of the whole tumour:
#   Enhancing_fraction   = ET / WT
#   Tumour_core_fraction = TC / WT   (TC contains ET)
#   Oedema_fraction      = oedema / WT
# So the necrotic / non-enhancing part of the core is TC - ET.
composition_df <- tibble(
  Component = factor(
    c("Enhancing tumour",
      "Necrotic / non-enhancing core",
      "Oedema / infiltrated tissue"),
    levels = c("Oedema / infiltrated tissue",
               "Necrotic / non-enhancing core",
               "Enhancing tumour")          # stacking order, top to bottom
  ),
  Fraction = c(
    features$Enhancing_fraction,
    features$Tumour_core_fraction - features$Enhancing_fraction,
    features$Oedema_fraction
  )
) %>%
  mutate(Volume_cm3 = Fraction * features$WT_volume_cm3)

p_composition <- ggplot(
  composition_df,
  aes(x = "Whole tumour", y = Fraction, fill = Component)
) +
  geom_col(width = 0.55, colour = "white") +
  geom_text(
    aes(label = sprintf("%s\n%.1f cm\u00B3", percent(Fraction, accuracy = 0.1),
                        Volume_cm3)),
    position = position_stack(vjust = 0.5),
    colour = "white", fontface = "bold", size = 3.6
  ) +
  scale_fill_manual(
    values = c("Enhancing tumour"              = COL_ET,
               "Necrotic / non-enhancing core" = COL_NECRO,
               "Oedema / infiltrated tissue"   = "#B58E00"),
    breaks = c("Enhancing tumour",
               "Necrotic / non-enhancing core",
               "Oedema / infiltrated tissue")
  ) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.02))) +
  guides(fill = guide_legend(nrow = 3)) +
  labs(
    title    = "Tumour composition",
    subtitle = "Share of the whole-tumour volume",
    x = NULL, y = "Fraction of whole tumour", fill = NULL
  )


# ------------------------------------------------------------
# 6. FIGURE 3: TUMOUR EXTENT (BOUNDING BOX)
# ------------------------------------------------------------

# Dimensions are in voxels. Task01 voxels are 1 mm, so voxels = mm.
# X, Y, Z are the image array axes, not guaranteed anatomical axes.
voxel_mm <- features$Voxel_volume_mm3 ^ (1 / 3)   # edge length of a voxel

bbox_df <- tibble(
  Axis = c("X", "Y", "Z"),
  Extent_mm = c(
    features$Bounding_box_X_voxels,
    features$Bounding_box_Y_voxels,
    features$Bounding_box_Z_voxels
  ) * voxel_mm
)

p_bbox <- ggplot(bbox_df, aes(x = Axis, y = Extent_mm)) +
  geom_col(fill = COL_WT, width = 0.55) +
  geom_text(aes(label = sprintf("%.0f mm", Extent_mm)),
            vjust = -0.5, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Tumour extent",
    subtitle = "Whole-tumour bounding box (image axes)",
    x = "Image axis", y = "Extent (mm)"
  )


# ------------------------------------------------------------
# 7. RESHAPE THE INTENSITY FEATURES
# ------------------------------------------------------------

# The CSV has one column per modality and statistic (e.g. FLAIR_mean).
# Convert it to one row per modality with a column per statistic.
intensity_df <- features %>%
  select(matches("^(T1ce|T1|T2|FLAIR)_(mean|median|std|p10|p90)$")) %>%
  pivot_longer(
    cols         = everything(),
    names_to     = c("Modality", "Statistic"),
    names_pattern = "^(T1ce|T1|T2|FLAIR)_(.*)$",
    values_to    = "Value"
  ) %>%
  pivot_wider(names_from = Statistic, values_from = Value) %>%
  mutate(
    Modality = factor(Modality, levels = c("T1", "T1ce", "T2", "FLAIR")),
    CV       = std / mean      # coefficient of variation = heterogeneity
  )


# ------------------------------------------------------------
# 8. FIGURE 4: INTENSITY PROFILE PER MODALITY
# ------------------------------------------------------------

# Vertical line = P10 to P90 range, dot = median, diamond = mean.
# Raw intensities are in arbitrary scanner units: compare within a
# modality, and normalise before comparing patients.
p_intensity <- ggplot(intensity_df, aes(x = Modality)) +
  geom_linerange(aes(ymin = p10, ymax = p90),
                 linewidth = 2.5, colour = "grey70") +
  geom_point(aes(y = median, shape = "Median"),
             size = 3.5, colour = "grey15") +
  geom_point(aes(y = mean, shape = "Mean"),
             size = 3.5, colour = COL_ET) +
  scale_shape_manual(values = c("Median" = 16, "Mean" = 18)) +
  labs(
    title    = "MRI intensity inside the whole tumour",
    subtitle = "Grey bar = 10th to 90th percentile",
    x = NULL, y = "Intensity (arbitrary units)", shape = NULL
  )


# ------------------------------------------------------------
# 9. FIGURE 5: INTENSITY HETEROGENEITY
# ------------------------------------------------------------

# Higher coefficient of variation = more heterogeneous signal inside
# the tumour (e.g. necrosis plus enhancing rim on T1ce).
p_cv <- ggplot(
  intensity_df,
  aes(x = fct_reorder(Modality, CV, .desc = TRUE), y = CV)
) +
  geom_col(fill = COL_TC, width = 0.6) +
  geom_text(aes(label = sprintf("%.2f", CV)),
            vjust = -0.5, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Intensity heterogeneity",
    subtitle = "Coefficient of variation (SD / mean)",
    x = NULL, y = "Coefficient of variation"
  )


# ------------------------------------------------------------
# 10. COMBINED DASHBOARD
# ------------------------------------------------------------

dashboard <- (p_volume | p_composition | p_bbox) /
  (p_intensity | p_cv) +
  plot_annotation(
    title    = paste("Brain tumour feature summary:", case_id),
    subtitle = sprintf(
      "Whole tumour %.1f cm\u00B3 | Enhancing %.1f cm\u00B3 | %d connected component(s), largest = %s voxels",
      features$WT_volume_cm3, features$ET_volume_cm3,
      as.integer(features$Number_of_components),
      format(features$Largest_component_voxels, big.mark = ",")
    ),
    theme = theme(
      plot.title    = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11, colour = "grey35")
    )
  )

print(dashboard)


# ------------------------------------------------------------
# 11. SAVE THE FIGURES
# ------------------------------------------------------------

save_plot <- function(plot, name, width, height) {
  ggsave(
    filename = file.path(OUTPUT_DIR, paste0(case_id, "_", name, ".png")),
    plot = plot, width = width, height = height, dpi = 300, bg = "white"
  )
}

save_plot(p_volume,      "volumes",       6, 5)
save_plot(p_composition, "composition",   5, 6)
save_plot(p_bbox,        "extent",        5, 5)
save_plot(p_intensity,   "intensity",     6, 5)
save_plot(p_cv,          "heterogeneity", 6, 5)
save_plot(dashboard,     "dashboard",     16, 10)

message("Figures saved in: ", normalizePath(OUTPUT_DIR))