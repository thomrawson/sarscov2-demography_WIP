# Shape file taken from: https://geoportal.statistics.gov.uk/datasets/9d1fd338cdf945b08aabefb8a89e63fa_0/about

ONS_population_projections <- readRDS("ONS_population_projections.rds")
#Cut to just 2047
ONS_population_projections <- filter(ONS_population_projections, year == 2047,
                                     scenario == "ONS_NHS_region_principal"
                                     )

#Cut the year column
ONS_population_projections$year <- NULL

age_cols <- setdiff(names(ONS_population_projections), c("CODE", "AREA", "scenario"))

pop_by_scenario <- ONS_population_projections %>%
  group_by(scenario) %>%
  summarise(across(all_of(age_cols), sum, na.rm = TRUE), .groups = "drop")

counterfactual_pop <- as.double(pop_by_scenario[1,2:18])
baseline_pop <- sircovid:::sircovid_population("england")

sircovid:::clear_cache()
counterfactual_trns_matrix <- sircovid:::sircovid_transmission_matrix('england', population = counterfactual_pop)
sircovid:::clear_cache()
baseline_trns_matrix <- sircovid:::sircovid_transmission_matrix('england', population = baseline_pop)
sircovid:::clear_cache()
#These are transmission matrices, each element means the daily contact rate between one specific individual in group i and one specific individual in group j (scaled by size)

age_bins <- sircovid:::sircovid_age_bins()
max_polymod_age <- 70

baseline_survey_pop <- data_frame(
  "lower.age.limit" = sircovid:::sircovid_age_bins()$start,
  population = baseline_pop)

counterfactual_survey_pop <- data_frame(
  "lower.age.limit" = sircovid:::sircovid_age_bins()$start,
  population = counterfactual_pop)

## Get the contact matrix from socialmixr; polymod only
## goes up to age 70
baseline_contact <- suppressMessages(socialmixr::contact_matrix(
  survey.pop = baseline_survey_pop,
  socialmixr::polymod,
  countries = "United Kingdom",
  age.limits = age_bins$start[age_bins$start <= max_polymod_age],
  symmetric = TRUE))
baseline_contact <- baseline_contact$matrix

counterfactual_contact <- suppressMessages(socialmixr::contact_matrix(
  survey.pop = counterfactual_survey_pop,
  socialmixr::polymod,
  countries = "United Kingdom",
  age.limits = age_bins$start[age_bins$start <= max_polymod_age],
  symmetric = TRUE))
counterfactual_contact <- counterfactual_contact$matrix

extend_contact_matrix <- function(m, age_bins, max_polymod_age = 70) {
  extra <- age_bins$start[age_bins$start > max_polymod_age]
  n_polymod <- nrow(m)
  idx <- c(seq_len(n_polymod), rep(n_polymod, length(extra)))
  m_full <- m[idx, idx]
  
  nms <- sprintf("[%d,%d)", age_bins$start, age_bins$end)
  dimnames(m_full) <- list(nms, nms)
  m_full
}

baseline_contact_full <- extend_contact_matrix(baseline_contact, age_bins)
counterfactual_contact_full <- extend_contact_matrix(counterfactual_contact, age_bins)

# sanity check: rows/cols 15, 16, 17 (70-74, 75-79, 80+) should be identical within each matrix
identical(baseline_contact_full[15, ], baseline_contact_full[16, ])
identical(baseline_contact_full[16, ], baseline_contact_full[17, ])

###########################################################################
###########################################################################
## ---------------------------------------------------------------------------
## Plot baseline, counterfactual, and difference contact matrices,
## with row totals appended as an extra column

## Toggle: sqrt-transform the fill scale to counter diagonal dominance.
## Applies to panels A/B only (the Total column is plain text, unaffected).
use_sqrt_scale <- TRUE

## -----------------------------------------------------------------------
## Relabel age categories: "0-4", "5-9", ..., "75-79", "80+"
## -----------------------------------------------------------------------
age_bins <- sircovid:::sircovid_age_bins()
age_labels <- sprintf("%d-%d", age_bins$start, age_bins$end)
age_labels[length(age_labels)] <- sprintf("%d+", age_bins$start[length(age_bins$start)])

dimnames(baseline_contact_full) <- list(age_labels, age_labels)
dimnames(counterfactual_contact_full) <- list(age_labels, age_labels)

total_label <- "Total"
n_age <- length(age_labels)

## -----------------------------------------------------------------------
## Helper: matrix -> long data frame for ggplot, with row totals appended
## as an extra "Total" column.
## -----------------------------------------------------------------------
matrix_to_long_with_totals <- function(m, value_name = "value") {
  age_levels <- rownames(m)
  totals <- rowSums(m)
  
  df <- as.data.frame(m)
  df$participant_age <- rownames(m)
  df <- pivot_longer(df, cols = -participant_age,
                     names_to = "contact_age", values_to = value_name)
  df$is_total <- FALSE
  
  totals_df <- data.frame(
    participant_age = names(totals),
    contact_age = total_label,
    value = as.numeric(totals),
    is_total = TRUE
  )
  names(totals_df)[names(totals_df) == "value"] <- value_name
  
  combined <- rbind(df, totals_df)
  combined$participant_age <- factor(combined$participant_age, levels = rev(age_levels))
  combined$contact_age <- factor(combined$contact_age, levels = c(age_levels, total_label))
  combined
}

baseline_df <- matrix_to_long_with_totals(baseline_contact_full, "contacts")
counterfactual_df <- matrix_to_long_with_totals(counterfactual_contact_full, "contacts")

diff_matrix <- counterfactual_contact_full - baseline_contact_full
diff_df <- matrix_to_long_with_totals(diff_matrix, "diff")

## -----------------------------------------------------------------------
## Shared fill limits (computed on individual cells only, excluding Total)
## -----------------------------------------------------------------------
max_contacts <- max(baseline_df$contacts[!baseline_df$is_total],
                    counterfactual_df$contacts[!counterfactual_df$is_total],
                    na.rm = TRUE)

max_abs_diff <- max(abs(diff_df$diff[!diff_df$is_total]), na.rm = TRUE)

fill_trans <- if (use_sqrt_scale) "sqrt" else "identity"

## -----------------------------------------------------------------------
## Shared theme
## -----------------------------------------------------------------------
heatmap_theme <- theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(1.4, "cm"),
    plot.title = element_text(face = "bold", size = 12, hjust = 0)
  )

## -----------------------------------------------------------------------
## Panel A: baseline
## -----------------------------------------------------------------------
p_baseline <- ggplot() +
  geom_tile(data = subset(baseline_df, !is_total),
            aes(x = contact_age, y = participant_age, fill = contacts)) +
  scale_fill_gradient(low = "white", high = "#1b9e77",
                      limits = c(0, max_contacts), trans = fill_trans,
                      name = "Mean daily\ncontacts") +
  geom_tile(data = subset(baseline_df, is_total),
            aes(x = contact_age, y = participant_age),
            fill = "grey94", colour = "white") +
  geom_text(data = subset(baseline_df, is_total),
            aes(x = contact_age, y = participant_age, label = sprintf("%.1f", contacts)),
            size = 2.4) +
  geom_vline(xintercept = n_age + 0.5, linetype = "dashed", colour = "grey60") +
  labs(title = "A. Contact Matrix - 2020 Baseline", x = "Age of contact", y = "Age of participant") +
  coord_fixed() +
  heatmap_theme

## -----------------------------------------------------------------------
## Panel B: counterfactual
## -----------------------------------------------------------------------
p_counterfactual <- ggplot() +
  geom_tile(data = subset(counterfactual_df, !is_total),
            aes(x = contact_age, y = participant_age, fill = contacts)) +
  scale_fill_gradient(low = "white", high = "#d95f02",
                      limits = c(0, max_contacts), trans = fill_trans,
                      name = "Mean daily\ncontacts") +
  geom_tile(data = subset(counterfactual_df, is_total),
            aes(x = contact_age, y = participant_age),
            fill = "grey94", colour = "white") +
  geom_text(data = subset(counterfactual_df, is_total),
            aes(x = contact_age, y = participant_age, label = sprintf("%.1f", contacts)),
            size = 2.4) +
  geom_vline(xintercept = n_age + 0.5, linetype = "dashed", colour = "grey60") +
  labs(title = "B. Contact Matrix - 2047 Counterfactual", x = "Age of contact", y = "Age of participant") +
  coord_fixed() +
  heatmap_theme

## -----------------------------------------------------------------------
## Panel C: difference (counterfactual - baseline)
## Total column here is the total CHANGE in mean daily contacts for that
## age group (row sum of the difference matrix), signed.
## -----------------------------------------------------------------------
p_diff <- ggplot() +
  geom_tile(data = subset(diff_df, !is_total),
            aes(x = contact_age, y = participant_age, fill = diff)) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                       midpoint = 0, limits = c(-max_abs_diff, max_abs_diff),
                       name = "Difference in\nmean daily contacts") +
  geom_tile(data = subset(diff_df, is_total),
            aes(x = contact_age, y = participant_age),
            fill = "grey94", colour = "white") +
  geom_text(data = subset(diff_df, is_total),
            aes(x = contact_age, y = participant_age, label = sprintf("%+.1f", diff)),
            size = 2.4) +
  geom_vline(xintercept = n_age + 0.5, linetype = "dashed", colour = "grey60") +
  labs(title = "C. Contact Matrix Difference \n(Counterfactual - Baseline)",
       x = "Age of contact", y = "Age of participant") +
  coord_fixed() +
  heatmap_theme

## -----------------------------------------------------------------------
## Combine and save
## -----------------------------------------------------------------------
combined <- p_baseline + p_counterfactual + p_diff + plot_layout(nrow = 1)

ggsave("contact_matrices_baseline_counterfactual_diff.pdf", combined,
       width = 16.5, height = 5.5, units = "in", dpi = 300)
ggsave("contact_matrices_baseline_counterfactual_diff.png", combined,
       width = 16.5, height = 5.5, units = "in", dpi = 300)

############
# Also a 2x2 version with the transmission matrix too

# Build the transmission matrices via the package's own function, not a
## manual contact/population division -- guarantees this matches exactly
## what lancelot_parameters() feeds into the compiled model.
## -----------------------------------------------------------------------
region <- "england" 

sircovid:::clear_cache()
baseline_transmission_full <- sircovid:::sircovid_transmission_matrix(
  region = region, population = baseline_pop)

sircovid:::clear_cache()
counterfactual_transmission_full <- sircovid:::sircovid_transmission_matrix(
  region = region, population = counterfactual_pop)

## sanity check: these should NOT be identical -- if they are, the cache
## wasn't cleared correctly between calls
stopifnot(!identical(baseline_transmission_full, counterfactual_transmission_full))

dimnames(baseline_transmission_full) <- list(age_labels, age_labels)
dimnames(counterfactual_transmission_full) <- list(age_labels, age_labels)

diff_transmission_matrix <- counterfactual_transmission_full - baseline_transmission_full
diff_transmission_df <- matrix_to_long_with_totals(diff_transmission_matrix, "diff")

max_abs_trans_diff <- max(abs(diff_transmission_df$diff[!diff_transmission_df$is_total]), na.rm = TRUE)

p_trans_diff <- ggplot() +
  geom_tile(data = subset(diff_transmission_df, !is_total),
            aes(x = contact_age, y = participant_age, fill = diff)) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                       midpoint = 0, limits = c(-max_abs_trans_diff, max_abs_trans_diff),
                       name = "Difference in\ntransmission rate") +
  # geom_tile(data = subset(diff_transmission_df, is_total),
  #           aes(x = contact_age, y = participant_age),
  #           fill = "grey94", colour = "white") +
  # geom_text(data = subset(diff_transmission_df, is_total),
  #           aes(x = contact_age, y = participant_age, label = sprintf("%+.1f", diff)),
  #           size = 2.4) +
  #geom_vline(xintercept = n_age + 0.5, linetype = "dashed", colour = "grey60") +
  labs(title = "D. Transmission Matrix Difference \n(Counterfactual - Baseline)",
       x = "Age of contact", y = "Age of participant") +
  coord_fixed() +
  heatmap_theme

combined <- (p_baseline + p_counterfactual) / (p_diff + p_trans_diff)

ggsave("Fig2.pdf", combined,
       width = 12, height = 11, units = "in", dpi = 300)
ggsave("Fig2.png", combined,
       width = 12, height = 11, units = "in", dpi = 300)
