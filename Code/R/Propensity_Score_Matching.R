# ------ 5350 Thesis in Economics ------ #             
# ---- Andres Cruz (25199) and Klara Holmer (25037) ---- #

# Propensity Score Matching 


#
#
#
#
#
#
#
#

# -------- Initializing -------- #

# Define the output path for Propensity Score Matching Results
path_output <- file.path("~/Documents/Handelshögskolan/MSc Economic/Semester 4/5350 Thesis in Economics/Output/Propensity Score Matching")

#
#
#
#
#
#
#
#

# -------- Propensity Score Matching -------- #

# Reference I: https://cran.r-project.org/web/packages/MatchIt/vignettes/MatchIt.html#ref-vanderweele2019
# Reference II: https://simonejdemyr.com/r-tutorials/statistics/tutorial8.html
# Reference III: https://rpubs.com/Paul_Rucki/934261
# Reference IIII: https://cran.r-project.org/web/packages/MatchIt/vignettes/matching-methods.html#generalized-full-matching-method-quick
# Reference IIIII: https://dlab.berkeley.edu/news/introduction-propensity-score-matching-matchit
# Reference IIIIII: https://cran.r-project.org/web/packages/MatchIt/vignettes/assessing-balance.html

# Filter for pre-treatment period only
school_data_pre <- school_data %>%
  filter(academic_year == '14/15') 

#
#
#
#
#
#
 
# --- No Matching: Checking covariate imbalance before matching --- # 
unmatched_object <- matchit(
  treatment_group ~ share_active_certified_teachers + 
    share_postsecondary_parents + 
    share_foreign_background, 
  data    = school_data_pre,
  method  = NULL,
  distance = 'glm',
  link    = 'logit'
)

# 1. Summarize unmatched object
sum_unmatched <- summary(unmatched_object, standardize = TRUE)

# 2. Extract full unmatched balance matrix
bal_all <- as.data.frame(sum_unmatched$sum.all)

# 3. Drop any Pair columns (all NA for method = NULL)
bal_all <- bal_all[, !grepl("Pair", names(bal_all))]

# 4. Move row names into a column
bal_all$Covariate <- rownames(bal_all)
rownames(bal_all) <- NULL

# 5. Reorder so Covariate is first
bal_all <- bal_all[, c("Covariate", setdiff(names(bal_all), "Covariate"))]

# >>>>>>>> NEW PART: Add Standard Deviations <<<<<<<<

# 5.1 Extract full unmatched data
unmatched_data <- match.data(unmatched_object)

# 5.2 Split into treatment groups
treated <- unmatched_data[unmatched_data$treatment_group == 1, ]
control <- unmatched_data[unmatched_data$treatment_group == 0, ]

# 5.3 Identify covariates
covariates <- bal_all$Covariate

# 5.4 Compute standard deviations
sd_table <- data.frame(
  Covariate  = covariates,
  SD_Treated = sapply(covariates, function(v) sd(treated[[v]],  na.rm = TRUE)),
  SD_Control = sapply(covariates, function(v) sd(control[[v]], na.rm = TRUE))
)

# 5.5 Merge into balance table
bal_all <- left_join(bal_all, sd_table, by = "Covariate")

# >>>>>>>> END OF NEW PART <<<<<<<<

# 6. Build alignment vector
n_data_cols <- max(ncol(bal_all) - 1, 0)
align_vec   <- c("l", rep("r", n_data_cols))

# 7. Render to LaTeX
latex_code <- kable(
  bal_all,
  format    = "latex",
  booktabs  = TRUE,
  caption   = "Covariate Imbalance Before Matching (Standardized Differences and SDs)",
  label     = "tab:balance_before",
  digits    = 3,
  align     = align_vec
)

# 8. Save to file
out_file <- file.path(path_output, "matchit_unmatched_summary.tex")
writeLines(latex_code, con = out_file)

#
#
#
#
#
#

# --- Nearest Neighbor Matching: Construct the matched object --- #
matched_object <- matchit(
  treatment_group ~ share_foreign_background + share_active_certified_teachers + share_postsecondary_parents, 
  data = school_data_pre,
  method = 'nearest', 
  distance = 'glm',
  link = 'logit',            # Using logistic regression to estimate propensity scores
  m.order = "largest",       # Matching starts with the treated units with highest propensity scores
  ratio = 1,                 # Each treated unit is matched to one control unit
  replace = FALSE,            # Do not reuse control units
  exact = ~private_ownership + school_municipality, # Match only within private/public school and municipality
  caliper = 0.95
  )

#
#
#
#
# - Matched Summary - #
# 1. Summarize matched object (post-matching only)
sum_matched <- summary(
  matched_object,
  standardize = TRUE,
  un          = FALSE
)

# 2. Pull out the matched-only balance matrix
bal_matched <- as.data.frame(sum_matched$sum.matched)

# 3. Drop the all-NA pair-diff column(s)
bal_matched <- bal_matched[, !grepl("Pair", names(bal_matched))]

# 4. Turn rownames into a column
bal_matched$Covariate <- rownames(bal_matched)
rownames(bal_matched) <- NULL

# 4.1 Drop municipality dummy variables
bal_matched <- bal_matched %>%
  filter(!grepl("^school_municipality", Covariate))

# 5. Reorder so Covariate is first
bal_matched <- bal_matched[, c("Covariate", setdiff(names(bal_matched), "Covariate"))]

# >>>>>>>> NEW PART: Add SDs <<<<<<<<

# 5.1 Get matched data
matched_data <- match.data(matched_object)

# 5.2 Split into groups
treated <- matched_data[matched_data$treatment_group == 1, ]
control <- matched_data[matched_data$treatment_group == 0, ]

# 5.3 Identify covariates
covariates <- bal_matched$Covariate

# 5.4 Compute SDs
sd_table <- data.frame(
  Covariate  = covariates,
  SD_Treated = sapply(covariates, function(v) sd(treated[[v]],  na.rm = TRUE)),
  SD_Control = sapply(covariates, function(v) sd(control[[v]], na.rm = TRUE))
)

# 5.6 Merge into balance matrix
bal_matched <- left_join(bal_matched, sd_table, by = "Covariate")

# >>>>>>>> END OF NEW PART <<<<<<<<

# 6. Build alignment vector
n_data_cols <- max(ncol(bal_matched) - 1, 0)
align_vec   <- c("l", rep("r", n_data_cols))

# 7. Render to LaTeX
latex_code <- kable(
  bal_matched,
  format    = "latex",
  booktabs  = TRUE,
  caption   = "Covariate Imbalance After Matching (Excluding Exact‐Match Variables)",
  label     = "tab:balance_after",
  digits    = 3,
  align     = align_vec
)

# 8. Save to file
out_file <- file.path(path_output, "matchit_matched_summary.tex")
writeLines(latex_code, con = out_file)

#
#

# - Histogram Plot - #
# Creating plot
histogram_plot <- bal.plot(
  matched_object, 
  var.name = "distance", 
  type     = "histogram", 
  which    = "both"       # plots pre- and post-matching
)

# Designing the plot
histogram_plot <- histogram_plot +
  scale_fill_manual(
    name   = "Group",
    values = c("brown1", "skyblue1"),
    labels = c("Lower Secondary", "Upper Secondary")) +
  labs(x = "Propensity Score", title = NULL) + 
  my_theme
print(histogram_plot)

# Save with ggsave 
output_file <- file.path(path_output, paste0("histogram_plot.png"))
ggsave(output_file, histogram_plot, width = 5, height = 4)

#
#
#
#

#eQQ plot
eQQ_plot <- plot(matched_object, type = "qq", which.xs = ~share_active_certified_teachers + share_postsecondary_parents +
                  share_foreign_background) 
print(eQQ_plot)
# SAVE MANUALLY

#eCDF plot
eCDF_plot <- plot(matched_object, type = "ecdf", which.xs = ~share_active_certified_teachers + share_postsecondary_parents + 
                    share_foreign_background)
print(eCDF_plot)
# SAVE MANUALLY

#
#
#
#

# - Density Plots - "

# All in One
plot(matched_object, type = "density", which.xs = ~share_active_certified_teachers + share_postsecondary_parents + 
       share_foreign_background)



# -------- Finalizing -------- #

# Convert matched object to data set 
matched_dataframe <- match.data(matched_object)

# Filter original panel data to include only matched schools, and dropping missing values
school_data <- school_data %>%
  filter(school_ID %in% matched_dataframe$school_ID) 


