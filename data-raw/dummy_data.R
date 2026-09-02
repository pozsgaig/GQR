# Rebuild the synthetic dummy_data example used by GQR.
# This development script is not included in the installed package.

dummy_data <- data.frame(
  Respondent = paste0("R", 1:10),
  Q1 = c(0.65, 0.07, 0.41, 0.24, 0.14, 0.53, 0.14, 0.35, 0.05, 0.98),
  Q2 = c(0.77, 0.73, 0.15, 0.48, 0.17, 0.67, 0.62, 0.25, 0.23, 0.50),
  Q3 = c(0.14, 0.73, 0.74, 0.62, 0.09, 0.10, 0.10, 0.03, 0.82, 0.82),
  Q4 = c(0.57, 0.98, 0.05, 0.82, 0.08, 0.47, 0.25, 0.14, 0.63, 0.31),
  Q5 = c(0.61, 0.77, 0.62, 0.95, 0.96, 0.16, 0.36, 0.88, 0.91, 0.13),
  Q6 = c(0.73, 0.41, 0.75, 0.65, 0.61, 0.22, 0.81, 0.19, 0.37, 0.30),
  Q7 = c(0.25, 0.51, 0.58, 0.58, 0.53, 0.84, 0.85, 0.96, 0.74, 0.64),
  Q8 = c(0.70, 0.05, 0.98, 0.08, 0.54, 0.93, 0.42, 0.56, 0.16, 0.68),
  Q9 = c(0.92, 0.13, 0.90, 0.20, 0.97, 0.48, 0.04, 0.38, 0.48, 0.68),
  Numeric_covariate = c(42, 27, 61, 35, 50, 23, 68, 44, 56, 31),
  Factor_covariate = factor(
    c("A", "B", "A", "B", "A", "B", "A", "B", "A", "B"),
    levels = c("A", "B")
  )
)

save(dummy_data, file = "data/dummy_data.rda", version = 3)
save(dummy_data, file = "inst/extdata/dummy_data.RDA", version = 3)
