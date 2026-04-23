model_1 <- lm(charges ~ age + bmi + smoker, data = insurance_cost)

model_2 <- lm(charges ~ age + bmi + smoker + chronic_condition + history,
               data = insurance_cost)

model_3 <- lm(log_charges ~ age + bmi + smoker + chronic_condition + history,
              data = insurance_cost)

model_final <- lm(log_charges ~ age * smoker + bmi + chronic_condition + history,
                  data = insurance_cost)


# Residualplot med färger
plot_residuals_grouped <- function(model, data, color_var = smoker, title = NULL) {
  outlier <- abs(rstandard(model)) > 3
  
  ggplot(data, aes(x = fitted(model), y = resid(model), color = {{ color_var }})) +
    geom_point(aes(shape = outlier), size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(
      title = title,
      x = "Fitted values",
      y = "Residualer"
    )
}

# Q-Q plot som matchar plot(model, which = 2)
plot_qq_model <- function(model, title = NULL) {
  res <- rstandard(model)
  qq_obj <- qqnorm(res, plot.it = FALSE)
  
  slope <- diff(quantile(res, c(0.25, 0.75))) / diff(qnorm(c(0.25, 0.75)))
  intercept <- quantile(res, 0.25) - slope * qnorm(0.25)
  
  qq_data <- data.frame(
    theoretical = qq_obj$x,
    sample = qq_obj$y,
    outlier = abs(qq_obj$y) > 3
  )
  
  ggplot(qq_data, aes(theoretical, sample)) +
    geom_point(aes(color = outlier), alpha = 0.7) +
    geom_abline(slope = slope, intercept = intercept, linetype = "dashed") +
    scale_color_manual(values = c("grey70", "red")) +
    theme_minimal() +
    labs(
      title = title,
      x = "Teoretiska kvantiler",
      y = "Standardiserade residualer"
    )
}



