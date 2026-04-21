plot_charges_distribution <- function(data) {
  ggplot(data, aes(x = charges)) +
    geom_histogram(bins = 30, color = "white") +
    labs(
      title = "Fördelning av försäkringskostnader",
      x = "Kostnad",
      y = "Antal"
    )
}

plot_log_charges_distribution <- function(data) {
  ggplot(data, aes(x = log_charges)) +
    geom_histogram(bins = 30, color = "white") +
    labs(
      title = "Log-transformerad fördelning av försäkringskostnader",
      x = "Log(kostnad)",
      y = "Antal"
    )
}

plot_charges_distribution_fill <- function(data, fill_var = smoker) {
  ggplot(data, aes(x = charges, fill = {{ fill_var }})) +
    geom_histogram(bins = 30, alpha = 0.5, position = "identity") +
    labs(
      title = "Fördelning av kostnader per grupp",
      x = "Kostnad",
      y = "Antal"
    )
}




plot_correlation_matrix <- function(data, target = "charges", exclude = "log_charges") {
  
  # Välj numeriska variabler
  numeric_data <- data %>%
    select(where(is.numeric)) %>%
    select(-any_of(exclude))
  
  # Skapa korrelationsmatris
  cor_matrix <- cor(numeric_data)
  
  # Sortera variabler efter absolut korrelation med target
  target_order <- cor_matrix[, target] %>%
    abs() %>%
    sort(decreasing = TRUE) %>%
    names()
  
  # Säkerställ att target ligger först
  ordered_vars <- c(target, setdiff(target_order, target))
  
  # Begränsa och ordna matrisen
  cor_selected <- cor_matrix[ordered_vars, ordered_vars]
  
  # Gör om till långt format
  cor_long <- as.data.frame(cor_selected) %>%
    tibble::rownames_to_column("var1") %>%
    pivot_longer(-var1, names_to = "var2", values_to = "correlation") %>%
    mutate(
      var1 = factor(var1, levels = ordered_vars),
      var2 = factor(var2, levels = rev(ordered_vars))
    )
  
  # Behåll bara halva matrisen
  cor_long_half <- cor_long %>%
    mutate(
      x_id = match(as.character(var1), ordered_vars),
      y_orig = match(as.character(var2), ordered_vars)
    ) %>%
    filter(x_id <= y_orig)
  
  # Position för target-kolumnen
  target_x <- match(target, ordered_vars)
  
  # Rita heatmap
  ggplot(cor_long_half, aes(x = var1, y = var2, fill = correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(correlation, 2)), size = 4) +
    geom_rect(
      xmin = target_x - 0.5, xmax = target_x + 0.5,
      ymin = 0.5, ymax = length(ordered_vars) + 0.5,
      fill = NA, color = "black", linewidth = 1.2
    ) +
    scale_fill_gradient2(
      low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", midpoint = 0,
      limits = c(-1, 1)
    ) +
    labs(
      title = "Korrelationsmatris för numeriska variabler",
      subtitle = paste("Variabler sorterade efter samband med", target),
      x = "",
      y = "",
      fill = "Korrelation"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}


plot_scatter_simple <- function(data, x, y = charges, title = NULL, x_label = NULL, y_label = "Kostnad") {
  if (is.null(x_label)) x_label <- deparse(substitute(x))
  
  ggplot(data, aes(x = {{ x }}, y = {{ y }})) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    theme_minimal()
}
  
plot_scatter_grouped <- function(data, x, y = charges, color_var, title = NULL, x_label = NULL, y_label = "Kostnad", color_label = NULL) {
  if (is.null(x_label)) x_label <- deparse(substitute(x))
  if (is.null(color_label)) color_label <- deparse(substitute(color_var))
  
  ggplot(data, aes(x = {{ x }}, y = {{ y }}, color = {{ color_var }})) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(
      aes(group = 1),
      method = "lm",
      color = "black",
      linetype = "dashed",
      linewidth = 0.7,
      se = FALSE
    ) +
    labs(
      title = title,
      x = x_label,
      y = y_label,
      color = color_label
    ) +
    theme_minimal()
}


summarise_charges_by_group <- function(data, group_var) {
  data %>%
    group_by({{ group_var }}) %>%
    summarise(
      mean_charges = mean(charges, na.rm = TRUE),
      median_charges = median(charges, na.rm = TRUE),
      sd_charges = sd(charges, na.rm = TRUE),
      min_charges = min(charges, na.rm = TRUE),
      max_charges = max(charges, na.rm = TRUE),
      .groups = "drop"
    )
}


plot_charges_by_group <- function(data, group_var, title = NULL, x_label = NULL) {
  ggplot(data, aes(x = {{ group_var }}, y = charges)) +
    geom_boxplot() +
    labs(
      title = title,
      x = x_label,
      y = "Kostnad"
    ) +
    theme_minimal()
}

plot_charges_by_group_factor <- function(data, group_var, title = NULL, x_label = NULL) {
  ggplot(data, aes(x = factor({{ group_var }}), y = charges)) +
    geom_boxplot() +
    labs(
      title = title,
      x = x_label,
      y = "Kostnad"
    ) +
    theme_minimal()
}

create_impact_table <- function(data, outcome = "charges") {
  
  compare_var <- function(data, var, outcome) {
    data %>%
      group_by(.data[[var]]) %>%
      summarise(
        median = median(.data[[outcome]], na.rm = TRUE),
        mean = mean(.data[[outcome]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      summarise(
        median_diff = max(median) - min(median),
        mean_diff = max(mean) - min(mean)
      )
  }
  
  cat_vars <- data %>%
    select(where(is.factor)) %>%
    names()
  
  tibble(variable = cat_vars) %>%
    rowwise() %>%
    mutate(
      stats = list(compare_var(data, variable, outcome))
    ) %>%
    tidyr::unnest(stats) %>%
    ungroup() %>%
    arrange(desc(median_diff))
}


