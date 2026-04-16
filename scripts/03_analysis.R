library(patchwork)


# Fördelning av kostnader
# Högerfördelade vilket gör att i regression som kräver normalfördelning bör log användas
distr_insurance_cost <- ggplot(insurance_cost, aes(x = charges)) +
  geom_histogram(bins = 30, color = "white") +
  labs(
    title = "Fördelning av försäkringskostnader",
       x = "Kostnad",
       y = "Antal"
    )

#Kostnaderna är högerskev fördelade, med många individer med låga 
#kostnader och ett fåtal med mycket höga. Detta indikerar att vissa individer 
#står för en stor del av kostnaderna.

insurance_cost <- insurance_cost %>%
  mutate(log_charges = log(charges))

distr_log_insurance_cost <- ggplot(insurance_cost, aes(x = log_charges)) +
  geom_histogram(bins = 30, color = "white") +
  labs(
    title = "Log-transformerad fördelning av försäkringskostnader",
  x = "Log(kostnad)",
  y = "Antal"
  )

# Korrelationsmatris
  # Numeriska variabler 
numeric_data <- insurance_cost %>%
  select(where(is.numeric)) %>%
  select(-log_charges) 

  # Skapa korrelationsmatris
cor_matrix <- cor(numeric_data)

  # Sortera variabler efter absolut korrelation med charges
charge_order <- cor_matrix[, "charges"] %>%
  abs() %>%
  sort(decreasing = TRUE) %>% 
  names()

  # Säkerställ att charges ligger först
ordered_vars <- c("charges", setdiff(charge_order, "charges"))

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

  # Position för charges-kolumnen
charges_x <- match("charges", ordered_vars)

  # Rita heatmap
ggplot(cor_long_half, aes(x = var1, y = var2, fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlation, 2)), size = 4) +
  # Ritar svart ruta runt charges kolumnen
  geom_rect(
    xmin = charges_x - 0.5, xmax = charges_x + 0.5,
    ymin = 0.5, ymax = length(ordered_vars) + 0.5,
    fill = NA, color = "black", linewidth = 1.2
  ) +
  scale_fill_gradient2(
    low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = "Korrelationsmatris för numeriska variabler",
    subtitle = "Variabler sorterade efter samband med charges",
    x = "",
    y = "",
    fill = "Korrelation"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ----------------------------------------------------------
# ----------------------------------------------------------
# Scatterplotter för kontinuerliga variabler ålder och BMI

ggplot(insurance_cost, aes(x = age, y = charges)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(
    title = "Samband mellan ålder och försäkringskostnader",
    x = "Ålder",
    y = "Kostnad"
  )

ggplot(insurance_cost, aes(x = age, y = charges, color = smoker)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm",            # gemensam linje
              color = "black", linetype = "dashed", 
              linewidth = 0.7, se = FALSE ) +
  labs(
    title = "Ålder, rökstatus och kostnader",
    x = "Ålder",
    y = "Kostnad"
  )

ggplot(insurance_cost, aes(x = age, y = charges, color = history_level)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm",            # gemensam linje
              color = "black", linetype = "dashed", 
              linewidth = 0.7, se = FALSE ) +
  labs(
    title = "Ålder, historik och kostnader",
    x = "Ålder",
    y = "Kostnad"
  )

scc <- ggplot(insurance_cost, aes(x = bmi, y = charges)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(
    title = "Samband mellan BMI och försäkringskostnader",
    x = "BMI",
    y = "Kostnad"
  )

scatter_char_BMI_smoker <- ggplot(insurance_cost, aes(x = bmi, y = charges, color = smoker)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm",            # gemensam linje
              color = "black", linetype = "dashed", 
              linewidth = 0.7, se = FALSE ) +
  labs(
    title = "BMI, rökstatus och försäkringskostnader",
    x = "BMI",
    y = "Kostnad"
  )
(scc| scatter_char_BMI_smoker) 

# Kostnader per rökstatus
insurance_cost %>%
  group_by(smoker) %>%
  summarise(
    mean_charges = mean(charges),
    median_charges = median(charges),
    sd_charges = sd(charges),
    min_charges = min(charges),
    max_charges = max(charges)
  )

plt_smoker <- ggplot(insurance_cost, aes(x = smoker, y = charges)) +
  geom_boxplot() +
  labs(title = "Kostnader beroende på rökstatus")
# Rökare har generellt högre kostnader än icke-rökare. Dessutom är variationen 
# större bland rökare, vilket tyder på att rökning är en viktig faktor för 
# kostnadsnivån.

#Kategoriska variabler
# Kategorisering av numeriska variabler kan tydliggöra skillnader mellan 
#grupper, men innebär samtidigt en informationsförlust. Samband som är tydliga 
#i scatterplots återspeglas därför inte alltid lika tydligt i grupperade 
#analyser.

# Kostnader per BMI-kategori
insurance_cost %>%
  group_by(bmi_category) %>%
  summarise(
    mean_charges = mean(charges),
    median_charges = median(charges),
    sd_charges = sd(charges),
    min_charges = min(charges),
    max_charges = max(charges)
  )

plt_BMI <- ggplot(insurance_cost, aes(x = bmi_category, y = charges)) +
  geom_boxplot() +
  labs(title = "Kostnader per BMI-kategori")
#Individer i kategorin “obese” har generellt högre kostnader än övriga grupper. 
# Detta tyder på ett samband mellan hög BMI och ökade försäkringskostnader.

# Kostnader per risknivå
insurance_cost %>%
  group_by(risk_level) %>%
  summarise(
    mean_charges = mean(charges),
    median_charges = median(charges),
    sd_charges = sd(charges),
    min_charges = min(charges),
    max_charges = max(charges)
  )

plt_risk <- ggplot(insurance_cost, aes(x = risk_level, y = charges)) +
  geom_boxplot() +
  labs(title = "Kostnader per risknivå")
#Kostnaderna ökar tydligt med risknivå, vilket tyder på att variabeln fångar 
#relevant information om individens riskprofil.

# Kostnader per history-level
insurance_cost %>%
  group_by(history_level) %>%
  summarise(
    mean_charges = mean(charges),
    median_charges = median(charges),
    sd_charges = sd(charges),
    min_charges = min(charges),
    max_charges = max(charges)
  )
plt_history <- ggplot(insurance_cost, aes(x = history_level, y = charges)) +
  geom_boxplot() +
  labs(title = "Kostnader per historiknivå")


# Kostnader per ålder
insurance_cost %>%
  group_by(age_group) %>%
  summarise(
    mean_charges = mean(charges),
    median_charges = median(charges),
    sd_charges = sd(charges),
    min_charges = min(charges),
    max_charges = max(charges)
  )
plt_age <- ggplot(insurance_cost, aes(x = age_group, y = charges)) +
  geom_boxplot() +
  labs(title = "Kostnader per åldersgrupp")


# Kostnader per antal barn
insurance_cost %>%
  group_by(children) %>%
  summarise(
    mean_charges = mean(charges),
    median_charges = median(charges),
    sd_charges = sd(charges),
    min_charges = min(charges),
    max_charges = max(charges)
  )
plt_age <- ggplot(insurance_cost, aes(x = factor(children), y = charges)) +
  geom_boxplot() +
  labs(title = "Kostnader per antal barn")

ggplot(insurance_cost, aes(x = children, y = charges)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(title = "Kostnader i relation till antal barn")


compare_var <- function(var) {
  insurance_cost %>%
    group_by({{var}}) %>%
    summarise(mean = mean(charges)) %>%
    summarise(diff = max(mean) - min(mean)) %>%
    pull(diff)
}

tibble(
  variable = c("smoker", "risk_level", "bmi_category", "history_level"),
  impact = c(
    compare_var(smoker),
    compare_var(risk_level),
    compare_var(bmi_category),
    compare_var(history_level)
  )
)

plt_box_rh <- (plt_risk | plt_history)


