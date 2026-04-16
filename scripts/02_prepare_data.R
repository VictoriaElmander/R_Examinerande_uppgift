glimpse(insurance_cost_raw)
summary(insurance_cost_raw)
colSums(is.na(insurance_cost_raw))

#Kategoriska variabler
insurance_cost_raw %>% 
  count(sex)
insurance_cost_raw %>% 
  count(region)
insurance_cost_raw %>% 
  count(smoker)
insurance_cost_raw %>% 
  count(chronic_condition)
insurance_cost_raw %>% 
  count(exercise_level)
insurance_cost_raw %>% 
  count(plan_type)


insurance_cost <- insurance_cost_raw %>%
  #Ta bort onödiga mellanslag i alla textvariabler
  mutate(across(where(is.character), str_trim)) %>%
  
  #Standardisera textformat
  mutate(
    sex = str_to_lower(sex),
    region = str_to_title(region),
    smoker = str_to_lower(smoker),
    chronic_condition = str_to_lower(chronic_condition),
    exercise_level = str_to_lower(exercise_level),
    plan_type = str_to_title(plan_type)
  ) %>%
  
  #Gör kategoriska variabler till faktorer
  mutate(
    sex = factor(sex, levels = c("female", "male")),
    region = factor(region),
    smoker = factor(smoker, levels = c("no", "yes")),
    chronic_condition = factor(chronic_condition, levels = c("no", "yes")),
    exercise_level = factor(exercise_level, levels = c("low", "medium", "high")),
    plan_type = factor(plan_type, levels = c("Basic", "Standard", "Premium"))
  ) 

insurance_cost %>% 
  count(region)
insurance_cost %>% 
  count(smoker)
insurance_cost %>% 
  count(chronic_condition)
insurance_cost %>% 
  count(exercise_level)
insurance_cost %>% 
  count(plan_type)


#Kontrollera dubletter  
insurance_cost %>%
  duplicated() %>%
  sum()

insurance_cost %>%
  count(customer_id) %>%
  filter(n > 1)

# Hantera saknade värden
# Saknade värden i kategoriska variabler ersattes med en egen kategori 
# (“unknown”) för att behålla observationer i analysen. För numeriska variabler 
# ersattes saknade värden med medianen, vilket är en robust metod som minskar påverkan av extremvärden.
insurance_cost <- insurance_cost %>%
  mutate(
    exercise_level = forcats::fct_na_value_to_level(exercise_level, "unknown"),
    across(
      c(bmi, annual_checkups),
      ~ if_else(is.na(.), median(., na.rm = TRUE), .)
    )
  )

insurance_cost %>% count(exercise_level)
colSums(is.na(insurance_cost))

# Kontrollera rimlighet i data
insurance_cost %>% summarise(
  min_age = min(age),
  max_age = max(age),
  
  min_bmi = min(bmi),
  max_bmi = max(bmi),
  
  min_children = min(children, na.rm = TRUE),
  max_children = max(children, na.rm = TRUE),
  
  min_accidents = min(prior_accidents),
  max_accidents = max(prior_accidents),
  
  min_claims = min(prior_claims),
  max_claims = max(prior_claims),
  
  min_checkups = min(annual_checkups),
  max_checkups = max(annual_checkups),
  
  min_charges = min(charges),
  max_charges = max(charges)
) %>%
  print(width = Inf)

hist_cost <- hist(insurance_cost$charges) #Snedvriden
hist_cost_log <- hist(log(insurance_cost$charges)) #Mer normalfördelad
hist_BMI <- hist(insurance_cost$bmi)

# Kontrollera förväntat resultat
glimpse(insurance_cost)
summary(insurance_cost)
colSums(is.na(insurance_cost))

#Undersök data innan skapa nya variabler

#Korrelationsmatris för numeriska värden

numeric_data <- insurance_cost %>%
  select(where(is.numeric))

cor_matrix <- cor(numeric_data)

charge_cor <- as.data.frame(cor_matrix[, "charges"]) %>%
  tibble::rownames_to_column("variable") %>%
  rename(correlation_with_charges = 2) %>%
  arrange(desc(abs(correlation_with_charges)))

charge_cor

# -> Både tidigare olyckor och tidigare försäkringsärenden uppvisar positiva 
# samband med kostnader. Då variablerna mäter relaterade aspekter av 
# individens skadehistorik kombinerades de till en gemensam historikvariabel.

# Boxplot för rökare
ggplot(insurance_cost, aes(x = smoker, y = charges)) +
  geom_boxplot() +
  labs(title = "Kostnader beroende på rökstatus")

#scatterplot BMI-charges
ggplot(insurance_cost, aes(x = bmi, y = charges)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Samband mellan BMI och försäkringskostnader",
    x = "BMI",
    y = "Kostnad"
  )

# -> BMI uppvisar en positiv korrelation med kostnader 
# (baserat på korrelationsanalys och scatterplot), och rökstatus visar 
# tydliga skillnader i kostnadsnivåer. Dessa variabler kombinerades därför 
# till en gemensam riskvariabel för att fånga deras samlade effekt.

# Nya variabler
insurance_cost <- insurance_cost %>%
  mutate(
    bmi_category = case_when(
      bmi < 18.5 ~ "underweight",
      bmi < 25 ~ "normal",
      bmi < 30 ~ "overweight",
      TRUE ~ "obese"
    ),
    bmi_category = factor(
      bmi_category,
      levels = c("normal", "underweight", "overweight", "obese")
    ),
    
    bmi_high = if_else(bmi > 30, "high_bmi", "not_high_bmi"),
    bmi_high = factor(bmi_high),
    
    age_group = case_when(
      age < 30 ~ "young",
      age < 50 ~ "middle",
      TRUE ~ "older"
    ),
    age_group = factor(
      age_group,
      levels = c("young", "middle", "older")
    ),
    
    risk_level = case_when(
      smoker == "yes" & bmi > 30 ~ "high",
      smoker == "yes" ~ "medium",
      TRUE ~ "low"
    ),
    risk_level = factor(
      risk_level,
      levels = c("low", "medium", "high")
    ),
    
    history = prior_accidents + prior_claims,
    history_level = case_when(
      history == 0 ~ "none",
      history <= 2 ~ "low",
      TRUE ~ "high"
    ),
    history_level = factor(
      history_level,
      levels = c("none", "low", "high")
    )
  )

insurance_cost %>% count(bmi_category)
insurance_cost %>% count(age_group)
insurance_cost %>% count(risk_level)
insurance_cost %>% count(history_level)


#Nya variabler konstruerades för att förbättra analysens tolkbarhet 
# och fånga underliggande mönster i datan. Kontinuerliga variabler 
# kategoriserades för att möjliggöra jämförelser mellan grupper, medan 
# sammansatta variabler skapades för att integrera flera relaterade faktorer, 
# såsom hälsorisk och tidigare skadehistorik. Detta bidrar till en mer 
# informativ och analytiskt användbar datamängd.
