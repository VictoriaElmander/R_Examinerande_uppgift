#
insurance_cost <- insurance_cost_raw %>%
  mutate(across(where(is.character), str_trim)) %>%
  mutate(
    sex = factor(str_to_lower(sex), levels = c("female", "male")),
    region = factor(str_to_title(region)),
    smoker = factor(str_to_lower(smoker),levels = c("no", "yes")),
    chronic_condition = factor(str_to_lower(chronic_condition), levels = c("no", "yes")),
    exercise_level = factor(str_to_lower(exercise_level),levels = c("low", "medium", "high")),
    plan_type = factor(str_to_title(plan_type), levels = c("Basic", "Standard", "Premium"))
  ) %>%
  mutate(
    exercise_level = forcats::fct_na_value_to_level(exercise_level, "unknown"),
    # exercise_level = forcats::fct_relevel(exercise_level, "unknown", after = Inf),
    across(
      c(bmi, annual_checkups),
      ~ if_else(is.na(.), median(., na.rm = TRUE), .)
    )
  ) %>%
  mutate(
    log_charges = log(charges),
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
    age_group = case_when(
      age < 30 ~ "young",
      age < 50 ~ "middle",
      TRUE ~ "older"
    ),
    age_group = factor(age_group,levels = c("young", "middle", "older")),
    risk_level = case_when(
      smoker == "yes" & bmi > 30 ~ "high",
      smoker == "yes" ~ "medium",
      TRUE ~ "low"
    ),
    risk_level = factor(risk_level,levels = c("low", "medium", "high")),
    history = prior_accidents + prior_claims,
    history_level = case_when(
      history == 0 ~ "none",
      history <= 2 ~ "low",
      TRUE ~ "high"
    ),
    history_level = factor(history_level,levels = c("none", "low", "high"))
  )
    