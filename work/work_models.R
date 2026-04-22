
model_1 <- lm(charges ~ age + bmi + smoker, data = insurance_cost)

summary(model_1)
par(mfrow = c(2, 2)) 
plot(model_1)
# Median nära 0 villket är bra, modellen är ungefär centrerad.-Residualernas 
# median är nära noll, vilket indikerar att modellen inte är systematiskt sned.
#Stora max och min, vissa datapunkter dåligt förklarade, outliers och variation

# Koeffecienten smoker har mycket stor inverkan, Rökare har i genomsnitt cirka 
# 8000 högre kostnader än icke-rökare, givet övriga variabler: age 89 och bmi 163. 

# Pr(>|t|) alla utom INtercept är mycket signifikanta

# R2 0.5437 och adj 0,5425, nästan samman vilket indikerar stabil och 
# inga onödiga variabler. Men modellen förklarar endast ca 54 % av variationen 
# så mycket förklaras ej. 

# F-statistic: 435.3 on 3 and 1096 DF,  p-value: < 2.2e-16, 
# modellen är klart bättre än ingen modell

# Residual 
plot(model_1, which = 1)

# uppdelat på rökare icke rökare
# ????? kommentera outliers
which(abs(rstandard(model_1)) > 3) #är de verkliga? (ofta ja i kostnadsdata)
outlier <- abs(rstandard(model_1)) > 3

ggplot(insurance_cost, aes(x = fitted(model_1), y = resid(model_1), color = smoker)) +
  geom_point(aes(color = smoker, shape = outlier), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal()


# Q-Q-plot
# ok i mitten, avviker i svansarna, residualer inte helt normalfördelade. 
#extrema positiva outliers i plot utmärkta.
# Modellen underskattar vissa höga värden rejält, vanligt i kostnadsdata

plot(model_1, which = 2)
# plottarna ser olika ut, pga ggplot gör annan kvantilberäkning
# Data för diagnostik
diag_data <- data.frame(
  std_resid = rstandard(model_1),
  outlier = abs(rstandard(model_1)) > 3,
  smoker = insurance_cost$smoker
)



q_q_1 <- ggplot(diag_data, aes(sample = std_resid)) +
  stat_qq(aes(color = outlier), alpha = 0.6) +
  stat_qq_line(linetype = "dashed") +
  scale_color_manual(values = c("grey70", "red")) +
  theme_minimal() +
  labs(
    title = "Q-Q plot (outliers markerade)",
    x = "Teoretiska kvantiler",
    y = "Standardiserade residualer"
  )

q_q_1

q_q_1_smoker <- ggplot(diag_data, aes(sample = std_resid, color = smoker)) +
  stat_qq(alpha = 0.6) +
  stat_qq_line(linetype = "dashed") +
  theme_minimal()

(q_q_1 | q_q_1_smoker)

# gör plot(model_1, which = 2) i ggplot
res_1 <- rstandard(model_1)
qq_obj_1 <- qqnorm(res_1, plot.it = FALSE)

slope_1 <- diff(quantile(res_1, c(0.25, 0.75))) / diff(qnorm(c(0.25, 0.75)))
intercept_1 <- quantile(res_1, 0.25) - slope_1 * qnorm(0.25)

qq_data_1 <- data.frame(
  theoretical = qq_obj_1$x,
  sample = qq_obj_1$y,
  outlier = abs(qq_obj_1$y) > 3
)

ggplot(qq_data_1, aes(theoretical, sample)) +
  geom_point(aes(color = outlier), alpha = 0.7) +
  geom_abline(slope = slope_1, intercept = intercept_1, linetype = "dashed") +
  scale_color_manual(values = c("grey70", "red")) +
  theme_minimal() +
  labs(
    title = "Q-Q plot (modell 1)",
    x = "Teoretiska kvantiler",
    y = "Standardiserade residualer"
  )


# test av heteroskedasticitet
# plot visar ingen direkt trend (svag), att spridning ökar med
# Det finns inga starka visuella bevis för heteroskedasticitet, även om en svag 
#tendens till ökad spridning vid högre värden kan anas.

plot(model_1, which = 3)

#Breusch-Pagan test (p < 0.05, heteroskedasticitet finns) 
# p värde mindre än 0,05 så statistiskt stöd för heteroskedasticitet
bptest(model_1)

# slutsats:
# Breusch-Pagan-testet indikerar heteroskedasticitet (p = 0.024). Dock visar de 
# grafiska analyserna endast en svag tendens till ökande spridning, vilket 
# tyder på att problemet inte är särskilt allvarligt.

# Sammanfattande slutsats:
# - Modellen är statistiskt signifikant och fångar viktiga samband, särskilt 
# effekten av rökning.
# - Den förklarar cirka 54% av variationen i kostnader.
# - Residualanalysen visar vissa avvikelser från modellantaganden, särskilt i 
# form av outliers 
# och svag heteroskedasticitet.
# - Sammantaget fungerar modellen relativt väl, men har begränsningar i att 
# förklara extrema värden.
# - Modellen är användbar för att beskriva generella samband, men mindre tillförlitlig för att predicera extrema kostnader.

#----------------------------------------------------------------------
#----------------------------------------------------------------------
# Modell 2
# väljer 2a, numerisk, mer info, ungefär linjärt.
ggplot(insurance_cost, aes(x = history, y = charges)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal()

model_2a <- lm(charges ~ age + bmi + smoker + chronic_condition + history,
              data = insurance_cost)
summary(model_2a)
plot(model_2a)

model_2b <- lm(charges ~ age + bmi + smoker + chronic_condition + history,
               data = insurance_cost)
summary(model_2b)

# Residual  
plot(model_2a, which = 1)
which(abs(rstandard(model_2a)) > 3) #är de verkliga? (ofta ja i kostnadsdata)
outlier <- abs(rstandard(model_2a)) > 3

ggplot(insurance_cost, aes(x = fitted(model_2a), y = resid(model_2a), color = smoker)) +
  geom_point(aes(color = smoker, shape = outlier), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal()

# Q-Q plot
plot(model_2a, which = 2)

diag_data_2a <- data.frame(
  std_resid = rstandard(model_2a),
  outlier = abs(rstandard(model_2a)) > 3,
  smoker = insurance_cost$smoker
)



q_q_2a <- ggplot(diag_data_2a, aes(sample = std_resid)) +
  stat_qq(aes(color = outlier), alpha = 0.6) +
  stat_qq_line(linetype = "dashed") +
  scale_color_manual(values = c("grey70", "red")) +
  theme_minimal() +
  labs(
    title = "Q-Q plot (outliers markerade)",
    x = "Teoretiska kvantiler",
    y = "Standardiserade residualer"
  )
q_q_2a
 # gör plot(model_2a, which = 2) i ggplot
res_2a <- rstandard(model_2a)
qq_obj_2a <- qqnorm(res_2a, plot.it = FALSE)

slope_2a <- diff(quantile(res_2a, c(0.25, 0.75))) / diff(qnorm(c(0.25, 0.75)))
intercept_2a <- quantile(res_2a, 0.25) - slope_2a * qnorm(0.25)

qq_data_2a <- data.frame(
  theoretical = qq_obj_2a$x,
  sample = qq_obj_2a$y,
  outlier = abs(qq_obj_2a$y) > 3
)

ggplot(qq_data_2a, aes(theoretical, sample)) +
  geom_point(aes(color = outlier), alpha = 0.7) +
  geom_abline(slope = slope_2a, intercept = intercept_2a, linetype = "dashed") +
  scale_color_manual(values = c("grey70", "red")) +
  theme_minimal() +
  labs(
    title = "Q-Q plot (modell 2a)",
    x = "Teoretiska kvantiler",
    y = "Standardiserade residualer"
  )

# Q-Q-plottet visar att residualerna följer normalfördelningen relativt väl i 
# mitten av fördelningen. Däremot avviker observationerna tydligt i svansarna, 
# särskilt i den övre delen där flera stora positiva residualer förekommer. 
# Detta indikerar att modellen har svårare att fånga mycket höga kostnader. 
# Endast en enstaka avvikelse observeras i den nedre svansen.

# Q-Q-plottarna för modell 1 och modell 2a är relativt lika och visar i båda 
# fallen avvikelser i den övre svansen. Detta tyder på att tillägget av nya 
# prediktorer inte i någon större utsträckning förbättrar residualernas 
# normalitet. (Däremot förbättras modellens anpassning enligt förklaringsgrad 
# och residual standard error, vilket innebär att modell 2a ändå är att föredra.)

# test av heteroskedasticitet
# inte statistiskt stöd för heteroskedasticitet
# visuellt: svag heteroskedasticitet
# 
plot(model_2a, which = 3)
bptest(model_2a)


#----------------------------------------------------------------------
#----------------------------------------------------------------------
# Modell 3
# log av charges för ökad normalitet
# koefficienter ungefär procentuella effekter

model_3 <- lm(log_charges ~ age + bmi + smoker + chronic_condition + history,
              data = insurance_cost)

summary(model_3)
summary(model_2a)
plot(model_3)

# Residual  
plot(model_3, which = 1)
which(abs(rstandard(model_3)) > 3) #är de verkliga? (ofta ja i kostnadsdata)
outlier <- abs(rstandard(model_3)) > 3

ggplot(insurance_cost, aes(x = fitted(model_3), y = resid(model_3), color = smoker)) +
  geom_point(aes(color = smoker, shape = outlier), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal()




plot(model_3, which = 2)

# gör plot(model_3, which = 2) i ggplot
res_3 <- rstandard(model_3)
qq_obj_3 <- qqnorm(res_3, plot.it = FALSE)

slope_3 <- diff(quantile(res_3, c(0.25, 0.75))) / diff(qnorm(c(0.25, 0.75)))
intercept_3 <- quantile(res_3, 0.25) - slope_3 * qnorm(0.25)

qq_data_3 <- data.frame(
  theoretical = qq_obj_3$x,
  sample = qq_obj_3$y,
  outlier = abs(qq_obj_3$y) > 3
)

ggplot(qq_data_3, aes(theoretical, sample)) +
  geom_point(aes(color = outlier), alpha = 0.7) +
  geom_abline(slope = slope_3, intercept = intercept_3, linetype = "dashed") +
  scale_color_manual(values = c("grey70", "red")) +
  theme_minimal() +
  labs(
    title = "Q-Q plot (modell 3)",
    x = "Teoretiska kvantiler",
    y = "Standardiserade residualer"
  )
# förbättrar normalitet i mitten
# svansarna mer symmetriska
#  mindre skevhet


# test av heteroskedasticitet
# starkt statistiskt stöd för heteroskedasticitet
# visuellt: svag heteroskedasticitet

plot(model_3, which = 3)
bptest(model_3)
bptest(model_2a)

#----------------------------------------------------------------------
#----------------------------------------------------------------------
# Modell 4
# log av charges och 

model_4 <- lm(log_charges ~ age + bmi * smoker +
                chronic_condition + history,
              data = insurance_cost)

model_4b <- lm(log_charges ~ age * smoker + bmi * smoker +
                 chronic_condition + history,
               data = insurance_cost)

summary(model_4)
summary(model_4b)
plot(model_4b)
# model 4: bmi:smoker p = 0.859 ingen signifikans
# model 4b:  age:smoker p=,001 signifikant. bmi:smoker fortfarande inte signifikant

model_final <- lm(log_charges ~ age * smoker + bmi +
                 chronic_condition + history,
               data = insurance_cost)

summary(model_final)
plot(model_final)
# smokeryes = 0.925, exp(0.925) ≈ 2.52  Rökare har cirka 150 % högre kostnader
# age = 0.0094 ≈  0.94 % ökning per år
# age:smokeryes = -0.006: ålder påverkar mindre för rökare
# 0.0094 - 0.006 = 0.0034 ≈ 0.34% per år Ålder har en svagare effekt på kostnader för rökare än för icke-rökare
# Slutsats: Rökare, redan höga kostnader. ålder mindre relevant
# Icke rökare, ålder driver kostnader mer. 

