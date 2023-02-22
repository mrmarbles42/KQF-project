require(ggplot2)
require(tidyverse)
require(lubridate)

#ggplot(fruit_data, aes(Variety, GTL)) +
# geom_boxplot()

ggplot(fruit_data, aes(Variety, GTL)) +
  geom_boxplot()

ggplot(fungicide_use, aes(active_ingredient)) + 
  geom_bar()

ggplot(fungicide_use, aes(date, active_ingredient)) + 
  geom_point()

hist(pesticide_use$application_date, breaks = 100)

#rot percentage by berry weight
cig_yield %>%
  ggplot(aes(wt_p_berry, rot_pct)) +
  geom_point() +
  geom_smooth()

#rot percent by yield
cig_yield %>%
  ggplot(aes(yield_p_ac_kg, rot_pct)) +
  geom_point() +
  geom_smooth(method = "lm")


levels(as.factor(fruit_14$Variety))

# Rot by variety by year(decas)

fruit_13 %>%
  filter(Variety == c("BL", "EB", "HO", "MX", "OS", "ST", "ORHO", "ORST")) %>%
  ggplot(aes(Variety, Rot)) +
  geom_boxplot()

fruit_14 %>%
  filter(Variety == c("BL", "EB", "HO", "MX", "OS", "ST", "ORHO", "ORST")) %>%
  ggplot(aes(Variety, Rot)) +
  geom_boxplot()

fruit_15 %>%
  filter(Variety == c("BL", "EB", "HO", "MX", "OS", "ST", "ORHO", "ORST")) %>%
  ggplot(aes(Variety, Rot)) +
  geom_boxplot()

fruit_16 %>%
  filter(Variety == c("BL", "EB", "HO", "MX", "OS", "ST", "ORHO", "ORST")) %>%
  ggplot(aes(Variety, Rot)) +
  geom_boxplot()

fruit_17 %>%
  filter(Variety == c("BL", "EB", "HO", "MX", "OS", "ST", "ORHO", "ORST")) %>%
  ggplot(aes(Variety, Rot)) +
  geom_boxplot()

fruit_18 %>%
  filter(Variety == c("BL", "EB", "HO", "MX", "OS", "ST", "ORHO", "ORST")) %>%
  ggplot(aes(Variety, Rot)) +
  geom_boxplot()

#variety by rot
comb_data %>%
  ggplot(aes(Variety, Rot)) +
  geom_point()

#fruit data aov
decas_rot_by_varv <- lm(fruit_data$Rot ~ fruit_data$Variety)
rot_resid <- residuals(decas_rot_by_varv)
hist(rot_resid)
rot_var_aov <- aov(decas_rot_by_varv)  
plot(TukeyHSD(rot_var_aov))

pest_data_2 %>%
  ggplot(aes(active_ingredient)) +
  geom_boxplot()

(aov(lm(comb_data$Rot ~ comb_data$Variety)))

kqf_data_combined %>%
  group_by(year)%>%
  filter(month %in% 9:12,
         year %in% 2013:2022) %>%
  ggplot(aes(factor(month), rot_pct, shape = factor(year))) +
  geom_jitter(alpha = 0.2) 

cor.test(kqf_data_combined$log_rot, kqf_data_combined$avg_temp_f, 
         method = "kendall",
         exact = F)
