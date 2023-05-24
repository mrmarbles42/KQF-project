
library(here)
library(lme4)
library(psych)
source(here("code", "kqf_clean.R"))

##Summary statistics----

#september mean
fruit_data_wide %>%
  group_by(bog) %>%
  filter(is.na(temp_9) == F) %>%
  summarize(max = max(temp_9),
            min = min(temp_9),
            mean = mean(temp_9),
            median = median(temp_9),
            n = n()) %>%
  arrange(bog, .by_group = T)

# Measures of center/Measures of spread----

# #what are average rot percentages by kqf level?
# rot_by_kqf <- fruit_data_combined %>%
#   group_by(final_points) %>%
#   summarize(mean = mean(rot_pct, na.rm = T))

# 
#average rot percentage by variety
fruit_data_wide %>%
  group_by(variety) %>%
  summarize(mean = mean(rot_pct, na.rm = T))

fruit_data_wide %>%
  filter(is.na(rot_pct) == F) %>%
  group_by(variety) %>%
  summarize(mean = mean(rot_pct, na.rm = T))

fruit_data_wide %>%
  filter(is.na(rot_pct) == F) %>%
  group_by(variety) %>%
  summarize(mean = mean(rot_pct, na.rm = T))

fruit_data_wide %>%
  group_by(variety) %>%
  summarize(mean = mean(rot_pct, 
                        na.rm = T)) %>%
  arrange(desc(mean), .by_group = T)
# 
# #average rot by data source
# rot_by_source <- fruit_data_combined %>%
#   group_by(data_source) %>%
#   summarize(mean = mean(rot_pct, na.rm = T))
# 
# #average rot by year
# rot_by_year <- fruit_data_combined %>%
#   group_by(year) %>%
#   summarise(mean = mean(rot_pct, na.rm = T))
# 
# #relationship between fungicide application counts and kqf by year
# application_by_kqf_year <- pest_data_combined %>%
#   group_by(year) %>%
#   filter(year %in% 2013:2018,
#          ingredient %in% c("Chlorothalonil", "Azoxystrobin", "Fenbuconazole",
#                            "EBDC", "Copper Hydroxide")) %>%
#   count(ingredient, sort = T) 
#   
# 

#relation between cumulative precip and kqf in march-october
fruit_data_norm %>%
  group_by(year) %>%
  filter(month %in% 3:10,
         year %in% 2013:2018) %>%
  summarize(mean_precip = mean(tot_precip),
            final_points = factor(unique(final_points)))



#NLME model & diagnostics----

##September
m_9 <- lme4::lmer(log_rot ~ temp_9 + precip_9 + (1 | bog),
                  data = fruit_data_wide)
AIC(m_9)
BIC(m_9)

m9_resid <- resid(m_9)
hist(m9_resid)

m9_fit <- fitted(m_9)
hist(m9_fit)


plot(m9_fit, m9_resid,
     xlab = "Predicted values",
     ylab = "Residual values",
     main = "Predicted vs Residuals (model: m_9)") #fitted values vs residuals

##October
m_10 <- lme4::lmer(log_rot ~ temp_10 + precip_10 + (1 | bog),
                   data = fruit_data_wide)

AIC(m_10)
BIC(m_10)

m10_resid <- resid(m_10)
hist(m10_resid)

m10_fit <- fitted(m_10)
hist(m10_fit)


plot(m10_fit, m10_resid,
     xlab = "Predicted values",
     ylab = "Residual values",
     main = "Predicted vs Residuals (model: m_10)") #fitted values vs residuals


##November
m_11 <- lme4::lmer(log_rot ~ temp_11 + precip_11 + (1 | bog),
                   data = fruit_data_wide)

AIC(m_11)
BIC(m_11)

m11_resid <- resid(m_11)
hist(m11_resid)

m11_fit <- fitted(m_11) 
hist(m11_fit)

plot(m11_fit, m11_resid,
     xlab = "Predicted values",
     ylab = "Residual values",
     main = "Predicted vs Residuals (model: m_11)") #fitted values vs residuals)

#summaries

summary(m_9)
summary(m_10)
summary(m_11)





# nlme_coef <- read_csv(here("data", "kqf_nlme_coef - Sheet1.csv"))
# nlme_coef$temp <- abs(as.numeric(nlme_coef$temp))
# nlme_coef$precip <- abs(as.numeric(nlme_coef$precip))



#Visualizations----
#What is the value of the final KQF value as a general predictor of harvest quality (rot %, color, weight, etc.)?


#rot_pct vis
fruit_data_wide %>%
  filter(is.na(final_points) == F) %>%
  ggplot(aes(as.factor(final_points), rot_pct)) +
  geom_point() +
  geom_jitter() 

#log_rot vis
logrot_vs_kqf <- fruit_data_wide %>%
  filter(is.na(final_points) == F) %>%
  ggplot(aes(as.factor(final_points), log_rot)) +
  geom_jitter() +
  geom_boxplot() 


fruit_data_wide %>% 
  ggplot(aes(log_rot)) +
  geom_histogram(bins = 10) +
  facet_wrap(~final_points)

#color vis
fruit_data_wide %>%
  filter(is.na(final_points) == F) %>%
  droplevels(.$variety) %>%
  ggplot(aes(factor(variety), log_rot)) +
  geom_boxplot(aes(color = variety))  




#pairs plot numeric vis
kqf_int <- fruit_data_combined %>%
  select(
    log_rot,
    color,
    avg_temp_f,
    tot_precip,
    final_points)

kqf_pairs <- ggpairs(kqf_int)



t_val <- function(x, y = 30) {
  (1 - pt(abs(x), df = y))
} 

#linear models----

fruit_data_wide %>%
  group_by(bog, grower) %>%
  nest()

grow_nest <- fruit_data_wide %>%
  nest(-grower)
