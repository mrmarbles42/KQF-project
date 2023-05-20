
library(here)
library(lme4)
library(GGally)
library(broom)
library(psych)
source(here("code", "kqf_clean.R"))





#NLME----
m_9 <- lme4::lmer(log_rot ~ temp_9 + precip_9 + (1 | bog),
                  data = fruit_data_wide)

m_10 <- lme4::lmer(log_rot ~ temp_10 + precip_10 + (1 | bog),
                   data = fruit_data_wide)

m_11 <- lme4::lmer(log_rot ~ temp_11 + precip_11 + (1 | bog),
                   data = fruit_data_wide)

m_12 <- lme4::lmer(log_rot ~ temp_12 + precip_12 + (1 | bog),
                   data = fruit_data_wide)

lme4::lmer(log_rot ~ temp_3 + precip_3 + (1 | bog),

                      data = fruit_data_wide)
fruit_data_wide %>%
  group_by(bog)
  nest()

#summaries----
  obj <- describe(fruit_data_wide)

  
fruit_data_wide %>%
  select(rot_pct, log_rot, debris, color, final_points) %>%
  describeBy(., omit = T, group=fruit_data_wide$final_points)

  vars <- c()
tbl_sum <- fruit_data_wide %>%
  group_by(grower,month) %>%
    select(rot_pct, final_points)

fruit_data_wide %>%
  filter(is.na(rot_pct) == F) %>%
  select(rot_pct, log_rot, color, variety) %>%
  summary(.)

summary(m_9)
summary(m_10)
summary(m_11)
m_11_sum <- summary(m_11)

#pt_10_variety 
ab2 <- lme4::lmer(log_rot ~ temp_1 + precip_1 + variety +(1 | bog),
           data = fruit_data_wide)


grow_nest <- fruit_data_wide %>%
  nest(-grower)

nlme_coef <- read_csv(here("data", "kqf_nlme_coef - Sheet1.csv"))
nlme_coef$temp <- abs(as.numeric(nlme_coef$temp))
nlme_coef$precip <- abs(as.numeric(nlme_coef$precip))
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
# 
# #relation between cumulative precip and kqf in march-october
# fruit_data_combined %>%
#   group_by(year) %>%
#   filter(month %in% 3:10,
#          year %in% 2013:2018) %>%
#   summarize(cum_precip = sum(tot_precip),
#             final_points = factor(unique(final_points)))
#   
# 

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
fruit_data_combined %>%
  filter(is.na(final_points) == F) %>%
  ggplot(aes(variety, log_rot)) +
  geom_boxplot(aes(color = variety))  

#LogRot lm (no int)
lm(fruit_data_combined$log_rot ~ fruit_data_combined$final_points + 0)
#color lm (no int)
lm(fruit_data_combined$color ~ fruit_data_combined$final_points + 0)

summary(rot_kqf_lm)


#pairs plot numeric vis
kqf_int <- fruit_data_combined %>%
  select(
         log_rot,
         color,
         avg_temp_f,
         tot_precip,
         final_points)

kqf_pairs <- ggpairs(kqf_int)
 



july <- subset(rot_kqf_vals, month = 7)

obj2 <- lmer(rot_pct ~ avg_temp_f + tot_precip + (1| final_points),fruit_data_combined)
#Which KQF factor is a more accurate predictor of keeping quality; total precipitation, sunshine hours, or mean temperature?
  
summary(obj2)



1 - pt(abs(-7), df = 30)


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
