library(here)
library(lme4)
library(GGally)
source(here("code", "kqf_clean.R"))


#NLME----

#precip_10_year
lme4::lmer(log_rot ~ precip_10 + (1 | year),
           data = fruit_data_wide)
#precip_9_year
lme4::lmer(log_rot ~ precip_9 + (1 | year),
           data = fruit_data_wide)
#pt_10_variety 
ab2 <- lme4::lmer(log_rot ~ temp_1 + precip_1 + variety +(1 | bog),
           data = fruit_data_wide)

pt_10_bog <- lme4::lmer(log_rot ~ temp_10 + precip_10 + (1 | bog),
            data = fruit_data_wide)
#pt 10 bog
lme4::lmer(rot_pct ~ temp_10 + precip_10 + (1 | bog),
           data = fruit_data_wide)

#pt_9_bog 
lme4::lmer(log_rot ~ temp_9 + precip_9 + (1 | bog),
           data = fruit_data_wide)

#pt_10_year
lme4::lmer(log_rot ~ temp_10 + precip_10 +(1 | year),
            data = fruit_data_wide)

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
