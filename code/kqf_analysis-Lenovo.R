require(here)

source(here("code", "kqf_clean.R"))



# #what are average rot percentages by kqf level?
# rot_by_kqf <- kqf_data_combined %>%
#   group_by(final_points) %>%
#   summarize(mean = mean(rot_pct, na.rm = T))

# 
# #average rot percentage by variety
# rot_by_variety <- kqf_data_combined %>%
#   group_by(variety) %>%
#   summarize(mean = mean(rot_pct, na.rm = T))
# 
# 
# #average rot by data source
# rot_by_source <- kqf_data_combined %>%
#   group_by(data_source) %>%
#   summarize(mean = mean(rot_pct, na.rm = T))
# 
# #average rot by year
# rot_by_year <- kqf_data_combined %>%
#   group_by(year) %>%
#   summarise(mean = mean(rot_pct, na.rm = T))
# 
# #relationship between fungicide application counts and kqf by year
# application_by_kqf_year <- kqf_data_combined %>%
#   group_by(year) %>%
#   filter(year %in% 2013:2018,
#          ingredient %in% c("Chlorothalonil", "Azoxystrobin", "Fenbuconazole",
#                            "EBDC", "Copper Hydroxide")) %>%
#   count(ingredient, sort = T) 
#   
# 
# 
# #relation between cumulative precip and kqf in march-october
# kqf_data_combined %>%
#   group_by(year) %>%
#   filter(month %in% 3:10,
#          year %in% 2013:2018) %>%
#   summarize(cum_precip = sum(tot_precip),
#             final_points = factor(unique(final_points)))
#   
# 

#What is the value of the final KQF value as a general predictor of harvest quality (rot %, color, weight, etc.)?


#rot_pct vis
kqf_data_combined %>%
  filter(is.na(final_points) == F) %>%
  ggplot(aes(as.factor(final_points), rot_pct)) +
  geom_point() +
  geom_jitter() 

#log_rot vis
logrot_vs_kqf <- kqf_data_combined %>%
  filter(is.na(final_points) == F) %>%
  ggplot(aes(as.factor(final_points), log_rot)) +
  geom_jitter() +
  geom_point() 


kqf_data_combined %>% 
  group_by(final_points) %>%
  filter(is.na(log_rot) == F) %>%
  ggplot(aes(log_rot)) +
  geom_histogram(bins = 10) +
  facet_wrap(~final_points)

#color vis
kqf_data_combined %>%
  filter(is.na(final_points) == F) %>%
  ggplot(aes(final_points, color)) +
  geom_point() +
  geom_jitter() 

#LogRot lm (no int)
logrot_lm <- lm(kqf_data_combined$log_rot ~ kqf_data_combined$final_points)
#color lm (no int)
color_lm <- lm(kqf_data_combined$color ~ kqf_data_combined$final_points)

summary(logrot_lm)
summary(color_lm)


boxplot(kqf_data_combined$rot_pct ~ kqf_data_combined$final_points)
boxplot(kqf_data_combined$color ~ kqf_data_combined$final_points)
#mean rot values by kqf category
rotsum <- kqf_data_combined %>%
  group_by(final_points) %>%
  filter(is.na(log_rot) == F) %>%
  summarize(mean_by_kqf = mean(log_rot))

#pairs plot numeric vis
kqf_int <- kqf_data_combined %>%
  select(tacy,
         absorbance,
         log_rot,
         debris,
         color,
         avg_temp_f,
         tot_precip,
         final_points)

kqf_pairs <- pairs(kqf_int)
 

#Which KQF factor is a more accurate predictor of keeping quality; total precipitation, sunshine hours, or mean temperature?

lm(final_points ~ tot_precip, data = kqf_data_combined)

lm(final_points ~ (avg_temp_f), data = kqf_data_combined)


mix_fact_lm <-lm(final_points ~ avg_temp_f + tot_precip, data = kqf_data_combined)


