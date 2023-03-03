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
kqf_data_combined %>%
  filter(is.na(final_points) == F) %>%
  ggplot(aes(as.factor(final_points), log_rot)) +
  geom_jitter() +
  geom_point()

#color vis
kqf_data_combined %>%
  filter(is.na(final_points) == F) %>%
  ggplot(aes(final_points, color)) +
  geom_point() +
  geom_jitter()

rot_kqf_lm <- lm(kqf_data_combined$log_rot ~ kqf_data_combined$final_points)
color_kqf_lm <- lm(kqf_data_combined$color ~ kqf_data_combined$final_points)

summary(rot_kqf_lm)

kqf_int <- kqf_data_combined %>%
  select(tacy,
         absorbance,
         log_rot,
         debris,
         color,
         avg_temp_f,
         tot_precip)

pairs(kqf_int)

#Which KQF factor is a more accurate predictor of keeping quality; total precipitation, sunshine hours, or mean temperature?
  
