require(here)

source(here("code", "kqf_clean.R"))



#what are average rot percentages by kqf level?
rot_by_kqf <- kqf_data_combined %>%
  group_by(final_points) %>%
  summarize(mean = mean(rot_pct, na.rm = T))

#average rot percentage by variety
rot_by_variety <- kqf_data_combined %>%
  group_by(variety) %>%
  summarize(mean = mean(rot_pct, na.rm = T))


#average rot by data source
rot_by_source <- kqf_data_combined %>%
  group_by(data_source) %>%
  summarize(mean = mean(rot_pct, na.rm = T))

#average rot by year
rot_by_year <- kqf_data_combined %>%
  group_by(year) %>%
  summarise(mean = mean(rot_pct, na.rm = T))

#relationship between fungicide application counts and kqf by year
application_by_kqf_year <- kqf_data_combined %>%
  group_by(year) %>%
  filter(year %in% 2013:2018,
         ingredient %in% c("Chlorothalonil", "Azoxystrobin", "Fenbuconazole",
                           "EBDC", "Copper Hydroxide")) %>%
  count(ingredient, sort = T) 
  

kqf_apps <- application_by_kqf_year %>%
  full_join(points, by = c("year"))
rm(kqf_apps)

#relation between cumulative precep and kqf in march-october
kqf_data_combined %>%
  group_by(year) %>%
  filter(month %in% 3:10,
         year %in% 2013:2018) %>%
  summarize(cum_precip = sum(tot_precip),
            final_points = factor(unique(final_points)))
  

