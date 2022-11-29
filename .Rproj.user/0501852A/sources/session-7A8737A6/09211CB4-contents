require(here)

source(here("code", "kqf_clean.R"))
source(here("code", "kqf_analysis.R"))

#rot density plots

decas_fruit %>%
  filter(rot != 0) %>%
  ggplot(aes(log_rot)) +
  geom_density()

cig_fruit %>%
  filter(rot_pct != 0) %>%
  ggplot(aes(log_rot)) +
  geom_density()


#----

fruit_temp_combined %>%
  select(rot_pct, log_rot, variety, year, month, data_source, kqf_final, date, avg_temp_f, tot_precip) %>%
  filter(is.na(data_source) != T) %>%
  ggplot(aes(year, log_rot)) + 
  geom_point() 


kqf_data_combined %>%
  filter(is.na(rot_pct) != T & log_rot >=0) %>%
  ggplot(aes(reorder(variety), log_rot)) +
  geom_boxplot(outlier.shape = NA) +
  facet_grid()
