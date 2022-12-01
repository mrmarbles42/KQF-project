require(here)

source(here("code", "kqf_clean.R"))
source(here("code", "kqf_analysis.R"))

#rot density plots

kqf_data_combined %>%
  filter(rot_pct != 0 & data_source == "decas") %>%
  ggplot(aes(log_rot)) +
  geom_density()

kqf_data_combined %>%
  filter(rot_pct != 0 & data_source == "cig") %>%
  ggplot(aes(log_rot)) +
  geom_density()


#----

kqf_data_combined %>%
  select(rot_pct, log_rot, variety, year, month, data_source, kqf_final, date, avg_temp_f, tot_precip) %>%
  filter(is.na(data_source) != T) %>%
  ggplot(aes(year, log_rot)) + 
  geom_point() +
  facet_grid(~(variety))


kqf_data_combined %>%
  filter(is.na(rot_pct) != T & log_rot >=0) %>%
  ggplot(aes(reorder(variety), log_rot)) +
  geom_boxplot(outlier.shape = NA) +
  facet_grid()

kqf_data_combined %>%
  filter(is.na(ingredient) != T) %>%
  ggplot(aes(reorder(ingredient,sort((ingredient))))) +
  geom_bar()

kqf_data_combined %>%
  ggplot(aes(year, tot_precip)) +
  geom_smooth()
