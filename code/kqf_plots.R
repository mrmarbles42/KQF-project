require(here)

source(here("code", "kqf_clean.R"))
#source(here("code", "kqf_analysis.R"))

#rot density plots

kqf_data_combined %>%
  filter(rot_pct != 0 & data_source == "decas") %>%
  ggplot(aes(log_rot)) +
  geom_freqpoly()

kqf_data_combined %>%
  filter(rot_pct != 0) %>%
  ggplot(aes(log_rot, fill = )) +
  geom_density() +
  facet_grid(~data_source) +
  labs(title = "Rot percentage relative density by data source")


#----

kqf_data_combined %>%
  filter(log_rot > 0 & year != 2022 & is.na(final_points) != T) %>%
  ggplot(aes(log_rot)) +
  geom_histogram(bins = 50) +
  facet_grid(~final_points) +
  labs(title = "Observed rot percentage (logarithmic) by kqf value")+
  xlab("log(rot)%")

plot(kqf_data_combined$log_rot ~ kqf_data_combined$final_points)

boxplot(kqf_data_combined$log_rot ~ kqf_data_combined$final_points,
        names = c(2, 3, 4, 7),
        xlab = "KQF final points",
        ylab = "log(rot) %",
        main = "KQF points vs log(rot) % (2013-2018)")


tacy_rot <- lm(kqf_data_combined$tacy ~ kqf_data_combined$log_rot)
plot(kqf_data_combined$tacy ~ kqf_data_combined$log_rot,
     xlab = "log(rot)(%)",
     ylab = "Total anthocyanin content (%)",
     main = "Rot (%) vs Total Anthocyanin content (%)")
abline(32.81, -7.51)

wt_rot <- lm(kqf_data_combined$wt_p_berry ~ kqf_data_combined$log_rot)
plot(kqf_data_combined$wt_p_berry ~ kqf_data_combined$log_rot,
     xlab = "log(rot)(%)",
     main = "Rot (%) vs weight per berry (g)")
abline(1.7091, 0.0159)


kqf_data_combined %>%
  ggplot(aes(log_rot, tacy, alpha = absorbance)) +
  geom_point() +
  labs(title = "Total anthocyanin content ~ log(rot)% + absorbance \n (Stevens variety)") +
  geom_abline(aes(intercept = 32.81,slope = -7.51))

kqf_data_combined %>%
  ggplot(aes(log_rot, wt_p_berry)) +
  geom_point()+
  geom_smooth()

kqf_data_combined %>%
  ggplot(aes(factor(final_points), log_rot)) +
  geom_point() 

kqf_data_combined %>%
  ggplot(aes())

#----
#
kqf_data_combined %>%
  filter(year %in% 2013:2018) %>%
  group_by((year)) %>%
  ggplot(aes(factor(month), avg_temp_f, color = avg_temp_f)) +
  geom_point() +
  facet_wrap(~year)

#rot by kqf
kqf_data_combined %>%
  group_by(final_points) %>%
  mutate(mean_rot = mean(rot_pct)) %>%
  ggplot(aes(factor(final_points), rot_pct)) +
  geom_point()
