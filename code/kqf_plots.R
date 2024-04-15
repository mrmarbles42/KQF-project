require(here)

#source(here("code", "kqf_clean.R"))
source(here("code", "kqf_analysis.R"))


# nlme_coef <- read_csv(here("data", "kqf_nlme_coef - Sheet1.csv"))
# nlme_coef$temp <- abs(as.numeric(nlme_coef$temp))

#rot density plots

fruit_data_wide %>%
  filter(rot_pct != 0 & data_source == "decas") %>%
  ggplot(aes(log_rot)) +
  geom_freqpoly()

kqf_data_combined %>%
  filter(rot_pct != 0) %>%
  ggplot(aes(log_rot, fill = )) +
  geom_density() +
  facet_grid(~data_source) +
  labs(title = "Rot percentage relative density by data source")


fruit_data_wide %>%
  filter(year %in% 2012:2018) %>%
  ggplot(aes(as.factor(year), temp_11)) +
  geom_boxplot()

#----

kqf_data_norm %>%
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
fruit_data_wide %>%
  group_by(final_points) %>%
  mutate(mean_rot = mean(rot_pct)) %>%
  ggplot(aes(factor(final_points), rot_pct)) +
  geom_point()

#mean rot by year
fruit_data_norm %>%
  group_by(year) %>%
  filter(year %in% 2013:2018) %>%
  summarize(mean_rot = mean(rot_pct, na.rm = T))%>%
  ggplot(aes(factor(year), mean_rot)) +
  geom_point() +
  geom_line()

levels <- c(2,3,4,7,NA)

fruit_data_wide %>%
  droplevels(.$final_points) %>%
  filter(final_points %in% levels) %>%
  ggplot(aes(factor(final_points), log_rot)) +
  geom_jitter() +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, color = "red") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25) +
  labs(x = "Final KQF Value",
             y = "Rot percentage %",
             title = "Rot Percentage(log-normalized) vs Final KQF Value") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("red", "blue", "green", "orange", "purple", "black")) +
  theme(legend.position = "none")



  plot(fruit_data_norm$rot_pct, fruit_data_norm$temp_9,
       ylab = "Temperature",
       xlab = "Rot percentage")
