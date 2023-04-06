library(here)
library(lme4)
library(GGally)
source(here("code", "kqf_clean.R"))

#NLME

lme4::lmer(rot_pct ~ avg_temp_f + (tot_precip || year),
           data = fruit_data_combined)

lme4::lmer(avg_temp_f ~ month + tot_precip + (1| year),
           data = fruit_data_combined)

#temperature by temporal factors w/ precip g/month as RE
lme4::lmer(avg_temp_f ~ month + year + (1 | month), 
           data = fruit_data_combined)

#rot_pct by climate factors w/ precip grouped by bog as RE
lme4::lmer(rot_pct ~ avg_temp_f + tot_precip + (1 |  bog), 
           data = fruit_data_combined,
           #na.action =  na.omit(default),
)

##----
ab1 <- lmer(rot_pct ~ scale(fruit_data_combined$avg_temp_f) + scale(fruit_data_combined$tot_precip) + (1 | bog),
     data = fruit_data_combined)


lmer(log_rot ~ scale(fruit_data_combined$avg_temp_f) + scale(fruit_data_combined$tot_precip) (1 | year),
     data = fruit_data_combined)