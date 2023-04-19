library(tidyverse)
library(data.table)
library(lubridate)
library(visdat)
library(here)
library(forcats)

source(here("code", "kqf_import.R"))


#Decas fruit data 2013-2018----

##date manipulation
fruit_decas$date <- parse_date_time(fruit_decas$date, "mdy")

##collapsed varieties vector
other_varieties = c("OE","ORST","OH",
                    "OM","GR","OB",
                    "SN","MC","SK",
                    "ORHO","OS","MX",
                    "GD","PD")

fruit_decas <- fruit_decas %>%
  ##exclude rows w/ NA values in rot
  filter(is.na(rot_pct) == F) %>%
  ##create log_rot / year / month columns and collapse varieties by frequency
  mutate(
    log_rot = log10(rot_pct + 1) ,
    variety = fct_collapse(variety, other = other_varieties),
    year = lubridate::year(date),
    month = lubridate::month(date),
    ##create data_source column
    data_source = as.factor("decas")
  ) %>%
  ##relocate date column to [,1]
  relocate(date) %>%
  #Remove duplicates fruit data
  unique()

#temperatures 2003-2021----


temp_precip <- temp_precip %>%
  mutate(month = lubridate::month(date),
         year = lubridate::year(date)) %>%
  #filter for years of interest
  filter(year %in% c(2012:2018)) 

temp_precip$month_2 <- temp_precip$month
temp_precip$month_0 <- temp_precip$month

temp_precip <- temp_precip %>%
  #temp pivot
  pivot_wider(names_from =  month,
              names_prefix = "temp_",
              values_from = avg_temp_f) %>%
  #precipitation pivot
  pivot_wider(names_from = month_2,
              names_prefix = "precip_",
              values_from = tot_precip) 


temp_precip$month <- temp_precip$month_0

#kqf point values----
points <- points %>%
  mutate(year = as.numeric(date_year)) %>%
  select(-date_year)


#fruit data combine----

fruit_data_combined <- fruit_decas %>%
  full_join(temp_precip, by = c("year", "month")) %>%
  rename("date" = "date.x") %>%
  full_join(points, by = c("year"))


#Extraneous object removal from environment----

rm(pest_cig, pest_decas)

rm(pest_comb,
   fungicide_use,
   comb_pest_cols)

rm(other_ingredients,
   other_varieties)

rm(lw_ctrl_cig,
   fruit_cig,
   fruit_decas)

# rm(fruit_data_unique,
#    fruit_data_combined)

rm(points)