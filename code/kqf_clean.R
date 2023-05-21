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
  relocate(date)
#Decas pest data 2011-2018----

##application_date (char to POSIXct)
pest_decas$date <- parse_date_time(pest_decas$application_date, "mdy") 

pest_decas <- pest_decas %>%
  #remove old application_date and extraneous columns
  select(date,
         grower,
         bog,
         treatment,
         active_ingredient) %>%
  #relocate new date column
  relocate(date) %>%
  #create data_source column
  mutate(data_source = "decas")

#CIG pest data 2012-2021----

pest_cig <- pest_cig %>%
  ##application_date (char to POSIXct)
  mutate(date = parse_date_time(application_date, orders = "ymd")) %>%
  #remove old application_date and select non-extraneous columns
  select(date,
         grower,
         Bog,
         treatment,
         active_ingredient) %>%
  #relocate new date column
  relocate(date) %>%
  #create data_source col
  mutate(data_source = 'cig') %>%
  rename("bog" = "Bog")

#temperatures 2003-2021----


temp_precip <- temp_precip %>%
  mutate(month = lubridate::month(date),
         year = lubridate::year(date)) %>%
  #filter for years of interest
  filter(year %in% c(2012:2018)) 

temp_precip$month_2 <- temp_precip$month
temp_precip$month_0 <- temp_precip$month

temp_precip_wide <- temp_precip %>%
  #temp pivot
  pivot_wider(names_from =  month,
              names_prefix = "temp_",
              values_from = avg_temp_f) %>%
  #precipitation pivot
  pivot_wider(names_from = month_2,
              names_prefix = "precip_",
              values_from = tot_precip) 

#kqf point values----
points <- points %>%
  mutate(year = as.numeric(date_year)) %>%
  select(-date_year)

#Pest data combine/clean----

##define join columns
comb_pest_cols <- c("date", 
                    "treatment", 
                    "active_ingredient", 
                    "grower", 
                    "bog", 
                    "data_source")

##full join on defined columns
pest_comb <- pest_decas %>%
  full_join(pest_cig, by = comb_pest_cols, suffix = c("_decas", "_cig")) %>%
  mutate(month = lubridate::month(date),
         year = lubridate::year(date))

#redefine and collapse repeated fungicides
pest_comb$active_ingredient <-
  str_replace_all(
    pest_comb$active_ingredient,
    c(
      "Manganese ethylenebisdithiocarbamate" = "EBDC",
      "Mancozeb" = "EBDC",
      "EDBC" = "EBDC",
      "Polyoxin D Zinc Salt" = "Polyoxin D",
      "Polyoxin D zinc salt" = "Polyoxin D"
    )
  )

#define alternative fungicides
other_ingredients <- c("Cuprous Oxide",
                       "Fosetyl-Al",
                       "Polyoxin D")

fungicide_use <- pest_comb %>%
  #filter out non-fungicide treatments
  filter(
    active_ingredient %in% c(
      "Copper Hydroxide",
      "Cuprous Oxide",
      "Polyoxin D",
      "Chlorothalonil",
      "Azoxystrobin",
      "Fenbuconazole",
      "Fosetyl-Al",
      "EBDC",
      "Ferbam"
    )
  ) %>%
  #collapse fungicides w/ <50 instances and rm original column
  mutate(ingredient = fct_collapse(active_ingredient,
                                   other = other_ingredients)) %>%
  select(-active_ingredient)

pest_data_combined <- fungicide_use %>%
  full_join(temp_precip, by = c("year", "month" = "month_0"), suffix = c("_fungi", "_temps")) %>%
  select(-date_temps) %>%
  rename("date" = "date_fungi") %>%
  full_join(points, by = c("year")) 

#fruit data combine----
fruit_data_norm <- fruit_decas %>% 
full_join(temp_precip, by = c("year", "month" = "month_0"), suffix = c("_fruit", "_temps")) %>% #join temp_precip on year and month
  rename("date" = "date_fruit") %>% #rename date column
  full_join(points, by = c("year")) %>% #join points on year col
  select(-debris, -color, -month_temps, -month_2, -date_temps) %>% #remove extraneous columns
  unique() #remove duplicates

fruit_data_wide <- fruit_decas %>%
  full_join(temp_precip_wide, by = c("year", "month" = "month_0")) %>%
  rename("date" = "date.x") %>%
  #join points on year col
  full_join(points, by = c("year")) %>%
  #Remove duplicates fruit data
  unique() %>%
  select(-debris, -color, -date.y) 

#Fruit data pivot----

# fruit_data_unique$month_2 <- fruit_data_unique$month
# fruit_data_wide <- fruit_data_unique %>%
#   filter(is.na(date) != T,
#          year %in% c(2013,2014,2015,2016,2017,2018))

# fruit_data_wide <- fruit_data_unique %>%
#   #filter NA/month/year
#   filter(is.na(date) != T,
#          month %in% c(3,4,5,6,7,8,9,10,11,12),
#          year %in% c(2013,2014,2015,2016,2017,2018)) %>%
#   #temperature pivot
#   pivot_wider(names_from =  month,
#             names_prefix = "temp_",
#             values_from = avg_temp_f) %>%
#   #precipitation pivot
#   pivot_wider(names_from = month_2,
#               names_prefix = "precip_",
#               values_from = tot_precip) 
# # %>%
# #   #select for desired fields
# #   select(date, year,
# #          grower, bog, variety,
# #          color, rot_pct, log_rot,
# #          pre_points, final_points,
# #          temp_9, temp_10, temp_11, temp_12,
# #          precip_9, precip_10, precip_11, precip_12)
# #KQF factorization
# fruit_data_wide$pre_points <- as.factor(fruit_data_wide$pre_points)
# fruit_data_wide$final_points <- as.factor(fruit_data_wide$final_points)


#Extraneous object removal from environment----

rm(pest_cig, pest_decas)

rm(pest_comb,
   fungicide_use,
   comb_pest_cols)

rm(other_ingredients,
   other_varieties)

rm(lw_ctrl_cig)

 rm(fruit_cig)

 rm(points)
