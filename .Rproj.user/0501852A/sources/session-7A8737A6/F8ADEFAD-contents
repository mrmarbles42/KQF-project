require(tidyverse)
library(data.table)
library(lubridate)
require(visdat)
require(here)
require(forcats)

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
  filter(is.na(rot) == F) %>%
  ##create log_rot / year / month columns and collapse varieties by frequency
  mutate(
    log_rot = if_else(rot == 0, 0, log10(rot + 0.01)) ,
    variety = fct_collapse(variety, other = other_varieties),
    year = lubridate::year(date),
    month = lubridate::month(date),
  ##create data_source column
    data_source = as.factor("decas")
  ) %>%
  ##relocate date column to [,1]
  relocate(date) %>%
  ##rename columns
  rename("grower" = "grower_id",
         "rot_pct" = "rot")

##factor kqf values
fruit_decas$kqf_final <- as.factor(fruit_decas$kqf_final)
fruit_decas$kqf_pre <- as.factor(fruit_decas$kqf_pre)

#CIG fruit data 2021----

fruit_cig <- fruit_cig %>%
  ##exclude rows w/ NA values in rot
  filter(is.na(rot_pct) == F) %>%
  ##create log_rot, variety, year, and data source columns
  mutate(
    log_rot = if_else(rot_pct == 0, 0, log10(rot_pct + 0.01)),
    variety = as_factor("ST"),
    year = 2021,
    month = 10,
    data_source = "cig"
  ) %>%
  rename("yield_acre_barrel" = "yield_per_ac_barrel",
         "yield_ha_barrel" = "yield_p_ha_barrel",
         "yield_ac_kg" = "yield_p_ac_kg",
         "yield_ac_lb" = "yield_p_ac_lb",
         "site" = "Site",
         "rep" = "Rep")


lw_ctrl_cig <- lw_ctrl_cig %>%
  #rename cols based on cig_fruit names
  rename("yield_acre_barrel" = "yield per acre",
         "rot_pct" = "pct_rot",
         "wt_p_berry" = "wt_per_berry") %>%
  #create yield, data_source, site, variety, kqf, and log(rot) columns
  mutate(yield_ha_barrel = yield_acre_barrel * 2.471,
         yield_ac_kg = yield_acre_barrel * 45.35929257,
         yield_ac_lb = yield_ac_kg * 2.20463,
         data_source = "cig_lw",
         site = 0,
         log_rot = if_else(rot_pct == 0, 0, log10(rot_pct + 0.01)),
         variety = "ST",
         kqf_final = 4,
         kqf_pre = 3)

# cig_fruit_cols <- c("site", "rep", "wt_p_berry", "yield_acre_barrel", "yield_ha_barrel",
#                     "rot_pct", "yield_ac_kg",  "yield_ac_lb", "tacy", "absorbance", "log_rot", 
#                     "variety", "year", "data_source")

##combine latewater controls and 24-site cig data
fruit_cig <- fruit_cig %>%
  bind_rows(lw_ctrl_cig)

#factor kqf values
fruit_cig$kqf_final <- as.factor(fruit_cig$kqf_final)
fruit_cig$kqf_pre <- as.factor(fruit_cig$kqf_pre)

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

#climate averages 2003-2021----

temp_precip <- temp_precip %>%
  mutate(month = lubridate::month(date),
         year = lubridate::year(date))

##change avg_date to numeric
#temp_precip$avg_temp <- as.numeric(temp_precip$avg_temp)

##change month and year to factors ordered by month name 
#temp_precip$month <- factor(temp_precip$month, levels = month.name)


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

pest_temp_combined <- fungicide_use %>%
  full_join(temp_precip, by = c("year", "month"), suffix = c("_fungi", "_temps")) %>%
  select(-date_temps) %>%
  rename("date" = "date_fungi")

#fruit data combine----

fruit_comb <- bind_rows(fruit_cig, fruit_decas)

fruit_temp_combined <- fruit_comb %>%
  full_join(temp_precip, by = c("year", "month")) %>%
  select(-date.y) %>%
  rename("date" = "date.x")

#full data combine----

##define joining columns for combined data
comb_cols <- c("data_source", "date", "month", "year", "avg_temp_f", "tot_precip", "grower", "bog", "treatment")

#Join all columns on joining vec
kqf_data_combined <- fruit_temp_combined %>%
  full_join(pest_temp_combined, 
            by = comb_cols,
            suffix = c("_fruit", "_pest"))

##factor coercion
kqf_data_combined$variety <- as_factor(kqf_data_combined$variety)
kqf_data_combined$site <- as_factor(kqf_data_combined$site)
kqf_data_combined$rep <- as_factor(kqf_data_combined$rep)
kqf_data_combined$data_source <- as_factor(kqf_data_combined$data_source)
kqf_data_combined$treatment <- as_factor(kqf_data_combined$treatment)
kqf_data_combined$ingredient <- as_factor(kqf_data_combined$ingredient)

#Extraneous object removal from environment----

rm(pest_cig,
   pest_decas,
   pest_comb,
   fungicide_use,
   comb_pest_cols)

rm(other_ingredients,
   other_varieties,
   comb_cols,
   temp_precip)

rm(fruit_cig,
   fruit_decas,
   fruit_comb,
   lw_ctrl_cig)

rm(fruit_temp_combined,
   pest_temp_combined)

fwrite(kqf_data_combined, "kqf_data_combined.csv")
