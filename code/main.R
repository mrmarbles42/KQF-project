library(tidyverse)
library(lubridate)
library(data.table)
require(here)
require(ggplot2)
require(visdat)

#Functions----
import_pest_csv <- function(x) {
  if (getwd() == "C:/Users/user/OneDrive - University of Massachusetts/kqf_project") {
    fread(here("data", x))
    
  } else {
    setwd("C:/Users/user/OneDrive - University of Massachusetts/kqf_project")
    fread(here("data", x))
    
  }
}
import_fruit_csv <- function(x) {
  if (getwd() == "C:/Users/user/OneDrive - University of Massachusetts/kqf_project") {
    fread(
      here("data", x)
      )
    
    
  } else {
    setwd("C:/Users/user/OneDrive - University of Massachusetts/kqf_project")
    fread(
      here("data", x),
      select = c(
        "grower_id",
        "deliver_date",
        "Bog",
        "Variety",
        "Rot",
        "GTL",
        "NetWeight",
        "kqf_pre",
        "kqf_final"
      )
    )
    
  }
}
#Decas fruit data import/ DT creation----
#init import
fruit_12 <- import_fruit_csv("grower_yield_rpt_2012.csv")
fruit_13 <- import_fruit_csv("grower_yield_rpt_2013.csv")
fruit_14 <- import_fruit_csv("grower_yield_rpt_2014.csv")
fruit_15 <- import_fruit_csv("grower_yield_rpt_2015.csv")
fruit_16 <- import_fruit_csv("grower_yield_rpt_2016.csv")
fruit_17 <- import_fruit_csv("grower_yield_rpt_2017.csv")
fruit_18 <- import_fruit_csv("grower_yield_rpt_2018.csv")

#date coercion
fruit_12$date <-
  as.Date(parse_date_time(fruit_12$Delivered, "mdy"))
fruit_13$date <-
  as.Date(parse_date_time(fruit_13$Delivered, "mdy"))
fruit_14$date <-
  as.Date(parse_date_time(fruit_14$Delivered, "mdy"))
fruit_15$date <-
  as.Date(parse_date_time(fruit_15$Delivered, "mdy"))
fruit_16$date <-
  as.Date(parse_date_time(fruit_16$Delivered, "mdy"))
fruit_17$date <-
  as.Date(parse_date_time(fruit_17$Delivered, "mdy"))
fruit_18$date <-
  as.Date(parse_date_time(fruit_18$Delivered, "mdy"))

#table bind by row
decas_fruit_data <-
  rbind(fruit_12,
        fruit_13,
        fruit_14,
        fruit_15,
        fruit_16,
        fruit_17,
        fruit_18,
        fill = T)

rm(fruit_12,
   fruit_13,
   fruit_14,
   fruit_15,
   fruit_16,
   fruit_17,
   fruit_18)


decas_fruit_data <- decas_fruit_data %>%
  relocate(deliver_date)

decas_fruit_data$date <-
  (parse_date_time(decas_fruit_data$deliver_date, orders = "mdy"))

#cig yield data import----

cig_fruit_data <- fread(here("data", "cig_yield_bloom.csv"))

#Decas pest data import----

pest_decas <- import_pest_csv("pest_use_decas.csv")
pest_decas <- pest_decas %>%
  select(application_date, treatment, active_ingredient) %>%
  mutate(date =  parse_date_time(application_date, orders = "mdy"))

pest_decas$application_date <- NULL
pest_decas <- pest_decas %>%
  relocate(date)

#cig pest data import----

pest_cig <- import_pest_csv("pest_use_cig.csv")
pest_cig <- pest_cig %>%
  select(application_date, treatment, active_ingredient) %>%
  mutate(date =  parse_date_time(application_date, orders = "ymd"))

pest_cig$application_date <- NULL
pest_cig <- pest_cig %>%
  relocate(date)

#climate parameter import----
temp_precip_monthly <- fread(here("data", "kqf_temp_precip_values.csv"))
#combined pest data-----
pest_data <- rbind(pest_cig,
                   pest_decas,
                   fill = T)
rm(pest_cig, pest_decas)

pest_data$active_ingredient <-
  str_replace_all(
    pest_data$active_ingredient,
    c(
      "Manganese ethylenebisdithiocarbamate" = "EBDC",
      "Mancozeb" = "EBDC",
      "EDBC" = "EBDC",
      "Polyoxin D Zinc Salt" = "Polyoxin D",
      "Polyoxin D zinc salt" = "Polyoxin D"
    )
  )

fungicide_use <- pest_data %>%
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
  )

#combined fruit/pest----
comb_data <-
  merge(decas_fruit_data,
        fungicide_use,
        by = "date",
        all = T)

#year and month columns
comb_data$year <- format(as.Date(comb_data$date, format="%Y-%m-%d"),"%Y")
comb_data$month <- format(as.Date(comb_data$date, format= "%Y-%m-%d"), "%m")

comb_data <- comb_data %>%
  select(date, grower_id,
         Bog, GTL,
         Rot, Color,
         Variety, kqf_pre,
         kqf_final, BogID,
         treatment,
         active_ingredient, year,
         month)



#log_rot col
comb_data <- comb_data %>%
  mutate(log_rot = log(Rot)) 

#KQF points----
points <- fread(here("data", "kqf_points.csv"))
points <- na.omit(points)

long_points <- pivot_longer(points,
                            cols = c("pre_points", "final_points"),
                            names_to = 'pre/final',
                            values_to = 'points')


pre_pts <- long_points %>%
  group_by(`pre/final`) %>%
  filter(`pre/final` == 'pre_points') %>%
  mutate(points_pct = points/10)
as.data.table(pre_pts)

final_pts <- long_points %>%
  filter(`pre/final` == "final_points") %>%
  mutate(points_pct = points/16)
as.data.table(final_pts)

points_pct <- full_join(final_pts, pre_pts)

rm(pre_pts,final_pts, long_points)


#cleaning----
#pest_dupes <- pest_data %>%
  #filter(duplicated(pest_data))

#fruit_dupes <- decas_fruit_data %>%
  #filter(duplicated(decas_fruit_data))

#comb_dupes <- comb_data %>%
  #filter(duplicated(comb_data))

#fruit_unique <- distinct(decas_fruit_data)

#pest_data_unique <- distinct(pest_data)


#pest_data_unique$active_ingredient <- str_replace_all(pest_data_unique$active_ingredient, c("Manganese ethylenebisdithiocarbamate" = "EBDC",
#                                                      "Mancozeb" = "EBDC",
#                                                      "EDBC" = "EBDC",
#                                                      "Polyoxin D Zinc Salt" = "Polyoxin D",
#                                                      "Polyoxin D zinc salt" = "Polyoxin D"
#                                                      ))

#date col clean
comb_date_na<- comb_data %>%
  filter(is.na(date))

#Variety col clean

other_varieties = c("OE", "ORST", "OH", "OM",
                    "GR", "OB", "SN", "GD",
                    "", "MC", "SK", "ORHO",
                    "PM", "OS")
comb_data <- comb_data %>%
  mutate(variety_collapsed = fct_collapse(Variety, 
                                          other = other_varieties))
comb_data %>%
  count(variety_collapsed, sort = T)

#treatment col clean
comb_data %>%
  mutate(miss_treat = is.na(treatment)) %>%
  group_by(miss_treat) %>%
  arrange(miss_treat) %>%
  vis_miss()
9
#plots----

#pest data active ingredient histogram
#ac_ingredient_hist <- pest_data_unique %>%
#  filter(active_ingredient %in% c(
#    "Copper Hydroxide",
#    "Cuprous Oxide",
#    "Polyoxin D",
#    "Chlorothalonil",
#    "Azoxystrobin",
#    "Fenbuconazole",
#    "Fosetyl-Al",
#    "EBDC",
#    "Ferbam"
#  )) %>%
#  ggplot(aes(active_ingredient)) +
#  geom_histogram(stat = "count")

points_pct %>%
  ggplot(aes(fill = `pre/final`, y = points_pct, x = date_year)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Preliminary vs Final KQF points by year (normalized)",
       x = "Year",
       y = "Point scale percentage") +
  theme(axis.ticks.x = element_line())
plot(pre_pts$points_pct, final_pts$points_pct)
cor(pre_pts$points_pct , final_pts$points_pct)

###
plot(jitter(comb_data$kqf_final), log(comb_data$Rot))
abline(lm(log(comb_data$Rot + 0.01)))

comb_data %>%
  mutate(rot_log = log(Rot)) %>%
  filter(year == c(2013, 
                   2014,
                   2015,
                   2016,
                   2017,
                   2018)) %>%
  ggplot(aes(year, rot_log)) +
  geom_boxplot()

comb_data %>%
  mutate(rot_log = log(Rot)) %>%
  ggplot(aes(month, rot_log)) +
  geom_point(aes(color = year)) +
  geom_jitter()

##log rot percent by variety
comb_data %>%
  ggplot(aes(variety_collapsed, log_rot)) +
  geom_boxplot()

  