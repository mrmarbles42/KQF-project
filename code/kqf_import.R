library(tidyverse)
library(data.table)
library(lubridate)
require(here)
require(openxlsx)

#Decas fruit data  2012-2018----

##inital import/bind/export to file

fruit_12 <- fread(here("data/base_files","grower_yield_rpt_2012.csv"))
fruit_13 <- fread(here("data/base_files","grower_yield_rpt_2013.csv"))
fruit_14 <- fread(here("data/base_files","grower_yield_rpt_2014.csv"))
fruit_15 <- fread(here("data/base_files","grower_yield_rpt_2015.csv"))
fruit_16 <- fread(here("data/base_files","grower_yield_rpt_2016.csv"))
fruit_17 <- fread(here("data/base_files","grower_yield_rpt_2017.csv"))
fruit_18 <- fread(here("data/base_files","grower_yield_rpt_2018.csv"))

fruit_decas <- list(fruit_13,
                    fruit_14,
                    fruit_15,
                    fruit_16,
                    fruit_17,
                    fruit_18)
fruit_decas$GTL <- NULL
fruit_decas <-
  rbindlist(fruit_decas,
        fill = T)
# fwrite(fruit_decas, "decas_fruit_2013-2018.csv")


fruit_decas <- fruit_decas %>%
  select(grower_id,
         deliver_date,
         Bog,
         Rot,
         Debris,
         Color,
         Variety,
         kqf_pre,
         kqf_final)

colnames(fruit_decas) <- c("grower_id",
                                "date",
                                "bog",
                                "rot",
                                "debris",
                                "color",
                                "variety",
                                "kqf_pre",
                                "kqf_final")

#import decas_fruit data
#fruit_decas <- fread(here("data", "decas_fruit_2013-2018.csv"))

#CIG fruit data 2021----

##import 24-site CIG data 
fruit_cig <- fread(here("data/base_files", "cig_yield_bloom.csv"))

##import latewater control data
lw_ctrl_cig <- fread(here("data", "2021_cig_latewater_controls.csv"))

#Decas pest data 2011-2018----
pest_decas <- fread(here("data/base_files","pest_use_decas.csv"))

#CIG pest data----

pest_cig <- fread(here("data/base_files", "pest_use_cig.csv"))

#climate parameters 2003-2020----
temp_precip <- fread(here("data/base_files", "kqf_temp_precip.csv"))




rm(fruit_12,
   fruit_13,
   fruit_14,
   fruit_15,
   fruit_16,
   fruit_17,
   fruit_18)

