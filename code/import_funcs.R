library(data.table)
require(readxl)
require(here)


import_fruit_csv <- function(x) {
  if (getwd() == "C:/Users/user/OneDrive - University of Massachusetts/kqf_project") {
    fread(here("data", x), select = c("grower_id", 
                                      "deliver_date", 
                                      "Bog", 
                                      "Variety",
                                      "Rot",
                                      "GTL",
                                      "NetWeight",
                                      "kqf_pre",
                                      "kqf_final"))
    
  } else {
    setwd("C:/Users/user/OneDrive - University of Massachusetts/kqf_project")
    fread(here("data", x), select = c("grower_id", 
                                      "deliver_date", 
                                      "Bog", 
                                      "Variety",
                                      "Rot",
                                      "GTL",
                                      "NetWeight",
                                      "kqf_pre",
                                      "kqf_final"))
    
  }
}

import_xl <-  function(x) {
  if (getwd() == "C:/Users/user/OneDrive - University of Massachusetts/kqf_project") {
    read_excel(here("data", x))
    
  } else {
    setwd("C:/Users/user/OneDrive - University of Massachusetts/kqf_project")
    read_excel(here("data", x))
    
    
    
  }
}


import_pest_csv <- function(x) {
  if (getwd() == "C:/Users/user/OneDrive - University of Massachusetts/kqf_project") {
    fread(here("data", x))
    
  } else {
    setwd("C:/Users/user/OneDrive - University of Massachusetts/kqf_project")
    fread(here("data", x))
    
  }
}