# https://www.cde.ca.gov/ta/ac/cm/

### Load libraries

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
tidyverse
,here
,gridExtra
,janitor
)


### Define default variables

# files <- c("cci", "chronic", "ela", "elaprate", "elpi", "grad", "math", "mathprate", "susp")

files <- c("cci", "chronic", "ela",  "grad", "math", "susp", "elpi")

year <- "2018"

county <- "Monterey"


pal <- c("0" = "white",
         "1" = "red",
         "2" = "orange",
         "3" = "yellow",
         "4" = "green",
         "5" = "blue")


ind_ord <- c("ela","math","cci","chronic","grad","susp", "elpi")
sg_ord <- c("ALL", "AA", "AI", "AS", "FI", "HI", "PI", "WH", "MR", "EL", "ELO", "EO", "SED", "SWD", "FOS", "HOM")

# Caption for later that explains the student group codes

capt <- "ALL = All Students,
AA = Black/African American,
AI = American Indian or Alaska Native,
AS = Asian,
FI = Filipino,
HI = Hispanic,
PI = Pacific Islander,
WH = White,
MR = Multiple Races/Two or More,
EL = English Learner,
ELO = English Learners Only, 
RFP = RFEPs Only,
EO = English Only,
SED = Socioeconomically Disadvantaged,
SWD = Students with Disabilities,
FOS = Foster Youth,
HOM = Homeless Youth"



###  Compile all the forms from the state into a single dataframe

 compile <- function(ind) {
         read.delim(paste0("data/",year,"/" , ind ,"download2018.txt")) %>%
#                 filter(countyname == county) %>%
                 mutate(ind = ind)
 }

 all <- data_frame()
 
 for(i in files) {
         
         print(i)
         
         temp <- compile(i)
         
         all <- bind_rows(all, temp)
         
         assign(i, temp)
 }
 
 
#  Fix factors

schools <-read.delim(here("data",  "pubschls.txt"))
narrow.schools <- schools %>% select(CDSCode,DOC:GSserved) %>%
        mutate(cds = str_pad( as.character(CDSCode), width = 14, side = "left", pad = "0"  ) )


 
all <- all %>%
        mutate(cds = str_pad( as.character(cds), width = 14, side = "left", pad = "0"  ) ) %>%
        mutate(ind = factor(ind, levels = ind_ord),
               studentgroup = factor(studentgroup, levels = sg_ord),
               color.factor = factor(color)
               ) %>%
        mutate(districtname = str_replace_all(districtname, "County Office of Education", "COE"),
               districtname = str_replace_all(districtname, "County Department of Education", "CDE"),
               districtname = str_replace_all(districtname, "County Superintendent of Schools", "CSOS"),
               districtname = str_replace_all(districtname, "Union", "U"),
               districtname = str_replace_all(districtname, "Unified", "U"),
               districtname = str_replace_all(districtname, "Elementary", "E"),
               districtname = str_replace_all(districtname, "County", "C")
               ) %>%
        left_join(narrow.schools ) %>%
        mutate(type = as.character(type)) %>%
        mutate(type = if_else(!is.na(type), type, case_when(EILCode == "ELEM" ~ "ES",
                                                            EILCode == "HS" ~ "HS" ,
                                                            EILCode == "INTMIDJR" ~ "MS", 
                                                            EILCode == "No Data" & str_detect(DOCType, "Elementary") ~ "ED",
                                                            EILCode == "No Data" & str_detect(DOCType, "High") ~ "HD", 
                                                            EILCode == "No Data" & str_detect(DOCType, "Unified") ~ "UD")   )) %>%
        mutate(cutoff = case_when(ind == "chronic" & statuslevel == 4 ~ 2.5, 
                                  ind == "chronic" & statuslevel == 3 ~ 5.0,
                                  ind == "chronic" & statuslevel == 2 ~ 10.0,
                                  ind == "chronic" & statuslevel == 1 ~ 20.0,
                                  
                                  ind == "cci" & statuslevel == 1 ~ 10.0,
                                  ind == "cci" & statuslevel == 2 ~ 35.0,
                                  ind == "cci" & statuslevel == 3 ~ 55.0,
                                  ind == "cci" & statuslevel == 4 ~ 70.0,
                                  
                                  ind == "grad" & statuslevel == 1 ~ 67.0,
                                  ind == "grad" & statuslevel == 2 ~ 80,
                                  ind == "grad" & statuslevel == 3 ~ 90,
                                  ind == "grad" & statuslevel == 4 ~ 95,                                  

                                  ind == "ela" & statuslevel == 1 & type %in% c("ES", "ED", "MS", "UD") ~ -70,
                                  ind == "ela" & statuslevel == 2 & type %in% c("ES", "ED", "MS", "UD") ~ -5,
                                  ind == "ela" & statuslevel == 3 & type %in% c("ES", "ED", "MS", "UD") ~ 10,
                                  ind == "ela" & statuslevel == 4 & type %in% c("ES", "ED", "MS", "UD") ~ 45,                                  

                                  ind == "ela" & statuslevel == 1 & type %in% c("HS", "HD") ~ -45,
                                  ind == "ela" & statuslevel == 2 & type %in% c("HS", "HD") ~ 0,
                                  ind == "ela" & statuslevel == 3 & type %in% c("HS", "HD") ~ 30,
                                  ind == "ela" & statuslevel == 4 & type %in% c("HS", "HD") ~ 75,                                  
                                  
                                  ind == "math" & statuslevel == 1 & type %in% c("ES", "ED", "MS", "UD") ~ -95,
                                  ind == "math" & statuslevel == 2 & type %in% c("ES", "ED", "MS", "UD") ~ -25,
                                  ind == "math" & statuslevel == 3 & type %in% c("ES", "ED", "MS", "UD") ~ 0,
                                  ind == "math" & statuslevel == 4 & type %in% c("ES", "ED", "MS", "UD") ~ 35,                                  
                                  
                                  ind == "math" & statuslevel == 1 & type %in% c("HS", "HD") ~ -115,
                                  ind == "math" & statuslevel == 2 & type %in% c("HS", "HD") ~ -60,
                                  ind == "math" & statuslevel == 3 & type %in% c("HS", "HD") ~ 0,
                                  ind == "math" & statuslevel == 4 & type %in% c("HS", "HD") ~ 25,   
                                  
                                  ind == "susp" & statuslevel == 1 & type == "ED" ~ 0.5,
                                  ind == "susp" & statuslevel == 2 & type == "ED" ~ 1.5,
                                  ind == "susp" & statuslevel == 3 & type == "ED" ~ 3.0,
                                  ind == "susp" & statuslevel == 4 & type == "ED" ~ 6.0,
                                  
                                  ind == "susp" & statuslevel == 1 & type == "HD" ~ 1.5,
                                  ind == "susp" & statuslevel == 2 & type == "HD" ~ 3.5,
                                  ind == "susp" & statuslevel == 3 & type == "HD" ~ 6.0,
                                  ind == "susp" & statuslevel == 4 & type == "HD" ~ 9.0,
                                  
                                  ind == "susp" & statuslevel == 1 & type == "UD" ~ 1.0,
                                  ind == "susp" & statuslevel == 2 & type == "UD" ~ 2.5,
                                  ind == "susp" & statuslevel == 3 & type == "UD" ~ 4.5,
                                  ind == "susp" & statuslevel == 4 & type == "UD" ~ 8.0,
                                  
                                  
                                  ind == "susp" & statuslevel == 1 & type == "ES" ~ 0.5,
                                  ind == "susp" & statuslevel == 2 & type == "ES" ~ 1.0,
                                  ind == "susp" & statuslevel == 3 & type == "ES" ~ 3.0,
                                  ind == "susp" & statuslevel == 4 & type == "ES" ~ 6.0,
                                  
                                  ind == "susp" & statuslevel == 1 & type == "MS" ~ 0.5,
                                  ind == "susp" & statuslevel == 2 & type == "MS" ~ 2.0,
                                  ind == "susp" & statuslevel == 3 & type == "MS" ~ 8.0,
                                  ind == "susp" & statuslevel == 4 & type == "MS" ~ 12.0,
                                  
                                  ind == "susp" & statuslevel == 1 & type == "HS" ~ 0.5,
                                  ind == "susp" & statuslevel == 2 & type == "HS" ~ 1.5,
                                  ind == "susp" & statuslevel == 3 & type == "HS" ~ 6.0,
                                  ind == "susp" & statuslevel == 4 & type == "HS" ~ 10.0,
                                  TRUE ~ NA_real_),
               cutoffdiff = cutoff - currstatus
        ) 


saveRDS(all, here("data" ,"01_all-school-dashboard-2018.rds"))


