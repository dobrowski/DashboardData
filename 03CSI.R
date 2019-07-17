# https://www.cde.ca.gov/ta/ac/cm/

### Load libraries

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
        tidyverse
        ,here
        ,gridExtra
        ,janitor
        ,knitr
        ,ggthemes
        ,kableExtra
        ,xtable
)


# ### Define default variables
# 
# # files <- c("cci", "chronic", "ela", "elaprate", "elpi", "grad", "math", "mathprate", "susp")
# 
# files <- c("cci", "chronic", "ela",  "grad", "math", "susp", "elpi")
# 
# year <- "2018"
# 
# county <- "Monterey"
# 
# 
pal <- c("0" = "white",
         "1" = "red",
         "2" = "orange",
         "3" = "yellow",
         "4" = "green",
         "5" = "blue",
         "Graduation Rate Below 67%" = alpha("light grey", alpha = 0.3),
         "Lowest 5%" = alpha( "white", alpha = 0.3))
# 
# 
# ind_ord <- c("ela","math","cci","chronic","grad","susp")
# sg_ord <- c("ALL", "AA", "AI", "AS", "FI", "HI", "PI", "WH", "MR", "EL", "ELO", "EO", "SED", "SWD", "FOS", "HOM")
# 
# # Caption for later that explains the student group codes
# 
# capt <- "ALL = All Students,
# AA = Black/African American,
# AI = American Indian or Alaska Native,
# AS = Asian,
# FI = Filipino,
# HI = Hispanic,
# PI = Pacific Islander,
# WH = White,
# MR = Multiple Races/Two or More,
# EL = English Learner,
# ELO = English Learners Only, 
# RFP = RFEPs Only,
# EO = English Only,
# SED = Socioeconomically Disadvantaged,
# SWD = Students with Disabilities,
# FOS = Foster Youth,
# HOM = Homeless Youth"
# 
# 
# 
# ###  Compile all the forms from the state into a single dataframe
# 
# compile <- function(ind) {
#         read.delim(paste0(year,"/" , ind ,"download2018.txt")) %>%
#                 #                 filter(countyname == county) %>%
#                 mutate(ind = ind)
# }
# 
# all <- data_frame()
# 
# for(i in files) {
#         
#         temp <- compile(i)
#         
#         all <- bind_rows(all, temp)
#         
#         assign(i, temp)
# }
# 
# 
# grad2017 <- read.delim("2017/graddownload2017f.txt")
# 
# 
# #  Fix factors
# 
# schools <-read.delim("pubschls.txt")
# narrow.schools <- schools %>% select(CDSCode,DOC:GSserved)
# 
# 
# 
# all <- all %>%
#         mutate(ind = factor(ind, levels = ind_ord),
#                studentgroup = factor(studentgroup, levels = sg_ord),
#                color.factor = factor(color)
#         ) %>%
#         mutate(districtname = str_replace_all(districtname, "County Office of Education", "COE"),
#                districtname = str_replace_all(districtname, "County Department of Education", "CDE"),
#                districtname = str_replace_all(districtname, "County Superintendent of Schools", "CSOS"),
#                districtname = str_replace_all(districtname, "Union", "U"),
#                districtname = str_replace_all(districtname, "Unified", "U"),
#                districtname = str_replace_all(districtname, "Elementary", "E"),
#                districtname = str_replace_all(districtname, "County", "C")
#         ) %>%
#         left_join(narrow.schools, by = c("cds" = "CDSCode")) %>%
#         mutate(type = as.character(type)) %>%
#         mutate(type = if_else(!is.na(type), type, case_when(EILCode == "ELEM" ~ "ES",
#                                                             EILCode == "HS" ~ "HS" ,
#                                                             EILCode == "INTMIDJR" ~ "MS", 
#                                                             EILCode == "No Data" & str_detect(DOCType, "Elementary") ~ "ED",
#                                                             EILCode == "No Data" & str_detect(DOCType, "High") ~ "HD", 
#                                                             EILCode == "No Data" & str_detect(DOCType, "Unified") ~ "UD")   )) %>%
#         mutate(cutoff = case_when(ind == "chronic" & statuslevel == 4 ~ 2.5, 
#                                   ind == "chronic" & statuslevel == 3 ~ 5.0,
#                                   ind == "chronic" & statuslevel == 2 ~ 10.0,
#                                   ind == "chronic" & statuslevel == 1 ~ 20.0,
#                                   
#                                   ind == "cci" & statuslevel == 1 ~ 10.0,
#                                   ind == "cci" & statuslevel == 2 ~ 35.0,
#                                   ind == "cci" & statuslevel == 3 ~ 55.0,
#                                   ind == "cci" & statuslevel == 4 ~ 70.0,
#                                   
#                                   ind == "grad" & statuslevel == 1 ~ 67.0,
#                                   ind == "grad" & statuslevel == 2 ~ 80,
#                                   ind == "grad" & statuslevel == 3 ~ 90,
#                                   ind == "grad" & statuslevel == 4 ~ 95,                                  
#                                   
#                                   ind == "ela" & statuslevel == 1 & type %in% c("ES", "ED", "MS", "UD") ~ -70,
#                                   ind == "ela" & statuslevel == 2 & type %in% c("ES", "ED", "MS", "UD") ~ -5,
#                                   ind == "ela" & statuslevel == 3 & type %in% c("ES", "ED", "MS", "UD") ~ 10,
#                                   ind == "ela" & statuslevel == 4 & type %in% c("ES", "ED", "MS", "UD") ~ 45,                                  
#                                   
#                                   ind == "ela" & statuslevel == 1 & type %in% c("HS", "HD") ~ -45,
#                                   ind == "ela" & statuslevel == 2 & type %in% c("HS", "HD") ~ 0,
#                                   ind == "ela" & statuslevel == 3 & type %in% c("HS", "HD") ~ 30,
#                                   ind == "ela" & statuslevel == 4 & type %in% c("HS", "HD") ~ 75,                                  
#                                   
#                                   ind == "math" & statuslevel == 1 & type %in% c("ES", "ED", "MS", "UD") ~ -95,
#                                   ind == "math" & statuslevel == 2 & type %in% c("ES", "ED", "MS", "UD") ~ -25,
#                                   ind == "math" & statuslevel == 3 & type %in% c("ES", "ED", "MS", "UD") ~ 0,
#                                   ind == "math" & statuslevel == 4 & type %in% c("ES", "ED", "MS", "UD") ~ 35,                                  
#                                   
#                                   ind == "math" & statuslevel == 1 & type %in% c("HS", "HD") ~ -115,
#                                   ind == "math" & statuslevel == 2 & type %in% c("HS", "HD") ~ -60,
#                                   ind == "math" & statuslevel == 3 & type %in% c("HS", "HD") ~ 0,
#                                   ind == "math" & statuslevel == 4 & type %in% c("HS", "HD") ~ 25,   
#                                   
#                                   ind == "susp" & statuslevel == 1 & type == "ED" ~ 0.5,
#                                   ind == "susp" & statuslevel == 2 & type == "ED" ~ 1.5,
#                                   ind == "susp" & statuslevel == 3 & type == "ED" ~ 3.0,
#                                   ind == "susp" & statuslevel == 4 & type == "ED" ~ 6.0,
#                                   
#                                   ind == "susp" & statuslevel == 1 & type == "HD" ~ 1.5,
#                                   ind == "susp" & statuslevel == 2 & type == "HD" ~ 3.5,
#                                   ind == "susp" & statuslevel == 3 & type == "HD" ~ 6.0,
#                                   ind == "susp" & statuslevel == 4 & type == "HD" ~ 9.0,
#                                   
#                                   ind == "susp" & statuslevel == 1 & type == "UD" ~ 1.0,
#                                   ind == "susp" & statuslevel == 2 & type == "UD" ~ 2.5,
#                                   ind == "susp" & statuslevel == 3 & type == "UD" ~ 4.5,
#                                   ind == "susp" & statuslevel == 4 & type == "UD" ~ 8.0,
#                                   
#                                   
#                                   ind == "susp" & statuslevel == 1 & type == "ES" ~ 0.5,
#                                   ind == "susp" & statuslevel == 2 & type == "ES" ~ 1.0,
#                                   ind == "susp" & statuslevel == 3 & type == "ES" ~ 3.0,
#                                   ind == "susp" & statuslevel == 4 & type == "ES" ~ 6.0,
#                                   
#                                   ind == "susp" & statuslevel == 1 & type == "MS" ~ 0.5,
#                                   ind == "susp" & statuslevel == 2 & type == "MS" ~ 2.0,
#                                   ind == "susp" & statuslevel == 3 & type == "MS" ~ 8.0,
#                                   ind == "susp" & statuslevel == 4 & type == "MS" ~ 12.0,
#                                   
#                                   ind == "susp" & statuslevel == 1 & type == "HS" ~ 0.5,
#                                   ind == "susp" & statuslevel == 2 & type == "HS" ~ 1.5,
#                                   ind == "susp" & statuslevel == 3 & type == "HS" ~ 6.0,
#                                   ind == "susp" & statuslevel == 4 & type == "HS" ~ 10.0,
#                                   TRUE ~ NA_real_),
#                cutoffdiff = cutoff - currstatus
#         ) 

grad2017 <- read.delim( here("data","2017", "graddownload2017f.txt") )

all <- readRDS(here("data" ,"01_all-school-dashboard-2018.rds") )


all.mc.schools <- all %>% 
        filter(color >= 1) %>%
        filter( str_detect( countyname, "Monterey") ) %>% 
        filter(studentgroup == "ALL") %>%
        filter(rtype == "S") %>%
        select(districtname, schoolname, ind, studentgroup, color, color.factor, currstatus, priorstatus) %>% 
        group_by(districtname, schoolname) %>% 
        arrange(districtname, schoolname)


# mc.schools.w1 <- all.mc.schools %>% 
#         filter(color == 1) %>%
#         select(districtname, schoolname) %>%
#         distinct()
# 
# all.mc.schools.w1 <- mc.schools.w1 %>% 
#         left_join(all.mc.schools) %>% 
#         group_by(districtname, schoolname) %>% 
#         arrange(districtname, schoolname) %>% 
#         mutate(reds = sum(color.factor == "1"),
#                oranges = sum(color.factor == "2"),
#                total = max( row_number() ) ,
#                CSI.all.reds = if_else(reds == total, 1, 0),
#                CSI.all.red.but.one = if_else(total - reds == 1, 1, 0),
#                CSI.all.red.or.orange = if_else(reds + oranges == total, 1, 0),
#                CSI.5.more.majority = if_else( (total >= 5) & ((reds/total) >.5) , 1, 0 )
# )
# 
# CSI.list <- all.mc.schools.w1 %>% 
#         mutate(CSI = if_else( sum(CSI.all.reds, CSI.all.red.but.one, CSI.all.red.or.orange, CSI.5.more.majority) > 0, "Y", "N") ) %>% 
#         select(districtname, schoolname, starts_with("CSI")) %>%
#         distinct() %>%
#         filter(CSI == "Y") %>%
#         arrange(desc(CSI), districtname, schoolname)



# 
# CSI.grad <- all.mc.schools %>% 
#         filter(ind == "grad") %>% 
#         mutate(CSI.grad.average = mean(currstatus, priorstatus))
# 





CSI.list2 <- all.mc.schools %>% 
        group_by(districtname, schoolname) %>% 
        arrange(districtname, schoolname) %>% 
        mutate(reds = sum(color.factor == "1"),
               oranges = sum(color.factor == "2"),
               total = max( row_number() ) ,
               CSI.all.reds = if_else(reds == total, 1, 0),
               CSI.all.red.but.one = if_else(total >=2 & total - reds == 1, 1, 0),
               CSI.all.red.or.orange = if_else( reds >=1  & reds + oranges == total, 1, 0),
               CSI.5.more.majority = if_else( (total >= 5) & ((reds/total) >.5) , 1, 0 ) ,
               two.year = (currstatus + priorstatus)/2 ,
               CSI.grad = if_else( ind == "grad" & two.year < 67, 1, 0    ),
               CSI.grad = sum(CSI.grad)) %>% 
        mutate(CSI = if_else( sum(CSI.all.reds, CSI.all.red.but.one, CSI.all.red.or.orange, CSI.5.more.majority, CSI.grad) > 0, "Y", "N") ) %>% 
        select(districtname, schoolname, reds, oranges, total, starts_with("CSI")) %>%
        distinct() %>%
        filter(CSI == "Y") %>%
        arrange(desc(CSI), districtname, schoolname) %>%
        filter(!str_detect(schoolname, "Special"))
               




write_csv(CSI.list2, here("data", "CSIlist.csv" ) )



CSI.list3 <- CSI.list2 %>%
        select(districtname, schoolname, CSI.grad) %>%
        left_join(all.mc.schools) %>%
        mutate(Reason = if_else(CSI.grad == 1, "Graduation Rate Below 67%", "Lowest 5%"),
               School = str_c(schoolname, districtname, sep = ",\n ") ,
               ind = recode(ind, "chronic" = "abs")
               )



ggplot(CSI.list3, aes(ind   , fct_rev(School)     ,  fill = color.factor )) + 
         geom_rect(data = CSI.list3, aes(fill =  Reason , alpha = 0.3) ,xmin = -Inf,xmax = Inf,
                   ymin = -Inf,ymax = Inf) +
        geom_tile(colour = "white") +
        geom_text(aes(label=ind)) +
        facet_wrap( fct_rev(Reason) ~.) +
        scale_fill_manual(values = pal) +
        theme_hc() +
        theme(
                legend.position = "none",
                # axis.title.x=element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                strip.background = element_rect(fill = "black"),
                strip.text = element_text(colour = 'white'),
                # panel.grid.major.y =   element_line(colour = "black",size=0.75),
                # panel.ontop = TRUE
                # panel.background = element_rect(fill = "lightblue")
        ) +
        labs(x="Dashboard Indicators",
             y="",
             title = "Dashboard Indicators for CSI Schools", 
             subtitle="", 
             fill="")


ggsave( here("figs", "CSI Dashboard Indicators with labels2.png"), width = 8.5, height = 6)
