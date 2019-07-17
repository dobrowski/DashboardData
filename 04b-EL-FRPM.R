


if (!require("pacman")) install.packages("pacman")
pacman::p_load(
        tidyverse
        ,here
        ,janitor
        ,ggthemes
        ,knitr
        ,kableExtra
        ,formattable
        ,readxl
)


EL.schools <-read.delim(here("data",  "LtelDownload.txt"))

EL.schools <- EL.schools %>% 
        mutate(cds = str_c( str_pad(  as.character(CountyCode) , width = 2, side = "left", pad = "0"  ) ,
                            str_pad(  as.character(DistrictCode), width = 5, side = "left", pad = "0"  ) ,
                            str_pad( as.character(SchoolCode), width = 7, side = "left", pad = "0"  )  )
               ) %>%  # current EL
        filter(Gender == "ALL") %>%
#        filter(str_detect(CountyName , "Monterey") ) %>%
        group_by(cds) %>%
        mutate(sumEL = sum(EL),
               sumTotal = sum(TotalEnrollment),
                ELpercent = sumEL/sumTotal) %>%
        select(SchoolName, cds, ELpercent) %>%
        ungroup() %>%
        distinct() 


frpm1718 <- read_excel(here("data", "frpm1718.xlsx"), sheet = "FRPM School-Level Data ", range = "A2:AB10477") %>% 
        mutate(cds = str_c(`County Code`,`District Code`,`School Code`)) %>%
        select(cds, starts_with("Percent")  ) %>%
        select(cds, 3) 

colnames(frpm1718) <- (c("cds", "frpm"))


school.EL.FRPM <- EL.schools %>% left_join(frpm1718) # %>% mutate(cds = as.numeric(cds))
