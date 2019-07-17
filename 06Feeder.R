


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


ds2 <- tibble(HSdistrict = c("Salinas Union", "SMCJUHSD"),
              feeder = list(c("Alisal", "Graves", "Lagunita", "Salinas City", "Santa Rita", "Spreck", "Washington"),
                            c("Greenf", "King City", "San Anton", "Bradley", "San Ardo", "San Lucas")))

grouper <- tibble(
        subgroup = c("ALL", "SWD", "EL"),
        susp = c("TA", "SD" , "SE"),
        chron = c("GR", "SD" , "SE"),
        sba.code = c(1, 128, 160) )


chronic.mc <- read_tsv(here("data", "ChrAbsRate1718v2.txt")) %>%
        filter( str_detect( CountyName, "Monterey") )


sba <- read_delim(here("data", "sb_ca2018_all_27_csv_v3.txt"), delim = ",") %>%
        mutate(`County Code` = as.character(`County Code`))

sba.entity <- read_delim(here("data", "sb_ca2018entities_csv.txt"), delim = ",") 

sba.mc <- sba %>% left_join(sba.entity)

susp.mc <- read_tsv(here("data", "susp1718.txt")) %>%
        filter( str_detect( CountyName, "Monterey") ) 

for(i in c(1:2)){

        for(j in c(1:3)){
        
districts <- ds2[[i,2]]

print(districts)

chronic <- chronic.mc  %>%
        filter(str_detect( ReportingCategory, grouper[[j,3]])) %>%  #  filter to change for SWD (SD)  and EL (SE)
        filter( str_detect(DistrictName , paste(districts,collapse = '|')  ) ) %>%
        filter(AggregateLevel != "D2")  %>%  
        filter(CumulativeEnrollment != "*") %>%
        mutate(gradelevel = fct_inorder(ReportingCategory, ordered = TRUE)) %>%
        filter(!str_detect( ReportingCategory, "GRK") ) %>%
        group_by(DistrictName, SchoolName) %>%
        filter(gradelevel == max(gradelevel)) %>%
        select(CountyCode:SchoolName, gradelevel, ChronicAbsenteeismRate) %>%
        ungroup()


sba2 <- sba.mc  %>%
        filter( str_detect(`District Name` , paste(districts,collapse = '|')  ) ) %>% 
        filter(`CAASPP Reported Enrollment` != "*") %>%
        filter(`Subgroup ID` == grouper[[j,4]]) %>%  # All students ;  change to ELO=160 and SWD =128
        filter(Grade != 13) %>%
        group_by(`District Name`, `School Name`) %>%
        filter(Grade == max(Grade)) %>%
        select(CountyCode = `County Code`,DistrictCode = `District Code`, SchoolCode = `School Code`,`District Name`,`School Name`, `Test Id`, Grade, `Percentage Standard Met and Above`)  %>%
        ungroup() %>%
        spread(`Test Id`, `Percentage Standard Met and Above`) %>%
        rename(ELA = `1`,
               Math = `2`)



susp <- susp.mc %>%
        filter(str_detect( ReportingCategory, grouper[[j,2]])) %>%   # filter to change for SWD (SD)  and EL (SE) 
        filter( str_detect(DistrictName , paste(districts,collapse = '|')  ) ) %>%
        filter(AggregateLevel != "D2")  %>%
        filter(!(AggregateLevel == "S" & str_detect( SchoolName, "District")  )) %>% 
        select(CountyCode:SchoolName,`Suspension Rate (Total)`)



feeders <- chronic %>%
        left_join(susp) %>%
        mutate(DistrictCode = as.character(DistrictCode),
               CDSCode = paste0(CountyCode,DistrictCode,SchoolCode)) %>%
        left_join(sba2) %>%
        select(-`District Name`, -`School Name`) %>% 
        arrange(DistrictName)


print(feeders %>% select(DistrictName) %>% distinct())


write_csv(feeders, here("data", "feeder", paste0("Feeder Districts ", ds2[[i,1]], " ", grouper[[j,1]], ".csv")))

myHeader <- c(length(feeders)-5 )
label <- paste0( c(ds2[[i,1]]), " ", grouper[[j,1]] )
 names(myHeader) <- label
 
feeders %>% 
        select(-1:-4, -CDSCode) %>%
        mutate(gradelevel = recode(gradelevel, "SE" = "EL", "SD" = "SWD" ),
               SchoolName = if_else(str_detect(SchoolName,"District"), "District Overall", SchoolName ) ) %>%
        rename(`District Name` = DistrictName,
               `School` = 2,
               Group = gradelevel,
               Rate = 4,
               `School Rate` = 5) %>%
        kable(booktabs = TRUE) %>%
        kable_styling("striped", "condensed", full_width = F) %>%
        column_spec(5, border_left = TRUE, border_right = TRUE) %>%
        add_header_above(c(" " = 2,"Chronic Absenteeism" = 2,"Suspension" = 1, "SBA Meet or Exceed" = 3)    ) %>% 
        add_header_above(  myHeader , bold = TRUE) %>%
        collapse_rows(columns = 1) %>%
        save_kable(here("figs","feeder",paste0(label,".png")))

}

        #### English Language Progress ----
        
        mc.elpi <- elpi %>%
                filter( str_detect( countyname, "Monterey") ) %>%
                filter( str_detect(districtname , paste(districts,collapse = '|')  ) ) %>%
                select(CDSCode = cds,
                       District = districtname,
                       SchoolName = schoolname,
                       ends_with("Pct")) 
        
        write_csv(mc.elpi, here("data", "feeder", paste0("Feeder Districts ", ds2[[i,1]], " ELPI.csv")))
        
        
        
        }

