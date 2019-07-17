# https://www.cde.ca.gov/ta/ac/cm/

### Load libraries

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
        tidyverse
        ,here
        ,gridExtra
        ,janitor
        ,ggthemes
        ,knitr
        ,kableExtra
        ,formattable
)


### Define default variables

# files <- c("cci", "chronic", "ela", "elaprate", "elpi", "grad", "math", "mathprate", "susp")

files2017 <- c("cci",  "ela11", "ela38",  "grad", "math11", "math38", "susp", "elpi")
files2018 <- c("cci", "chronic", "ela", "elaprate" , "grad", "math", "mathprate" , "susp", "elpi")

county <- "Monterey"

pal <- c("0" = "white",
         "1" = "red",
         "2" = "orange",
         "3" = "yellow",
         "4" = "green",
         "5" = "blue")

sg_ord <- c("ALL", "AA", "AI", "AS", "FI", "HI", "PI", "WH", "MR", "EL", "ELO", "EO", "RFP", "SED", "SWD", "FOS", "HOM")

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

compile2017 <- function(ind) {
        read.delim(paste0("2017/" , ind ,"download2017f.txt")) %>%
                #                 filter(countyname == county) %>%
                mutate(ind = ind)
}

compile2018 <- function(ind) {
        read.delim(paste0("2018/" , ind ,"download2018.txt")) %>%
                #                 filter(countyname == county) %>%
                mutate(ind = ind)
}

TSI.calc <- function(df) { df %>%
                select(districtname, schoolname, ind, studentgroup, color, color.factor, year) %>% 
                group_by(districtname, schoolname, studentgroup) %>% 
                arrange(districtname, schoolname, studentgroup) %>% 
        mutate(reds = sum(color.factor == "1"), # Calculate the number of Red, Orange and Total indicators
               oranges = sum(color.factor == "2"),
               total = max( row_number() ) ,
               TSI.all.reds = if_else(total >= 2 & reds == total, 1, 0),  # Check against the various 5% criteria
               TSI.all.red.but.one = if_else(total >=2 & total - reds == 1, 1, 0),
               TSI.all.red.or.orange = if_else(total >=2 & reds >=1 &  reds + oranges == total, 1, 0),
               TSI.5.more.majority = if_else( (total >= 5) & ((reds/total) >.5) , 1, 0 ) 
        ) %>% # See if any of the criteria qualify the subgroup at the school
                mutate(TSI = if_else( sum(TSI.all.reds, TSI.all.red.but.one, TSI.all.red.or.orange, TSI.5.more.majority) > 0, 1, 0) )
}


all2017 <- data_frame()
all2018 <- data_frame()

for(i in files2017) {
        
        temp <- compile2017(i)
        
        all2017 <- bind_rows(all2017, temp)
        
        assign(i, temp)
}

for(i in files2018) {
        
        temp <- compile2018(i)
        
        all2018 <- bind_rows(all2018, temp)
        
        assign(i, temp)
}


all2017 <- all2017 %>%
        mutate(ind = factor(ind, levels = files2017),
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
        )  %>%
        mutate(ind = case_when( ind == "ela11" ~ "ela",
                                ind == "ela38" ~ "ela",
                                ind == "math11" ~ "math",
                                ind == "math38" ~ "math",
                                TRUE ~ as.character( ind)  ) )%>%
        mutate(year = as_factor("2017")) %>%
        select(-reportingyear)


all2018 <- all2018 %>%
        mutate(ind = factor(ind, levels = files2018),
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
        )  %>%
        mutate(year = as_factor("2018")) %>%
        select(-reportingyear)

all.two.years <- all2018 %>%
        bind_rows(all2017) %>%
        mutate(cds = str_pad( as.character(cds), width = 14, side = "left", pad = "0"  ) )

write_rds(all.two.years, here("data","all_two_years.rds") )



all.mc.schools2017 <- all.two.years %>% 
        filter(year == "2017") %>%
        filter(color >= 1) %>%
        filter( str_detect( countyname, "Monterey") ) %>% 
        filter(rtype == "S") %>%
        TSI.calc()
        

all.mc.schools2018 <- all.two.years %>% 
        filter(year == "2018") %>%
        filter(color >= 1) %>% # Only those rated 
        filter(str_detect( countyname, "Monterey") ) %>% # Only Monterey County
        filter(rtype == "S") %>% # Only schools 
        TSI.calc()


all.mc.school.two.years <- all.mc.schools2018 %>%
        bind_rows(all.mc.schools2017) %>%  # Combine the years 
        select(districtname, schoolname, studentgroup, year, TSI) %>%
        distinct() %>%  # see which subgroups in which schools have TSI for a year
        group_by(districtname, schoolname, studentgroup) %>%
        arrange(districtname, schoolname, studentgroup) %>% 
        mutate(both.years = sum(TSI)) %>% 
        filter(both.years >= 2) %>% # Make sure the subgroup qualifies two years in a row
        select(districtname, schoolname, studentgroup) %>% 
        distinct() # simplify the list

# remove schools on CSI list
TSI.list <- all.mc.school.two.years %>% anti_join(CSI.list2)

TSI.list2 <- TSI.list %>%
        ungroup()  %>%
        select(districtname, schoolname) %>%
        distinct() %>%
        filter( !str_detect(schoolname, "Greenfield"))

write.csv(TSI.list, here("data","TSIlist.csv") )


all.mc.school.two.years.more <- all.mc.schools2018 %>%
        bind_rows(all.mc.schools2017) %>%
        select(-ind, -color, -color.factor) %>%
        distinct()


TSI.list3 <- TIS.list %>%
        filter( !str_detect(schoolname, "Greenfield")) %>%
        left_join(all.mc.school.two.years.more)

write.csv(TSI.list3, here("data", "TSIlist3.csv") )

TSI.list3 %>%
        mutate(TSI = if_else(TSI == 1, TRUE, FALSE)) %>%
        mutate(reds = color_bar("red")(reds),
               oranges = color_bar("orange")(oranges),
               TSI.all.red.or.orange = color_text("white", "blue")(TSI.all.red.or.orange),
               TSI.all.reds = color_tile("white", "black")(TSI.all.reds)
               ) %>%
        formattable(
                list(TSI = formatter("span",
                                     style = x ~ style(color = ifelse(x, "green", "red")),
                                     x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))
                                         )
                        )
                ) %>%
        kable("html" , booktabs = T, escape = F) %>%
        kable_styling(bootstrap_options = c("striped","hover")) %>%
        column_spec(5:7, width = "5em") %>%
        save_kable("test.html")





soledad.el.swd <- all.mc.schools2018 %>%
        bind_rows(all.mc.schools2017) %>%  # Combine the years 
        filter( str_detect(districtname, "Soledad") ,
                studentgroup %in% c("SWD",  "EL") )

pal <- c("0" = "white",
         "1" = "red",
         "2" = "orange",
         "3" = "yellow",
         "4" = "green",
         "5" = "blue")
        

ggplot(soledad.el.swd, aes(ind   ,schoolname  ,  fill = color.factor )) + 
        geom_tile(colour = "white") +
        facet_grid( studentgroup ~ year) +
        scale_fill_manual(values = pal) +
        theme_hc() +
        theme(legend.position = "none") +
        labs(x="Indicator",
             y="",
             title = "Soledad indicators for EL and SWD in 2017 and 2018", 
             subtitle="", 
             fill="")





scj1 <- all2017 %>% filter(studentgroup == "SWD", str_detect(districtname, "South Monterey"))
scj2 <- all2018 %>% filter(studentgroup == "SWD", str_detect(districtname, "South Monterey"))

write_csv(scj2, here("data","SMCJUH-SWD.csv") )

