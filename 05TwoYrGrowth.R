
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

math.table <- function(df,label){
        lbs <- label
        
         myHeader <- c(length(df) )
         names(myHeader) <- c(label)
        
        print(label)
        
        df %>%
        head(10) %>%
        kable(booktabs = TRUE) %>%
        kable_styling("striped", "condensed", full_width = F) %>%
        add_header_above(  myHeader , bold = TRUE, background = "skyblue") %>%
        column_spec(1, width = "20em") %>%
        save_kable(here("figs","math",paste0(label,".png")))
}



all.two.years <- read_rds(here ("data", "all_two_years.rds"))

title1 <- read_xlsx(here("data","scheligibilitystate.xlsx")) %>%
        select(-ReportingYear)  %>% 
        mutate(cds = as.character(CDS))





all.mc.school.two.years <- all.two.years %>% 
        filter( str_detect( countyname, "Monterey") ) %>% 
        filter(rtype == "S") %>%
        filter(studentgroup == "ALL") %>%
        filter(ind %in% c("ela","math")) %>%
        select(schoolname, districtname, currdenom, currstatus, change, ind, year) 

two.yr.growth <- all.mc.school.two.years %>%
        group_by(schoolname, districtname, ind) %>%
        summarise(`Two Year Points Change` = sum(change)) %>%
        arrange(ind, desc(`Two Year Points Change`))


two.yr.growth %>% 
        filter(ind == "ela") %>%
        select(-ind) %>%
        math.table("ELA 2 Year Growth")


two.yr.growth %>% 
        filter(ind == "math") %>%
        select(-ind) %>%
        math.table("Math 2 Year Growth")

###

all.tail<- tail(all.two.years, 200)


all.tail.join <- all.tail %>% left_join(title1) %>% left_join(school.EL.FRPM )


math.state <- all.two.years %>% 
 #       filter( str_detect( countyname, "Monterey") ) %>% 
        filter(year == "2018") %>%
        filter(rtype == "S") %>%
        filter(studentgroup == "ALL") %>%
        filter(charter_flag != "Y") %>%
        filter(ind == "math") %>%
        select(cds,schoolname, districtname, countyname, currdenom, currstatus, change, box, color,  ind, year) %>%
        left_join(title1) %>%
        left_join(school.EL.FRPM) %>%
        arrange(desc(currstatus)) %>% 
        mutate(stat.mean = mean((currstatus+295), na.rm = TRUE),
               status.scaled = (currstatus+295 - stat.mean )/stat.mean,
               chan.mean = mean(change+137, na.rm = TRUE),
               change.scaled = (change+137 - chan.mean )/chan.mean,
               scale.comp = status.scaled*change.scaled)


# function

make.tables<-function(df, titl){

df %>%
        arrange(desc(currstatus)) %>%
        select(`School Name` = schoolname, `District Name` = districtname, `County` = countyname ,`Current Status` = currstatus, `Change` = change) %>%
        math.table(paste0("Top Status ", titl))

df %>%
        arrange(desc(change)) %>%
        select(`School Name` = schoolname, `District Name` = districtname, `County` = countyname ,`Current Status` = currstatus, `Change` = change) %>%
        math.table(paste0("Top Change ", titl))


df  %>%
        filter( change.scaled > 0) %>% 
        arrange(desc(scale.comp)) %>%
        select(`School Name` = schoolname, `District Name` = districtname, `County` = countyname ,`Current Status` = currstatus, `Change` = change) %>%
        math.table(paste0("Top Composite ", titl))
}

make.tables(math.state, "State")

make.tables( math.state %>% filter(TitleIStatus1718 == "Yes"), "State Title 1" )

make.tables( math.state %>% filter(ELpercent >= .50), "State EL Above 50 percent" )

make.tables( math.state %>% filter(frpm >= .75), "State FRPM Above 75 percent" )

make.tables( math.state %>% filter(frpm >= .75, ELpercent >= .50), "State EL and FRPM" )

make.tables( math.state %>% filter(frpm >= .75, str_detect(schoolname, "High")), "State FRPM High School" )



make.tables(math.mc, "Monterey")

make.tables(math.mc  %>% filter(TitleIStatus1718 == "Yes"), "Monterey Title 1")

make.tables(math.mc  %>% filter(ELpercent >= .50), "Monterey EL Above 50 percent")

make.tables(math.mc  %>% filter(frpm >= .75), "Monterey FRPM Above 75 percent")






#### Subgroups in Monterey County districts


math.mc.subs <- all.two.years %>% 
        filter( str_detect( countyname, "Monterey") ) %>% 
        filter(year == "2018") %>%
        filter(rtype == "D") %>%
#        filter(studentgroup == "ALL") %>%
        filter(charter_flag != "Y") %>%
        filter(ind == "math") %>%
        select(districtname, studentgroup, currstatus, change, color, color.factor) %>%
        arrange(desc(currstatus))

pal <- c("0" = "light grey",
         "1" = "red2",
         "2" = "orange",
         "3" = "yellow",
         "4" = "lime green",
         "5" = "deep sky blue",
         "Graduation Rate Below 67%" = alpha("light grey", alpha = 0.3),
         "Lowest 5%" = alpha( "white", alpha = 0.3))



ggplot(data = math.mc.subs %>% filter(!is.na(currstatus),
                                      studentgroup != "ELO",
                                      studentgroup != "EO",
                                      studentgroup != "RFP"),
       aes(studentgroup, fct_rev(districtname),  fill = color.factor)) + 
        geom_tile(colour = "white") +
        geom_text( aes(label= round( currstatus ) )) +
        scale_fill_manual(values = pal) +
        theme_hc() +
        theme(
                legend.position = "none",
                # axis.title.x=element_blank(),
 #               axis.text.x = element_blank(),
                axis.ticks.x = element_blank()
        ) +
        labs(x="Student Subgroup",
             y="",
             title = "Math Color by Student Group in District", 
             subtitle="", 
             fill="")


ggsave( here("figs", "math", "Monterey districts by subgroup w scores.png"), width = 10, height = 10)



ggplot(data = math.mc.subs %>% filter(color >= 1), aes(studentgroup , fct_rev(districtname)     ,  fill = color.factor)) + 
        geom_tile(colour = "white") +
        geom_text( aes(label=  studentgroup)) +
        scale_fill_manual(values = pal) +
        theme_hc() +
        theme(
                legend.position = "none",
                # axis.title.x=element_blank(),
                #               axis.text.x = element_blank(),
                axis.ticks.x = element_blank()
        ) +
        labs(x="Student Subgroup",
             y="",
             title = "Math Color by Student Group in District", 
             subtitle="", 
             fill="")


ggsave( here("figs", "math", "Monterey districts by subgroup.png"), width = 8.5, height = 9.5)


#  filter out the child development center 
#  new graph county overall with subgroups 
#  set at 40%  