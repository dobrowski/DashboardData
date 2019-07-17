


###  This looks at all the Districts/Schools and says the average color rating by indicator (1 = Red)


all.mean <- all %>% filter(color != 0) %>%
        select(1:4, color, ind, studentgroup) %>%
        group_by(districtname, schoolname, ind) %>%
        summarise(mean = mean(color)) %>% 
        arrange(mean)


###  This looks at all the Districts/Schools and says how many student groups are red for that indicator

all.count1 <- all %>% 
        filter(color != 0) %>%
        select(1:4, color, ind, studentgroup) %>%
        group_by(districtname, schoolname, ind) %>%
        summarise(count = sum(color == 1)) %>% 
        arrange(count)



###  This looks at all COEs and graphs the red indicators and orange for the academic ones too


all.count2 <- all %>% 
        #       filter(coe_flag == "Y") %>%
        filter(color >= 1) %>%
        #       filter( (color %in% c(1,2) & ind %in% c("ela", "math")) | color == 1 ) %>%
        filter( color %in% c(1,2)  ) %>%
        filter( str_detect( countyname, "Monterey") ) %>% 
        filter( str_detect( districtname, "Soledad") ) %>% 
        #        filter(studentgroup == "ALL") %>%
        filter(rtype == "S") %>%
        select(districtname, schoolname, ind, studentgroup, color, color.factor, currdenom ,cutoffdiff, currstatus) %>% 
        group_by(districtname, schoolname) %>% 
        arrange(districtname, schoolname)



gg <- ggplot(all.count2, aes(ind, y = fct_rev(studentgroup) )) + 
        geom_tile(aes(fill = color.factor)) +
        scale_fill_manual(values = pal) +
        theme_minimal() +
        #        legend(element_blank()) +
        facet_wrap(~districtname, ncol = 3) +
        theme(legend.position = "none") +
        labs(x = "LCFF Priority",
             y = "Student Groups",
             title = "Student Groups by Color by LCFF Priority by COE",
             subtitle = "In order by English/Language Arts, Math, Graduation Rate, Chronic Absenteeism, \n Suspension Rate, College/Career Indicator",
             caption = capt)



gg


ggsave(here("figs","COEs.png"), gg, width = 7, height = 36, units = "in" )

# ggsave("MRYschools.png", gg, width = 7, height = 36, units = "in" )


#  Attempt to have x axis on each subgraph

# cc <- "Monterey COE"

counties <- unique(all.count2$districtname) %>% sort()

lapply(counties, function(cc) {
        ggplot(filter(all.count2, districtname == cc), aes(ind, y = fct_rev(studentgroup))) + 
                geom_tile(aes(fill = color.factor)) +
                scale_fill_manual(values = pal) +
                theme_minimal()  +
                theme(legend.position = "none") +
                labs(x="",y="",title =  cc)      
}) -> cclist

cclist[["ncol"]] <- 4

gg <- do.call(grid.arrange, cclist)


ggsave(here("figs" ,"coes.jpg"), gg, width = 9.5, height = 24, units = "in" )



####  Identify student groups more than two levels below All students

mont.below2 <- all %>% 
        filter(str_detect( countyname, "Monterey") ,
               color != 0) %>%
        select(districtname, schoolname, ind, studentgroup, color, color.factor, cutoff, cutoffdiff) 

mont.below2.all <-  mont.below2 %>% 
        filter(studentgroup == "ALL") %>% 
        mutate(all =  color) %>%
        select(districtname, schoolname, ind, all)

mont.below3 <- mont.below2 %>%
        left_join(mont.below2.all) %>%
        mutate(steps =  all - color) %>%
        filter(steps >= 2) %>%
        arrange(districtname, schoolname)



mont.below4 <- mont.below2 %>%
        left_join(mont.below2.all) %>%
        mutate(steps =  all - color) %>%
        filter(steps >= 2,
               str_length(schoolname) <= 2 ) %>%
        arrange(districtname, schoolname)


mont.below2 %>% tabyl(districtname,studentgroup) %>% adorn_totals()



# Chronic percentage for all schools and districts 


chronic.df <- chronic %>%
        filter(countyname == "Monterey") %>%
        select(districtname, schoolname, studentgroup,currstatus, color) %>%
        arrange(districtname, schoolname)




chronic.df <- chronic %>%
        filter(countyname == "Monterey", studentgroup == "ALL") %>%
        select(districtname, schoolname, currstatus, color) %>%
        arrange(districtname, schoolname)

write_csv(chronic.df, here("data", "chronic.csv") )



temp <- all %>% filter(color == 5) %>% select(statuslevel, changelevel)
temp <- all %>% filter(ind == "ela") %>% filter(str_detect(districtname, "South Monterey"  ) )

temp <- all %>% filter( str_detect(  EILCode, "No Data") )


## Dashboard ELA and Math for CORE using the SBA percentages



# CORE.compare <- all %>%
#         filter(rtype == "D") %>%
#         filter( str_detect( countyname, "Monterey") ) %>% 
#         filter(studentgroup == "ALL") %>% 
#         filter(ind == "math" | ind == "ela" | ind == "susp" | ind == "chronic")  %>%
#  #       mutate(schoolname == trimws(schoolname))
#         filter( str_detect( districtname, "Mission|Salinas|Chualar|Soledad") )  %>% 
#         select( districtname, ind, starts_with("ca"))
#   #      filter(districtname %in%  c("Mission U E"))
# 
# 


sba <- read_delim(here("data", "sb_ca2018_all_27_csv_v3.txt"), delim = ",")

sba.entity <- read_delim(here("data", "sb_ca2018entities_csv.txt"), delim = ",") 


sba.entity <- sba.entity %>%
        select(`County Code`, `District Code`, `School Code`, `District Name`) %>%
        mutate(`County Code` = as.numeric(`County Code`) )


districts <- paste(c("Mission", "Salinas City", "Chualar", "Soledad", "King", "Penin"),collapse = '|')


sba2 <- sba %>% 
        filter(`Subgroup ID` == 1) %>%  # All students 
        filter(`School Code` == "0000000") %>%  # Districts only, not school level
        filter(Grade == 13) %>% # All grades 
        left_join(sba.entity) %>%
        filter( str_detect(`District Name` , districts) )  %>%
        select(`District Name`, `Test Id`, `Percentage Standard Met and Above`)

write.csv(sba2, here("data",  "SBA scores.csv"))

# local.schools <-  schools %>%
#         select(NCESDist, District) %>% 
#         mutate(NCESDist = str_sub(NCESDist, 1, str_length(NCESDist)-1) ) %>%
#         mutate(NCESDist = str_sub(NCESDist,2)) %>%
#         distinct()


