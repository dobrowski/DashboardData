
#  Load libraries needed ------

library(tidyverse)
library(here)
library(janitor)



####  Identify student groups more than two levels below All students

# all <- readRDS(here("data" ,"01_all-school-dashboard-2018.rds") )
# 
# monterey <- all %>% 
#         filter(str_detect( countyname, "Monterey"))
# 
# saveRDS(monterey, here("data" ,"01_monterey-school-dashboard-2018.rds") )

monterey <- readRDS( here("data" ,"01_monterey-school-dashboard-2018.rds") )

mont.below2 <- monterey %>% 
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
        select(-schoolname) %>%
        arrange(districtname)



####  Identify indicators that are Red or Orange for All students


mont.redorange <- monterey %>% 
        filter(str_detect( countyname, "Monterey") ,
               rtype == "D",
               color %in% c(1,2),
               studentgroup == "ALL") %>%
        select(districtname, ind, studentgroup, color, color.factor, currstatus, priorstatus, change, cutoff, cutoffdiff) %>%
        arrange(districtname)




mont.summ <- monterey %>% 
        filter(str_detect( countyname, "Monterey") ,
               rtype == "D",
               # color %in% c(1,2),
               studentgroup == "ALL",
               ind != "cci") %>%
        select(districtname, ind, studentgroup, color, color.factor, currstatus, priorstatus, change, cutoff, cutoffdiff) %>%
        arrange(districtname)


mont.elpi <- monterey %>% 
        filter(str_detect( countyname, "Monterey") ,
               rtype == "D",
               # color %in% c(1,2),
               ind == "elpi") %>%
        select(districtname, ends_with("Pct")) %>%
        select(districtname, starts_with("PL")) %>%
        arrange(districtname)



district.list <- unique(mont.below4$districtname)
names(district.list) <- district.list


# UI -------

ui <- fluidPage(
        
        # App title ----
        titlePanel("LCAP Cheat Sheet"),
        
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(
                        
                        # checkboxInput("district", label = "Districts Only", value = TRUE),
                        selectInput("select", h3("Choose a district"), 
                                    choices = district.list,
                                    selected = 1)
                        
                        # p(span("Red is 2015-16,", style = "color:red"),
                        #   span("Orange is 2016-17", style = "color:orange"),
                        #   " and",
                        #   span( "Dark blue is 2017-18",  style = "color:darkblue")
                        #   )
                ),
                
                
                # Main panel for displaying outputs ----
                mainPanel(
                        

                        
                        # Output: Chart ----
                        h4("Greatest Need"),
                       tableOutput('table'),
                        
                        # Output: Chart ----
                        h4("Performance Gaps"),
                       tableOutput('table2'),
                       
                       #  4A Statewide Assessments : ELA and Math 
                       #  4C EL progress
                       #  5B Chronic Absentee
                       #  5E Graduation Rate 
                       #  6A Suspension Rate
                       
                       # Output: Chart ----
                       h4("Priority 4a - Statewide Assessments"),
                       tableOutput('table4a'),
                       
                       # Output: Chart ----
                       h4("Priority 4c - EL Progress (Level 4 is Well Developed)"),
                       tableOutput('table4c'),
                       
                       # Output: Chart ----
                       h4("Priority 5b - Absenteeism"),
                       tableOutput('table5b'),
                       
                       # Output: Chart ----
                       h4("Priority 5e - Graduation"),
                       tableOutput('table5e'),
                       
                       # Output: Chart ----
                       h4("Priority 6a - Suspension"),
                       tableOutput('table6a')
                       
                       
                       )
        )
)


#  4A Statewide Assessments : ELA and Math 
#  4C EL progress
#  5B Chronic Absentee
#  5E Graduation Rate 
#  6A Suspension Rate

#  Graphing ------

server <- function(input, output) {

               output$table <- renderTable(mont.redorange %>% 
                                               filter(str_detect(districtname,input$select) ) )  
               
               output$table2 <- renderTable(mont.below4 %>% 
                                               filter(str_detect(districtname,input$select)) )

               output$table4a <- renderTable(mont.summ %>% 
                                                     filter(str_detect(districtname,input$select),
                                                            ind %in% c("ela", "math") ) )
               
               output$table4c <- renderTable(mont.elpi %>% 
                                                     filter(str_detect(districtname,input$select) ) )
               
               
              output$table5b <- renderTable(mont.summ %>% 
                                                         filter(str_detect(districtname,input$select),
                                                                ind == "chronic" ) )
               
               output$table5e <- renderTable(mont.summ %>% 
                                                         filter(str_detect(districtname,input$select),
                                                                ind == "grad" ) )
               
               output$table6a <- renderTable(mont.summ %>% 
                                                         filter(str_detect(districtname,input$select),
                                                                ind == "susp" ) )
}



shinyApp(ui = ui, server = server)
