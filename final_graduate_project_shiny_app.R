#================================================================================================#
#-------Kelvin Njuki                                                                             #
#-------STA 504 A                                                                                #
#-------Advanced Data Visualization                                                              #
#-------Graduate Project                                                                         #
#================================================================================================#

#Loading all necessary packages
library(tidyverse)
library(plotly)
library(stringr)
library(shiny)
library(rlang)

#Setting working directory
setwd("E:/Miami/ClassWork/Spring2020/STA 504/Graduate Project")

#================================================================================================#
#---------------------------------LOADING PROCESSED DATASETS-------------------------------------#
#================================================================================================#
#Setting working directory
setwd("E:/Miami/ClassWork/Spring2020/STA 504/Graduate Project")

#Datasets loading
load("covid_vaccines.RData")
load("covid_treatments.RData")

#================================================================================================#
#--------------------------------------------DEFINING UI-----------------------------------------#
#================================================================================================#

#Defining UI
ui <- fluidPage(
  titlePanel(
    # creating a title bar
    div("COVID-19 TREATMENTS AND VACCINES UNDER DEVELOPMENT", 
        style="text-align:center;width:100%;height:60px;margin:0;font-weight:bold;
        font-size:20px; color:#fff;background:red; line-height:60px")
            ),
  #First row
  fluidRow(
    column(style="border-top:3px solid grey;padding-top:20px;",
           width=3,
           p("Visualizing Treatments Categories versus Stage or Developers", 
             style="color:#fff;font-weight:bold;font-size:18px;border-radius:5px;background:green;padding:4px;
             text-align:center;border-right:3px solid grey; margin-bottom:10px;"),
           #Giving user flexibility to change the fill variable, x-axis variable is intact
           selectInput("t_yaxis", "Select Y-Axis variable", choices=colnames(covid_treatments[,3:4]),
                       selected = "Stage")
           
          ),

    column(style="border-top:3px solid grey;padding-top:20px; margin-bottom:20px;",
           width=9,
           plotlyOutput("trts_plots")
          )
          ),
  
  #Second row
  fluidRow(
    column(style="border-top:3px solid grey;padding-top:20px;",
           width=3,
           p("Visualizing Vaccines Categories versus Stage or Developers", 
             style="color:#fff;font-weight:bold;font-size:18px;border-radius:5px;background:green;padding:4px;
             text-align:center;border-right:3px solid grey; margin-bottom:10px;"),
           #Giving user flexibility to change the fill variable, x-axis variable is intact
           selectInput("v_yaxis", "Select Y-Axis variable", choices=colnames(covid_vaccines[,3:4]),
                       selected = "Developer")           
          ),
    
    column(style="border-top:3px solid grey;padding-top:20px; margin-bottom:20px;",
           width=9,
           plotlyOutput("vaccines_plot")
          )
        ),
 #Third row: Creating a footer with Data Source Link, App developer and the Instructor names
 fluidRow(
   HTML('<footer style="width:100%; height: 80px;background:green; color:#fff; padding:6px;">
                      <div style="border-right:#fff; width:30%; padding;left:6px;"> 
                          <span style="font-weight:bold;">Data Source:</span> <br>
                          <a href="https://milkeninstitute.org/covid-19-tracker" style="color:#fff;text-decoration:none;">https://milkeninstitute.org/covid-19-tracker</a>
                      </div>
                      <div style="border-right:1px dotted #fff; border-left:1px dotted #fff;width:35%;position:relative;left:30%;top:-40px; padding-left:10px;"> 
                          <span style="font-weight:bold;">Developed By:</span><br> Kelvin Njuki<br>
                          Statistics Graduate Student, Miami University
                      </div>
                      <div style=" width:35%;position:relative;left:65%; top:-100px;padding-left:10px;"> 
                          <span style="font-weight:bold;">Professor:</span><br>
                          Dr. Qingcong Yuan,  Miami University <br>
                          Advanced Data Visualization Instructor
                     </div>
         </footer>'
        )
          )
)


#================================================================================================#
#---------------------------------------GENERATING PLOTS-----------------------------------------#
#================================================================================================#

# Define server logic ----
server <- function(input, output) {
  
    output$trts_plots <- renderPlotly({
     
      y=parse_quo(input$t_yaxis, env=caller_env())

      trts_plots <- ggplot()+
        geom_bar(aes(x=Category, fill=!!y), 
                 stat="count",position="stack",
                 data=covid_treatments)+
        labs(x="Treatment Category",y="Counts",
             title=paste("Distribution of Treatments Categories versus", input$t_yaxis)) +
        theme_bw()+
        coord_flip()
      ggplotly(trts_plots)
    })
    
    output$vaccines_plot <- renderPlotly({
      
      y=parse_quo(input$v_yaxis, env=caller_env())
      
      vaccines_plots <- ggplot()+
        geom_bar(aes(x=Category, fill=!!y), 
                 stat="count",position="stack",
                 data=covid_vaccines)+
        labs(x="Vaccine Category",y="Counts",
             title=paste("Distribution of Vaccines Categories versus", input$v_yaxis)) +
        theme_bw()+
        coord_flip()
      ggplotly(vaccines_plots)
    })
}
#================================================================================================#

# Run the app
shinyApp(ui = ui, server = server)