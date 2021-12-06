library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(RcmdrMisc)
library(shiny)
library(shinyWidgets)
library(leaflet)
library(rjson)
library(dplyr)
library(jsonlite)
library(leaflet)
library(fmsb)
business<-read.csv('./data/business.csv')
radardata<-read.csv('./data/radar.csv')

datos<-business[,c(1:5,9,10)]
review<-stream_in(file('./data/review_Gym.json'), pagesize = 100)
single<-read.csv('./data/single_TOP100.csv')
bigram<-read.csv('./data/bigram_TOP100.csv')
trigram<-read.csv('./data/trigram_TOP50.csv')
comm<-read.csv('./data/comments.csv')
#===========Home============
shinyUI(fluidPage(theme = shinytheme("simplex"),
                  
                  titlePanel(fluidPage(
                    column(img(src="bigyelp.png",width="250px",height="100px"),width=3),
                    column(br(),"Review Analysis on GYMs",width=8)
                  )
                  ),
                  
                  navbarPage("",
                             tabPanel(icon("home"),
                                      tags$style(".fa-database {color:rgba(218,35,15)}"),
                                      h3(p(em("Dataset "),icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
                                      fluidRow(column(width=2),
                                               column(
                                                 br(),
                                                 p("Using this application, suggestions can be given to gyms' management in North America. 
                                                   The following pages include the locations, attributes and performance of the gyms. 
                                                   The last page provides analysis and suggestions of specific gyms.", 
                                                   style="text-align:justify;color:black;padding:15px;border-radius:10px"),
                                                 width=8)
                                      ),
                                      hr(),
                                      fluidRow(column(DT::dataTableOutput("RawData"),
                                                      width = 12)),
                                      
                                      hr(),
                                      p(em("Contact"),br("sliu736@wisc.edu"),style="text-align:center; font-family: times")
                             ),
                             #========================TOPwords==============================
                             tabPanel("FREQUENT WORDS",
                                      tags$style(".fa-chart-pie {color:rgb(218,35,15)}"),
                                      h3(p(em("Frequent words"),icon("chart-pie",lib = "font-awesome"),style="color:black;text-align:center")),
                                      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: coral; 
                                                      border-top: 1px coral; border-bottom: 1px coral;border-left: 1px coral}")),
                                      tags$style(HTML(".js-irs-0 .irs-max, .js-irs-0 .irs-min {background:papayawhip}")),
                                      fluidRow(column(width=2),
                                               column(
                                                 br(),
                                                 p("Barplots showing the dirstribution of ratings on the most frequent words/phrases.
                                                   Input numbers from 1 to 16.",
                                                   style="color:black;text-align:center;border-radius: 10px"),
                                                 width=8),
                                               br()
                                      ),
                                      hr(),
                                      fluidRow(
                                        column(
                                          sliderInput("Number", "Start from(integer):", min=1, max=16,value=1, step=1),width = 3
                                          ),
                                        column(
                                          selectInput('id_type',label="Select word type:",
                                                      selected = "Single",choices = c("Single","Bigram","Trigram")),width=3)
                                        ),
                                      mainPanel(
                                        fluidRow(
                                          column(br(),plotOutput("Bargrama"),br(),width=12,style="height:800px;width:1380px;border:1px solid black")
                                        )
                                      )
                             ),
                             #=================Locations==================
                             tabPanel("MAPS",
                                      tags$style(".fa-map-marker-alt {color:rgb(218,35,15)}"),
                                      h3(p(em("Gym Locations"),icon("map-marker-alt",lib = "font-awesome"),style="color:black;text-align:center")),
                                      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: coral; 
                                                      border-top: 1px coral; border-bottom: 1px coral;border-left: 1px coral}")),
                                      tags$style(HTML(".js-irs-0 .irs-max, .js-irs-0 .irs-min {background:papayawhip}")),
                                      fluidRow(column(width=2),
                                               column(
                                                 br(),
                                                 p("Available gyms can be found on the map. Select state -> city -> street to find gyms.",
                                                   style="color:black;text-align:center;border-radius: 10px"),
                                                 width=8),
                                               br()
                                      ),
                                      tabsetPanel(
                                        tabPanel("Address",
                                                 br(),br(),
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     checkboxGroupInput("state", h4("State"), 
                                                                        choices=c("Georgia"="GA","Massachusetts"="MA","Washington"="WA","British Columbia"="BC","Texas"="TX","Florida"="FL","Oregon"="OR","Colorado"="CO","Ohio"="OH")),
                                                     conditionalPanel("input.state",
                                                                      selectInput("city", h4("City"), c("All cities"=""), multiple=TRUE)), 
                                                     conditionalPanel("input.state",
                                                                      selectInput("address", h4("Street"), c("All streets"=""), multiple=TRUE)),
                                                     helpText("Black icon means not open"),
                                                     width = 3
                                                   ),
                                                   mainPanel(leafletOutput("mymap",height=580), width = 9)
                                                 )#sidebarlaypout1
                                        ),#address
                                        tabPanel("Name",
                                                 br(),br(),
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     selectizeInput('name', h4("Name"),
                                                                    choices=business$name%>%unique(), multiple=TRUE),
                                                     width = 3
                                                   ),
                                                   mainPanel(leafletOutput("mymap1",height=580), width = 9)
                                                 )#sidebarlayout2
                                        )#name
                                      )#tabsetpanel
                             ),
                             #============Ratings===================
                             tabPanel("EVALUATIONS",
                                      tags$style(".fa-info-circle {color:rgb(218,35,15)}"),
                                      h3(p(em("Performaces and Suggestions"),icon("info-circle",lib = "font-awesome"),style="color:black;text-align:center")),
                                      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: coral; 
                                                      border-top: 1px coral; border-bottom: 1px coral;border-left: 1px coral}")),
                                      tags$style(HTML(".js-irs-0 .irs-max, .js-irs-0 .irs-min {background:papayawhip}")),
                                      fluidRow(column(width=2),
                                               column(
                                                 br(),
                                                 p("Detailed information and correspondding evaluation/suggestion for any specfic gym can be found on this page.",
                                                   style="color:black;text-align:center;border-radius: 10px"),
                                                 width=8),
                                               br()
                                      ),
                                      hr(),
                                      sidebarLayout(
                                        sidebarPanel(
                                          selectizeInput('newname',h4('Gym Name:'),c("All names"=""), choices=business$name),
                                          selectizeInput('newaddress',h4('Complete Address:'),c("All addresses"=""), choices = business$newaddress%>%unique()),
                                          width = 4),
                                        mainPanel(
                                          tabsetPanel(
                                            tabPanel("Performance",
                                                     fluidRow(
                                                       br(),
                                                       column(
                                                         h4("Yelp Rating:"),
                                                         imageOutput("rating", width = "100%", height = "100px"),
                                                         h4("Popularity:"),
                                                         imageOutput("fire", width = "100%", height = "100px"),
                                                         h4("Attributes:"),
                                                         verbatimTextOutput('attri'),
                                                         width=3),
                                                       column(width=1),
                                                       column(h4("Nonattributes"),
                                                              plotOutput("radarplot"),
                                                              br(),
                                                              width=4)
                                                     )#fluidrow
                                            ),#panel1
                                            tabPanel("Suggestions",
                                                     br(),
                                                     h4("Strength:"),
                                                     verbatimTextOutput('suggest_pros'),
                                                     br(),
                                                     h4("Weakness:"),
                                                     verbatimTextOutput('suggest_cons'),
                                                     br(),br()
                                            )
                                          )#tabsetpanel
                                          ,width = 8)#mainpanel
                                      )#siderbarlayout
                             )#ratings
                  )#navbarpage
))
