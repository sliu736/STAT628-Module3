library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(RcmdrMisc)
library(lmtest)
library(tidyverse)
library(rjson)
library(dplyr)
library(jsonlite)
library(leaflet)
library(fmsb)

#==========data==================
business<-read.csv('./data/business.csv')
radardata<-read.csv('./data/radar.csv')

datos<-business[,c('business_id', 'name', 'address', 'city', 'state', 'stars')]
review<-stream_in(file('./data/review_Gym.json'), pagesize = 100)
single<-read.csv('./data/single_TOP100.csv')
bigram<-read.csv('./data/bigram_TOP100.csv')
trigram<-read.csv('./data/trigram_TOP50.csv')
comm<-read.csv('./data/comments.csv')
#===============server================
shinyServer(function(input, output, session) {
#==============1=================   
  output$RawData <- DT::renderDataTable(
    DT::datatable({
      datos
    },
    options = list(lengthMenu=list(c(5,15,20,50),c('5','15','20','50')),pageLength=10,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': 'rgba(218,35,15,0.2)', 'color': '1c1b1b'});",
                     "}"),
                   columnDefs=list(list(className='dt-center',targets="_all"))
                   ),
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE,
    colnames = names(datos)
    ))
  
  Trans <- reactive({
    if(input$id_type == 'Single'){star_df=single}
    else if(input$id_type == 'Bigram'){star_df=bigram}
    else {star_df=trigram}
    star_df[,-1]
  })
  
  
  output$Bargrama <- renderPlot({
    par(mfrow=c(4,6))
    for (i in input$Number:(input$Number+23)){
      barplot(Trans()[,i], ylab='Word Freq', xlab = 'Stars', cex.main=1.8,cex.axis=1.5,cex.lab=1.5, main=names(Trans())[i],ylim=c(0,max(Trans()[,i])+0.1))
    }
  },height = 750, width = 1350)
  
#==================2=====================
#=====map=====  
  standIcons <- iconList(
    black = makeIcon("marker-icon-black.png"),
    blue = makeIcon("marker-icon-blue.png"))
  
  observe({
    city <- if (is.null(input$state)) character(0) else {
      filter(business, state %in% input$state) %>%
        `$`('city') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$city[input$city %in% city])
    updateSelectizeInput(session, "city", choices = city,
                         selected = stillSelected, server = TRUE)
  })
  
  observe({
    address <- if (is.null(input$state)) character(0) else {
      business %>%
        filter(state %in% input$state,
               is.null(input$city) | city %in% input$city) %>%
        `$`('address') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$address[input$address %in% address])
    updateSelectizeInput(session, "address", choices = address,
                         selected = stillSelected, server = TRUE)
  })
  
  output$mymap <- renderLeaflet({
    df <- business %>%
      filter(
        is.null(input$state) | state %in% input$state,
        is.null(input$city) | city %in% input$city,
        is.null(input$address) | address %in% input$address
      )
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng=df$longitude,
                 lat=df$latitude,
                 label = df$name,
                 popup = df$newaddress, 
                 icon=standIcons[df$status])
  })
  
  output$mymap1 <- renderLeaflet({
    df <- business %>%
      filter(
        is.null(input$name) | name %in% input$name
      )
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng=df$longitude,
                 lat=df$latitude,
                 label = df$name,
                 popup = df$newaddress, 
                 icon=standIcons[df$status])
  })
  
#====================================================================
#====ratings=====
  observe({
    newaddress <- if (is.null(input$newname)) character(0) else {
      filter(business, name %in% input$newname) %>%
        `$`('newaddress') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$newaddress[input$newaddress %in% newaddress])
    updateSelectizeInput(session, "newaddress", choices = newaddress,
                         selected = stillSelected,
                         server = TRUE)
    })
#==============
  Trans1 <- reactive({
    business%>%left_join(radardata,by=c('business_id'='business_id'))%>%
      filter(newaddress==input$newaddress)
  })
  #rating

  output$rating <- renderImage({
    if (is.null(input$newaddress))
      return(NULL)
    else {
      filename <- normalizePath(file.path('./www',
                  paste('star', Trans1()%>%select(star_gym), '.png', sep='')))
      return(list(
        src = filename,
        contentType = "image/png",
        width=200,
        height=40,
        alt = "rating"
        )
      )
    }
  }, deleteFile = FALSE)
  
  output$fire <- renderImage({
    if (is.null(input$newaddress))
      return(NULL)
    else {
      filename <- normalizePath(file.path('./www',
                                          paste('fire', Trans1()%>%select(popularity), '.png', sep='')))
      return(list(
        src = filename,
        contentType = "image/png",
        width=200,
        height=40,
        alt = "fire"
      )
      )
    }
  }, deleteFile = FALSE)
  
  
  #radar
  output$radarplot <- renderPlot({
    rd<-rbind(rep(1,6),rep(-1,6),Trans1()%>%select(class,equipment,service,facility,price))
    if(sum(is.na(rd))) rd<-rd[,-which(is.na(rd[3,]))]
    if(ncol(rd)<3) {
      temp<-data.frame(score=rd[3,]%>%as.numeric(),name=names(rd))
      ggplot(data=temp, aes(x=name, y=score, fill=name)) +
        geom_bar(stat="identity", width=0.5)+ ylim(-1, 5)
    }
    else{
    radarchart( rd, axistype=1 , 
                #custom polygon
                pcol=rgb(0.855,0.13,0.06,0.9) , pfcol=rgb(0.855,0.13,0.06,0.5) , plwd=4 , 
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(-1,1,0.5), cglwd=1.2,
                #custom labels
                vlcex=1.5
    )
      }
    },height = 400, width = 600)

  output$attri <- renderText({
    if (input$newaddress == '') 'You select nothing'
    else{
      temp<-Trans1()%>%select(dogs,wifi,parking)
      if(is.na(temp)%>%sum()){
        paste(names(temp)[-which(is.na(temp))],':',temp[,-which(is.na(temp))], sep=' ', collapse = '\n')
      }
      else
        paste(names(temp),':',temp, sep = ' ',collapse = '\n')
    }
  })
  
  #suggestion
  Trans2 <- reactive({
    business%>%left_join(comm,by=c('business_id'='business_id'))%>%
      filter(newaddress==input$newaddress)
  })
  
  output$suggest_pros <- renderText({
    if (input$newaddress == '') 'You select nothing' 
    else Trans2()$Pros
      })
  output$suggest_cons <- renderText({
    Trans2()$Cons
  })
  
  
  })#end















