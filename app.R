library(shiny)
library(shinydashboard)
library(lubridate)
library(ggplot2)
library(dplyr)
library(DT)
library(gridExtra)
library(scales)

temp = list.files(pattern="*.tsv")
allData <- lapply(temp,read.delim)
cta <- do.call(rbind, allData)

cta$date<-mdy(cta$date)
names(cta)[1]<-"stationId"

ui <- dashboardPage(
  dashboardHeader(title="CS 424 Project 1"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
      menuItem("", tabName = "cheapBlankSpace"),
    sidebarMenu(menuItem("About Page", tabName = "dashboard")),
    sidebarMenu(menuItem("Main Page", tabName = "widgets",selected = TRUE)),
    sidebarMenu(menuItem("Interesting Dates", tabName = "dates")))),
  
  dashboardBody(
    tabItems(tabItem(tabName = "dashboard",
                     h2("About Page"),
                     p("The data is collected from this website 
                       https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f"),
                     p("Chicago Transit Authority published this data and it contains 
                       how many people have taken trains or buses at every stop. 
                       Data is collected from 2000 to 2021."),
                     p("This app is written by Abhijeet Chintakunta as part of course project")),
    tabItem(tabName ="widgets",
    fluidRow(
      column(6,
             fluidRow(plotOutput("barplot")),
             fluidRow(selectInput(inputId="stationname",label="Select the station name",c("UIC-Halsted","O'Hare Airport","Racine"),selected="UIC-Halsted"))),
      column(6,
             fluidRow(plotOutput("barplot1")),
             fluidRow(selectInput(inputId="stationname1",label="Select the station name",c("UIC-Halsted","O'Hare Airport","Racine"),selected="O'Hare Airport"))),
      ),
    fluidRow(
      column(6,
             fluidRow(selectInput(inputId="year",label="Select the year",c(2001:2021),selected=2021)),
             fluidRow(selectInput(inputId="plottype",label="Select the plot type",c("Daily","Monthly","Weekly","Select all"),selected="Daily")),
             fluidRow(plotOutput("barplotdmw"))),
      column(6,
             fluidRow(selectInput(inputId="year1",label="Select the year",c(2001:2021),selected=2021)),
             fluidRow(selectInput(inputId="plottype1",label="Select the plot type",c("Daily","Monthly","Weekly","Select all"),selected="Daily")),
             fluidRow(plotOutput("barplotdmw1"))),
      ),
    fluidRow(
            column(6,
                     box(width = 12,
                       dataTableOutput("table1",height=100))),
            column(6,
                   box(width = 12,
                       dataTableOutput("table2", height =100))),
    )
    ),
    tabItem(tabName = "dates",
            fluidRow(
                     plotOutput("barplotdate"),
                     textOutput("summary",container = div),
                     selectInput(inputId="Interesting_Dates",label="Select a Date",
                                 c("March 20th 2020","September 28th 2019","November 4th 2008",
                                   "October 28th 2016","November 28th 2019","March 16th 2019",
                                   "November 11th 2019","July 16th 2004","February 2nd 2011",
                                   "December 25th 2019"),
                                 selected="March 20th 2020"),
                     dataTableOutput("datatable")
                     ))
    )
    ))
    
            

server <- function(input, output) { 
  #UIC-Halsted Data
  data<-reactive({subset(cta,cta$stationname==input$stationname)})
  stationname <- reactive({
    if(input$stationname=="UIC-Halsted"){
      stationname <- "Yearly entries at UIC Halsted"
  }
  else if (input$stationname=="Racine"){
    stationname <- "Yearly entries at Racine"
  }
  else if (input$stationname=="O'Hare Airport"){
      stationname <- "Yearly entries at O'Hare Airport"
  }
  })
  
  output$barplot<-renderPlot({
    ggplot(data(),aes(x=year(date),y=rides))+geom_bar(stat="identity",fill="steelblue")+
      labs(title = stationname() , x = "Year", y ="Entries")+
      scale_y_continuous(labels = comma)+scale_x_continuous(breaks=c(2000:2021))
  })
  
  cta4<-reactive({subset(data(),year(date)==input$year)})
  
  stationname1 <- reactive({
    if(input$stationname=="UIC-Halsted"){
      if(input$plottype=="Monthly"){
        stationname1 <- "Monthly entries at UIC Halsted"
      }
      else if (input$plottype=="Weekly"){
        stationname1 <- "Weekly entries at UIC Halsted"
      }
      else if (input$plottype=="Daily"){
        stationname1 <- "Day entries at UIC Halsted"
      }
      else if (input$plottype =="Select All"){
        stationname1 <- "Entries at UIC Halsted"
      }
    }
    else if (input$stationname=="Racine"){
      if(input$plottype=="Monthly"){
        stationname1 <- "Monthly entries at Racine"
      }
      else if (input$plottype=="Weekly"){
        stationname1 <- "Weekly entries at Racine"
      }
      else if (input$plottype=="Daily"){
        stationname1 <- "Day entries at Racine"
      }
      else if (input$plottype =="Select All"){
        stationname1 <- "Entries at Racine"
      }
    }
    else if (input$stationname=="O'Hare Airport"){
      if(input$plottype=="Monthly"){
        stationname1 <- "Monthly entries at O'Hare Airport"
      }
      else if (input$plottype=="Weekly"){
        stationname1 <- "Weekly entries at O'Hare Airport"
      }
      else if (input$plottype=="Daily"){
        stationname1 <- "Day entries at O'Hare Airport"
      }
      else if (input$plottype =="Select All"){
        stationname1 <- "Entries at O'Hare Aiport"
      }
    }
  })
  
  # output$table<-renderDataTable(cta1())
  observeEvent(input$plottype,if (input$plottype=="Monthly"){
    output$barplotdmw<-renderPlot({
      ggplot(cta4(),aes(x=month(date,label=TRUE,abbr=TRUE),y=rides))+geom_bar(stat="identity", fill="steelblue")+labs(title = stationname1(), x = "Month", y ="Entries")+scale_y_continuous(labels = comma)})
    cta7 <- reactive({cta4() %>% group_by(month(date)) %>% summarize(Total_No_of_Rides=sum(rides))})
    output$table1 <- DT::renderDataTable({cta7()},rownames=FALSE,options=list(pageLength=7),colnames = c("Month","Entries")) 
  }
  else if (input$plottype=="Weekly"){
    output$barplotdmw<-renderPlot({ggplot(cta4(),aes(x=wday(date,label=TRUE,abbr=TRUE),y=rides))+geom_bar(stat="identity", fill="steelblue")+labs(title = stationname1(), x = "WeekDay", y ="Entries")+scale_y_continuous(labels = comma)})
    cta7 <- reactive({cta4() %>% group_by(wday(date)) %>% summarize(Total_No_of_Rides=sum(rides))})
    
    output$table1 <- DT::renderDataTable({cta7()},rownames=FALSE,options=list(pageLength=7),colnames = c("Weekday","Entries")) 
  }
  else if (input$plottype=="Daily"){
    output$barplotdmw<-renderPlot({ggplot(cta4(),aes(x=date(date),y=rides))+geom_bar(stat="identity", fill="steelblue")+labs(title = stationname1(), x = "Day", y ="Entries")+
        scale_y_continuous(labels = comma)+scale_x_date(date_breaks = "1 month",date_labels="%b")})
    output$table1 <- DT::renderDataTable({select(cta4(),date,rides)},rownames=FALSE,options=list(pageLength=7),colnames = c("Date","Entries")) 
  }
  else if (input$plottype=="Select all"){
    output$barplotdmw<-renderPlot({
      p1<-ggplot(cta4(),aes(x=date(date),y=rides))+geom_bar(stat="identity", fill="steelblue")+labs(x = "Day", y ="Entries")+scale_y_continuous(labels = comma)
      p3<-ggplot(cta4(),aes(x=month(date,label=TRUE,abbr=TRUE),y=rides))+geom_bar(stat="identity", fill="steelblue")+labs(x = "Month", y ="Entries")+scale_y_continuous(labels = comma)
      p2<-ggplot(cta4(),aes(x=wday(date,label=TRUE,abbr=TRUE),y=rides))+geom_bar(stat="identity", fill="steelblue")+labs(x = "WeekDay", y ="Entries")+scale_y_continuous(labels = comma)
      grid.arrange(p1,p2,p3)
    })
  })
  
  
  
  
  # Second Region Output Code
  # O'Hare Data
  data1<-reactive({cta2<-subset(cta,cta$stationname==input$stationname1)})
  
  stationname2 <- reactive({
    if(input$stationname1=="UIC-Halsted"){
      stationname2 <- "Yearly entries at UIC Halsted"
    }
    else if (input$stationname1=="Racine"){
      stationname2 <- "Yearly entries at Racine"
    }
    else if (input$stationname1=="O'Hare Airport"){
      stationname2 <- "Yearly entries at O'Hare Airport"
    }
  })
  
  output$barplot1<-renderPlot({
    ggplot(data1(),aes(x=year(date),y=rides))+geom_bar(stat="identity",fill="steelblue")+
      labs(title = stationname2(), x = "Year", y ="Entries")+
      scale_y_continuous(labels = comma)+scale_x_continuous(breaks = c(2000:2021))
  })
  
  cta5<-reactive({subset(data1(),year(date)==input$year1)})
  
  stationname3 <- reactive({
    if(input$stationname1=="UIC-Halsted"){
      if(input$plottype1=="Monthly"){
      stationname3 <- "Monthly entries at UIC Halsted"
      }
      else if (input$plottype1=="Weekly"){
        stationname3 <- "Weekly entries at UIC Halsted"
      }
      else if (input$plottype1=="Daily"){
        stationname3 <- "Day entries at UIC Halsted"
      }
    }
    else if (input$stationname1=="Racine"){
      if(input$plottype1=="Monthly"){
        stationname3 <- "Monthly entries at Racine"
      }
      else if (input$plottype1=="Weekly"){
        stationname3 <- "Weekly entries at Racine"
      }
      else if (input$plottype1=="Daily"){
        stationname3 <- "Day entries at Racine"
      }
    }
    else if (input$stationname1=="O'Hare Airport"){
      if(input$plottype1=="Monthly"){
        stationname3 <- "Monthly entries at O'Hare Airport"
      }
      else if (input$plottype1=="Weekly"){
        stationname3 <- "Weekly entries at O'Hare Airport"
      }
      else if (input$plottype1=="Daily"){
        stationname3 <- "Day entries at O'Hare Airport"
      }
    }
  })
  
  observeEvent(input$plottype1,if (input$plottype1=="Monthly"){
    output$barplotdmw1<-renderPlot({
      ggplot(cta5(),aes(x=month(date,label=TRUE,abbr=TRUE),y=rides))+geom_bar(stat="identity", fill="steelblue")+labs(title = stationname3(), x = "Month", y ="Entries")+scale_y_continuous(labels = comma)})
    cta6 <- reactive({cta5() %>% group_by(month(date)) %>% summarize(Total_No_of_Rides=sum(rides))})
    output$table2 <- DT::renderDataTable({cta6()},rownames=FALSE,options=list(pageLength=7),colnames = c("Month","Entries")) 
  }
  else if (input$plottype1=="Weekly"){
    output$barplotdmw1<-renderPlot({ggplot(cta5(),aes(x=wday(date,label=TRUE,abbr=TRUE),y=rides))+geom_bar(stat="identity", fill="steelblue")+labs(title = stationname3(), x = "WeekDay", y ="Entries")+scale_y_continuous(labels = comma)})
    cta6 <- reactive({cta5() %>% group_by(wday(date)) %>% summarize(Total_No_of_Rides=sum(rides))})
    output$table2 <- DT::renderDataTable({cta6()},rownames=FALSE,options=list(pageLength=7),colnames = c("Weekday","Entries")) 
  }
  else if (input$plottype1=="Daily"){
    output$barplotdmw1<-renderPlot({ggplot(cta5(),aes(x=date(date),y=rides))+geom_bar(stat="identity", fill="steelblue")+labs(title = stationname3(), x = "Day", y ="Entries")+scale_y_continuous(labels = comma)+scale_x_date(date_breaks = "1 month",date_labels="%b")})
    output$table2 <- DT::renderDataTable({select(cta5(),date,rides)},rownames=FALSE,options=list(pageLength=7),colnames = c("Date","Entries")) 
  }
  else if (input$plottype1=="Select all"){
    output$barplotdmw1<-renderPlot({
      p1<-ggplot(cta5(),aes(x=date(date),y=rides))+geom_bar(stat="identity", fill="steelblue")+ labs(x = "Day", y ="Entries")+scale_y_continuous(labels = comma)
      p2<-ggplot(cta5(),aes(x=month(date,label=TRUE,abbr=TRUE),y=rides))+geom_bar(stat="identity", fill="steelblue")+labs(x = "Month", y ="Entries")+scale_y_continuous(labels = comma)
      p3<-ggplot(cta5(),aes(x=wday(date,label=TRUE,abbr=TRUE),y=rides))+geom_bar(stat="identity", fill="steelblue")+labs(x = "WeekDay", y ="Entries")+scale_y_continuous(labels = comma)
      grid.arrange(p1,p2,p3)
      })
  })
  
  observeEvent(input$Interesting_Dates,
               if (input$Interesting_Dates=="March 20th 2020"){
    datesdata<-reactive({cta7<-subset(cta,cta$stationname=="O'Hare Airport")})
    output$summary <- renderText({"Covid lockdown was announced in Illinois"})
    cta8<-reactive({subset(datesdata(),year(date)==2020)})
    output$barplotdate<-renderPlot(
      {ggplot(cta8(),aes(x=date(date),y=rides))+geom_bar(stat="identity", fill="steelblue")+
          labs(title = "", x = "Day", y ="Entries")+scale_y_continuous(labels = comma)+
          scale_x_date(limit=c(as.Date("2020-03-09"),as.Date("2020-04-09")),
                       date_breaks = "2 day",date_labels = "%b %d")})
    cta9<- reactive({subset(cta8(), date > "2020-03-09" & date < "2020-04-09")})
    output$datatable <- DT::renderDataTable({select(cta9(),date,rides)},
                                            rownames=FALSE,
                                            options=list(search = list(regex = TRUE, caseInsensitive = FALSE, search = ""),
                                                         pageLength=20))
  }
  else if (input$Interesting_Dates=="September 28th 2019"){
      datesdata<-reactive({cta7<-subset(cta,cta$stationname=="O'Hare Airport")})
      output$summary <- renderText({"Blue line closed its operations for 10 days"})
      cta8<-reactive({subset(datesdata(),year(date)==2019)})
      output$barplotdate<-renderPlot(
        {ggplot(cta8(),aes(x=date(date),y=rides))+geom_bar(stat="identity", fill="steelblue")+
            labs(title = "", x = "Day", y ="Entries")+scale_y_continuous(labels = comma)+
            scale_x_date(limit=c(as.Date("2019-09-15"),as.Date("2019-10-15")),
                         date_breaks = "2 day",date_labels = "%b %d")})
      cta9<- reactive({subset(cta8(), date > "2019-09-15" & date < "2019-10-15")})
      output$datatable <- DT::renderDataTable({select(cta9(),date,rides)},
                                              rownames=FALSE,
                                              options=list(search = list(regex = TRUE, caseInsensitive = FALSE, search = ''),
                                                           pageLength=20))
  }
  else if (input$Interesting_Dates=="November 4th 2008"){
    datesdata<-reactive({cta7<-subset(cta,cta$stationname=="Harrison")})
    output$summary <- renderText({"Obama's Speech at Grant Park the data presented is of Harrison
                                which is close to Grant park station"})
    cta8<-reactive({subset(datesdata(),year(date)==2008)})
    output$barplotdate<-renderPlot(
      {ggplot(cta8(),aes(x=date(date),y=rides))+geom_bar(stat="identity", fill="steelblue")+
          labs(title = "", x = "Day", y ="Entries")+scale_y_continuous(labels = comma)+
          scale_x_date(limit=c(as.Date("2008-10-15"),as.Date("2008-11-10")),
                       date_breaks = "2 day",date_labels = "%b %d")})
    cta9<- reactive({subset(cta8(), date > "2008-10-15" & date < "2008-11-10")})
    output$datatable <- DT::renderDataTable({select(cta9(),date,rides)},
                                            rownames=FALSE,
                                            options=list(search = list(regex = TRUE, caseInsensitive = FALSE, search = ''),
                                                         pageLength=20))
  }
  else if (input$Interesting_Dates=="October 28th 2016"){
    datesdata<-reactive({cta7<-subset(cta,cta$stationname=="Sheridan")})
    output$summary <- renderText({"World Series at Wrigley Field the data is displayed for Sheridan station which is close to Wrigley field"})
    cta8<-reactive({subset(datesdata(),year(date)==2016)})
    output$barplotdate<-renderPlot(
      {ggplot(datesdata(),aes(x=date(date),y=rides))+geom_bar(stat="identity", fill="steelblue")+
          labs(title = "", x = "Day", y ="Entries")+scale_y_continuous(labels = comma)+
        scale_x_date(limit=c(as.Date("2016-10-10"),as.Date("2016-10-31")),
                     date_breaks = "2 day",date_labels = "%b %d")})
    cta9<- reactive({subset(cta8(), date > "2016-10-15" & date < "2016-10-31")})
    output$datatable <- DT::renderDataTable({select(cta9(),date,rides)},
                                            rownames=FALSE,
                                            options=list(search = list(regex = TRUE, caseInsensitive = FALSE, search = ''),
                                                         pageLength=20))
  }
  else if (input$Interesting_Dates=="November 28th 2019"){
    datesdata<-reactive({cta7<-subset(cta,cta$stationname=="O'Hare Airport")})
    output$summary <- renderText({"Thanksgiving weekend the data shown is of O'Hare Airport"})
    cta8<-reactive({subset(datesdata(),year(date)==2019)})
    output$barplotdate<-renderPlot(
      {ggplot(datesdata(),aes(x=date(date),y=rides))+geom_bar(stat="identity", fill="steelblue")+
          labs(title = "", x = "Day", y ="Entries")+scale_y_continuous(labels = comma)+
          scale_x_date(limit=c(as.Date("2019-11-15"),as.Date("2019-11-30")),
                       date_breaks = "2 day",date_labels = "%b %d")})
    cta9<- reactive({subset(cta8(), date > "2019-11-15" & date < "2019-11-30")})
    output$datatable <- DT::renderDataTable({select(cta9(),date,rides)},
                                            rownames=FALSE,
                                            options=list(search = list(regex = TRUE, caseInsensitive = FALSE, search = ''),
                                                         pageLength=20))
  }
  else if (input$Interesting_Dates=="March 16th 2019"){
    datesdata<-reactive({cta7<-subset(cta,cta$stationname=="State/Lake")})
    output$summary <- renderText({"St Patricks Day Parade and Chicago River Dying the data shown is of State/Lake station which is close to the parade"})
    cta8<-reactive({subset(datesdata(),year(date)==2019)})
    output$barplotdate<-renderPlot(
      {ggplot(datesdata(),aes(x=date(date),y=rides))+geom_bar(stat="identity", fill="steelblue")+
          labs(title = "", x = "Day", y ="Entries")+scale_y_continuous(labels = comma)+
          scale_x_date(limit=c(as.Date("2019-03-01"),as.Date("2019-03-20")),
                       date_breaks = "2 day",date_labels = "%b %d")})
    cta9<- reactive({subset(cta8(), date > "2019-03-01" & date < "2019-03-20")})
    output$datatable <- DT::renderDataTable({select(cta9(),date,rides)},
                                            rownames=FALSE,
                                            options=list(search = list(regex = TRUE, caseInsensitive = FALSE, search = ''),
                                                         pageLength=20))
  }
  else if (input$Interesting_Dates=="July 16th 2004"){
    datesdata<-reactive({cta7<-subset(cta,cta$stationname=="Randolph/Wabash")})
    output$summary <- renderText({"Millenium Park opened on this date and the data is of Randolph/Wabash which is the station closest to it"})
    cta8<-reactive({subset(datesdata(),year(date)==2004)})
    output$barplotdate<-renderPlot(
      {ggplot(datesdata(),aes(x=date(date),y=rides))+geom_bar(stat="identity", fill="steelblue")+
          labs(title = "", x = "Day", y ="Entries")+scale_y_continuous(labels = comma)+
          scale_x_date(limit=c(as.Date("2004-07-01"),as.Date("2004-07-30")),
                       date_breaks = "2 day",date_labels = "%b %d")})
    cta9<- reactive({subset(cta8(), date >= "2004-07-01" & date <= "2004-07-30")})
    output$datatable <- DT::renderDataTable({select(cta9(),date,rides)},
                                            rownames=FALSE,
                                            options=list(search = list(regex = TRUE, caseInsensitive = FALSE, search = ''),
                                                         pageLength=20))
  }
  else if (input$Interesting_Dates=="February 2nd 2011"){
    datesdata<-reactive({cta7<-subset(cta,cta$stationname=="O'Hare Airport")})
    output$summary <- renderText({"GroundHog Day and the data shown is O'Hare Airport data"})
    cta8<-reactive({subset(datesdata(),year(date)==2011)})
    output$barplotdate<-renderPlot(
      {ggplot(datesdata(),aes(x=date(date),y=rides))+geom_bar(stat="identity", fill="steelblue")+
          labs(title = "", x = "Day", y ="Entries")+scale_y_continuous(labels = comma)+
          scale_x_date(limit=c(as.Date("2011-01-20"),as.Date("2011-02-20")),
                       date_breaks = "2 day",date_labels = "%b %d")})
    cta9<- reactive({subset(cta8(), date >= "2011-01-15" & date <= "2011-02-15")})
    output$datatable <- DT::renderDataTable({select(cta9(),date,rides)},
                                            rownames=FALSE,
                                            options=list(search = list(regex = TRUE, caseInsensitive = FALSE, search = ''),
                                                         pageLength=20))
  }
  else if (input$Interesting_Dates=="December 25th 2019"){
    datesdata<-reactive({cta7<-subset(cta,cta$stationname=="O'Hare Airport")})
    output$summary <- renderText({"Christmas and the data shown is O'Hare Airport data"})
    cta8<-reactive({subset(datesdata(),year(date)==2019 | year(date)==2020)})
    output$barplotdate<-renderPlot(
      {ggplot(datesdata(),aes(x=date(date),y=rides))+geom_bar(stat="identity", fill="steelblue")+
          labs(title = "", x = "Day", y ="Entries")+scale_y_continuous(labels = comma)+
          scale_x_date(limit=c(as.Date("2019-12-20"),as.Date("2020-01-20")),
                       date_breaks = "2 day",date_labels = "%b %d")})
    cta9<- reactive({subset(cta8(), date >= "2019-12-20" & date <= "2020-01-20")})
    output$datatable <- DT::renderDataTable({select(cta9(),date,rides)},
                                            rownames=FALSE,
                                            options=list(search = list(regex = TRUE, caseInsensitive = FALSE, search = ''),
                                                         pageLength=20))
  }
  else if (input$Interesting_Dates=="November 11th 2019"){
    datesdata<-reactive({cta7<-subset(cta,cta$stationname=="O'Hare Airport")})
    output$summary <- renderText({"9/11"})
    cta8<-reactive({subset(datesdata(),year(date)==2019)})
    output$barplotdate<-renderPlot(
      {ggplot(datesdata(),aes(x=date(date),y=rides))+geom_bar(stat="identity", fill="steelblue")+
          labs(title = "", x = "Day", y ="Entries")+scale_y_continuous(labels = comma)+
          scale_x_date(limit=c(as.Date("2019-11-01"),as.Date("2019-11-30")),
                       date_breaks = "2 day",date_labels = "%b %d")})
    cta9<- reactive({subset(cta8(), date >= "2019-11-01" & date <= "2019-11-30")})
    output$datatable <- DT::renderDataTable({select(cta9(),date,rides)},
                                            rownames=FALSE,
                                            options=list(search = list(regex = TRUE, caseInsensitive = FALSE, search = ''),
                                                         pageLength=20))
  }
  )
  }


shinyApp(ui, server)

