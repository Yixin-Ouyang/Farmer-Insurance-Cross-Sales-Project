library(ggplot2)
library(ggalt)
library(ggthemes)
library(dplyr)
library(stringr)
library(shiny)
library(shinydashboard)


data = readRDS(file = "Dashboard_data.rds")

ui = dashboardPage(
  
  skin = 'black',
  
  dashboardHeader(title = 'Farmers Insurance'),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("SummaryData", tabName = "SummaryData", icon = icon("dashboard")),
      menuItem("Correlation", tabName = "Correlation", icon = icon("th")),
      menuItem("Segmentation", tabName = "Segmentation", icon = icon("th"))
    )
      
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "SummaryData",
        
        fluidRow(
          infoBox("# of customers in Arizona","145587" ,icon = icon("credit-card"), color = 'light-blue'),
          infoBox('72%', "Bought Home/Auto together", icon = icon("thumbs-up", lib = "glyphicon"),color = 'light-blue'),
          infoBox("45", "Different Attributes", icon = icon("list"), color = 'light-blue')
          
        ),
        fluidRow(
            box(
              title = "Distribution of variables",width = 6,status = "primary",
              selectInput(
                inputId = "var1", 
                label = HTML("Below are distributions of different variables, 
                             you can choose the variable you want to see. 
                             <br />For numeric variables, we show a histogram.<br />
                             For categorical variables, we show a horizontal bar chart of different categories. 
                             If the x axis is too crowded, we use a vertical bar chart."), 
                choices = c("Whether purchased Home/Auto insurance together"="AH_DISC_IND",
                            "Fire peril"="FIRE_CS_CD",
                            "Premuim charged"="PREM",
                            "Age of head of the household"="HEAD_HH_AGE",
                            "Type of garage"="GRG_DESC",
                            "Occupation type"="FPPS_OCCUP_CD",
                            "Electronic policy"="EPLCY_IND",
                            "Claim Free Discount"="CLM_FREE_DISC_IND",
                            "Square foot of living area"="LVNG_AREA_SQRFT",
                            "Risk factor"="Risk.factor")),

              plotOutput(outputId = "sum_left")),

            box(
              title = "Choropleth map",width = 6,status = "primary",
              selectInput(inputId = "group", 
                          label = HTML("Below are Choropleth maps, showing the number of customers in each zipcode. 
                                       The darker the color, the more customers in a particular region.
                                       <br />You can also choose the age group that you want to see, 
                                       such as age smaller than 49, age between 49 and 70 and age larger than 70."), 
                          choices = list("All groups","Group1","Group2","Group3","Group4",
                                         "Group5","Group6","Group7","Group8","Group9")),
           plotOutput(outputId = "sum_right"))
          
        )
      ),
     
      tabItem(tabName = "Correlation",
        fluidRow(
                 box(
                   title = "Correlation between varibales and cross-sale rate",width = 6,
                   selectInput(
                     inputId = "var3", 
                     label = HTML("Below bar chart shows the correlation between various variables and the ratio of people who purchased Home/Auto insurance together. (i.e. the cross-sale ratio)<br />You can choose the variable that you want to see using the drop-down menu. "),
                     choices = c(
                                 "Fire peril"="FIRE_CS_CD",
                                 "Age of head of the household"="HEAD_HH_AGE",
                                 "Type of garage"="GRG_DESC",
                                 "Occupation type"="FPPS_OCCUP_CD",
                                 "Electronic policy"="EPLCY_IND",
                                 "Claim Free Discount"="CLM_FREE_DISC_IND",
                                 "Risk factor"="Risk.factor")),
                 selectInput(
                   inputId = "y_var",label = '',
                   choices = c('Number of people purchased Home/Auto together'='yes',
                               'Percentage of people purchased Home/Auto together' ='ratio')),
                 
                 plotOutput(outputId = "corel_left")
          ),
        
                 box(
                   title = "Correlation between premium charged and cross-sale rate",width = 6,
                   sliderInput(inputId = "var2", 
                               label = HTML("Below bar chart shows the correlation between premium charged and the ratio of people who purchased Home/Auto insurance together. <br />You can choose the range of premium that you want to see using the slider bar. The darker the color, the higher the cross-sale ratio. "),
                               min = 0, max = 12000, value = c(0, 2000)),
                   selectInput(
                     inputId = "y_var2",label = '',
                     choices = c('Number of people purchased Home/Auto together'='yes',
                                 'Percentage of people purchased Home/Auto together' ='ratio')),
           
                 plotOutput(outputId = "corel_right"))
          )
      ),
      
      tabItem(tabName = "Segmentation",
        fluidRow(
          box(
            title = "Customer Segmentation",
            selectInput(inputId = "seg_x", 
                        label = "", 
                        choices = list("All groups","Group1","Group2","Group3","Group4",
                                                 "Group5","Group6","Group7","Group8","Group9")),
            textOutput(outputId = 'description')),
            
            box(plotOutput(outputId = "segmentation"))
          )
      )
    )
  )
)


server = function(input,output){

  output$sum_left = renderPlot({
    if (input$var1 %in% c("PREM","LVNG_AREA_SQRFT","HEAD_HH_AGE")) {
      ggplot(data,aes_string(x= input$var1)) +
        geom_histogram(fill = 'White',color = 'black')+
        ylab('number of people')+
        theme(panel.background = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.border = element_blank(),
              axis.title = element_blank(), 
              axis.ticks = element_blank()) 
      
    } else if (input$var1 == "GRG_DESC"){
      ggplot(data,aes_string(x= input$var1)) +
        geom_bar(fill = 'White',color = 'black',width =0.4 )+
        coord_flip()+
        ylab('number of people')+
        theme(panel.background = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.border = element_blank(),
              axis.title = element_blank(), 
              axis.ticks = element_blank())
    } else {
      ggplot(data,aes_string(x= input$var1)) +
        geom_bar(fill = 'White',color = 'black',width = 0.4)+
        ylab('number of people')+
        theme(panel.background = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.border = element_blank(),
              axis.title = element_blank(), 
              axis.ticks = element_blank())
    }
  
  })
  
  output$sum_right <- renderPlot({
    mymap = readRDS("AZ_map.RDS")
    
    if (input$group=="All groups") {
      subData = data
    } else if (input$group=="Group1") {
      subData = filter(data,HEAD_HH_AGE>=49 & Risk.factor>=3)
    } else if (input$group=="Group2") {
      subData = filter(data, HEAD_HH_AGE>=28 & HEAD_HH_AGE<49 & Risk.factor>=4)
    } else if (input$group=="Group3") {
      subData = filter(data, HEAD_HH_AGE>=70 & PREM<733.64 & Risk.factor==2)
    } else if (input$group=="Group4") {
      subData = filter(data, HEAD_HH_AGE>50 & HEAD_HH_AGE<70 & PREM<733.64 & CLM_FREE_DISC_IND=="N")
    } else if (input$group=="Group5") {
      subData = filter(data, HEAD_HH_AGE>50 & HEAD_HH_AGE<70 & PREM<733.64 & CLM_FREE_DISC_IND=="Y" & FPPS_OCCUP_CD%in%c(5,6))
    } else if (input$group=="Group6") {
      subData = filter(data, HEAD_HH_AGE>=37 & HEAD_HH_AGE<=50)
    } else if (input$group=="Group7") {
      subData = filter(data, HEAD_HH_AGE>=27 & HEAD_HH_AGE<=37)
    } else if (input$group=="Group8") {
      subData = filter(data, HEAD_HH_AGE<27 & PREM<517)
    } else if (input$group=="Group9") {
      subData = filter(data, HEAD_HH_AGE<27 & PREM>517)
    } 
    df = subData %>%
      group_by(PROP_ZIP_CD) %>%
      summarise(count = n())
    
    ggplot() +
      geom_cartogram(data = df, aes(fill = count, map_id = PROP_ZIP_CD), map = mymap) +
      scale_fill_gradient2(low = "white", high = "steel blue") +
      borders("state",regions="arizona", colour = "lightgrey") +
      coord_map() +
      theme_map() +
      theme(legend.position="left")+
      labs(fill = 'Number of Customers')
    
  })
  

  output$corel_left <- renderPlot({
    data3 = data %>%
      group_by_(input$var3) %>%
      summarise(count = n(), yes = sum(AH_DISC_IND=="Y"), no = sum(AH_DISC_IND=="N"))
    
    data3$ratio = data3$yes / data3$count
    
    ggplot(data3, aes_string(x=input$var3, y=input$y_var)) +
      geom_col(fill = 'white',color = 'black')+
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.border = element_blank(),
            axis.title = element_blank(), 
            axis.ticks = element_blank())
      
  })  
  
  output$corel_right <- renderPlot({
    
    data$premium= round(data$PREM,0)
    
    data2 = data %>%
      filter(premium >= input$var2[1] & premium <= input$var2[2]) %>%
      group_by(premium)%>%
      summarise(count = n(),yes = sum(AH_DISC_IND=="Y"),no = sum(AH_DISC_IND=="N"))
    
    data2$ratio = data2$yes / data2$count
    
    ggplot(data2, aes(x=premium, y=get(input$y_var2))) +
      geom_point(color = 'steel blue')+
      xlab('$ of premium')+
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.border = element_blank(),
            axis.title = element_blank(), 
            axis.ticks = element_blank())
  })
  
  output$segmentation <- renderPlot({
    seg = read.csv("Segmentation.csv")
    ggplot(seg,aes(x=reorder(Group,Home.Auto.ratio),y=Home.Auto.ratio))+
      geom_col( color = 'black',fill = 'white',width = 0.4)+
      geom_col(data=seg[c(2,5,8,12), ], aes(x=reorder(Group,Home.Auto.ratio), y=Home.Auto.ratio), fill="steel blue",color = 'black',width = 0.4)+
      coord_flip()+
      ylab('Home/Auto ratio')+
      xlab('Groups')+
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.border = element_blank(), 
            axis.ticks = element_blank())
 
  })
  
  output$description <- renderText({
    if (input$seg_x=="All groups") {
      'subData = data'
    } else if (input$seg_x=="Group1") {
      'subData = filter(data,HEAD_HH_AGE>=49 & Risk.factor>=3)'
    } else if (input$seg_x=="Group2") {
      'subData = filter(data, HEAD_HH_AGE>=28 & HEAD_HH_AGE<49 & Risk.factor>=4)'
    } else if (input$seg_x=="Group3") {
      'subData = filter(data, HEAD_HH_AGE>=70 & PREM<733.64 & Risk.factor==2)'
    } else if (input$seg_x=="Group4") {
      'subData = filter(data, HEAD_HH_AGE>50 & HEAD_HH_AGE<70 & PREM<733.64 & CLM_FREE_DISC_IND=="N")'
    } else if (input$seg_x=="Group5") {
      'subData = filter(data, HEAD_HH_AGE>50 & HEAD_HH_AGE<70 & PREM<733.64 & CLM_FREE_DISC_IND=="Y" & FPPS_OCCUP_CD%in%c(5,6))'
    } else if (input$seg_x=="Group6") {
      'subData = filter(data, HEAD_HH_AGE>=37 & HEAD_HH_AGE<=50)'
    } else if (input$seg_x=="Group7") {
      'subData = filter(data, HEAD_HH_AGE>=27 & HEAD_HH_AGE<=37)'
    } else if (input$seg_x=="Group8") {
      'subData = filter(data, HEAD_HH_AGE<27 & PREM<517)'
    } else if (input$seg_x=="Group9") {
      'subData = filter(data, HEAD_HH_AGE<27 & PREM>517)'
    } 
    
    
  })
  
  
}

shinyApp(ui,server)