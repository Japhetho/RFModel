library(shiny)
library(shinydashboard)
library(raster)
library(caret)
library(randomForest)
library(mapview)
library(sf)
library(ggplot2)
library(geoR)
library(geojsonio)
library(leaflet)

mapviewOptions(basemaps = "OpenStreetMap")

ui <- dashboardPage(title = "SPATIAL MODELING SYSTEM FOR WEEDS",
                    
                    dashboardHeader(title = "My Dashboard",
                                    dropdownMenu(type = "messages",messageItem(from = "Research Team",message = "Check out this page",time = "30-5-2019"),
                                                 messageItem(from = "Administrator",message = "Your request has been granted.",time = "24-5-2019")),
                                    dropdownMenu(type = "notifications",notificationItem(text = "Payment for the previous job has been deposited in your account.",icon = icon("warning"))),
                                    dropdownMenu(type = "tasks",taskItem(value = 72,color = "aqua",text = "Digitizing progress"),
                                                 taskItem(value = 45,color = "blue",text = "Raster Download Progress"),
                                                 taskItem(value = 90,color = "green", text = "Almost done"))
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        sidebarSearchForm("SearchText","buttonSearch","search"),
                        menuItem("Home",tabName = "Home"),
                        menuItem("Spatial-Temporal Modeling",
                                 menuItem("Spatial Model",tabName = "model"),
                                 menuItem("Variable Importance",tabName = "statistics")),
                        menuItem("ScatterPlot Viewer",tabName = "scatterplot"),
                        menuItem("Contact Us",badgeLabel = "NEW",badgeColor = "green",tabName = "Jobs")
                      )),
                    dashboardBody(
                      
                      tabItems(
                        tabItem(tabName = "Home",
                                h2("WEED MODELING SYSTEM",style="text-decoration:underline;text-align:center"),
                                p("This application can be used for the spatial analysis and modeling of weeds.",style="text-align:center;color:blue")),
                        tabItem(tabName = "scatterplot",
                                h3("Select a variable to be plotted on the Y-axis:"),
                                selectInput(
                                  inputId = "y",
                                  label = "Y-axis",
                                  choices = c(
                                    "Variable Y"
                                  )),
                                h3("Select a variable to be plotted on the X-axis:"),
                                selectInput(inputId = "x",
                                            label = "X-axis",
                                            choices = c(
                                              "Variable X"
                                            )),
                                br(),
                                helpText("Click on the button below before selecting variables to be plotted."),
                                actionButton("run_scatter","View Scatterplot"),
                                plotOutput(outputId = "scatter")),
                        tabItem(tabName = "statistics",
                                h3("Click on the button below to view the importance of each variable in the model."),
                                actionButton("click","Click Here"),
                                plotOutput(outputId = "varimp")),
                        tabItem(tabName = "model",
                                h3("Select the Input explanatory variables"),
                                fileInput(inputId = "rasters",
                                          label = "Upload first raster",
                                          accept = 'tif',
                                          multiple = TRUE
                                ),
                                fileInput("rst2","Upload second raster",accept = 'tif'),
                                fileInput("rst3","Upload third raster",accept = 'tif'),
                                fileInput("rst4","Upload fourth raster",accept = 'tif'),
                                fileInput("rst5","Upload fifth raster",accept = 'tif'),
                                helpText("Note: All input variables should be of the same spatial extent and resolution."),
                                h3("Provide the Geojson file containing your dependent variable:"),
                                fileInput(inputId = "training",
                                          label = "Upload Geojson",
                                          placeholder = "Geojson file"),
                                br(),
                                textInput("dep","Write the column name containing the dependent variable:"),
                                selectInput(inputId = "type",
                                            label = "Select type of model:",
                                            choices = c(
                                              "RandomForest"="rf",
                                              "KNN"="knn")
                                ),
                                actionButton("run","Run Model"),
                                br(),
                              
                                leafletOutput(outputId = "modelvis"),
                                br(),
                                verbatimTextOutput("clas"))
                      )
                    ))

#Define server function that executes the logic behind the scenes
server <- function(input,output,session){
  
  
  observeEvent( input$run, {output$modelvis <- renderLeaflet({
    
    infile1 <- input$rasters
    infile2 <- input$rst2
    infile3 <- input$rst3
    infile4 <- input$rst4
    infile5 <- input$rst5
    newe <- input$training
    depe <- input$dep
    shp <- geojsonio::geojson_read(newe$datapath, what = "sp")
    
    if(is.null(infile1))
      return(NULL)
    
    if(is.null(infile2))
      return(NULL)
      
    data1 <- raster(infile1$datapath)
    name1 <- infile1$name
    name11 <- gsub(pattern = "\\.tif$", "", name1)
    data2 <- raster(infile2$datapath)
    name2 <- infile2$name
    name22 <- gsub(pattern = "\\.tif$", "", name2)
    data3 <- raster(infile3$datapath)
    name3 <- infile3$name
    name33 <- gsub(pattern = "\\.tif$", "", name3)
    data4 <- raster(infile4$datapath)
    name4 <- infile4$name
    name44 <- gsub(pattern = "\\.tif$", "", name4)
    data5 <- raster(infile5$datapath)
    name5 <- infile5$name
    name55 <- gsub(pattern = "\\.tif$", "", name5)
    rstack <- stack(data1,data2,data3,data4,data5)
    names(rstack)[names(rstack) == "X0.1"] <- name11
    names(rstack)[names(rstack) == "X0.2"] <- name22
    names(rstack)[names(rstack) == "X0.3"] <- name33
    names(rstack)[names(rstack) == "X0.4"] <- name44
    names(rstack)[names(rstack) == "X0.5"] <- name55
    extr <- extract(rstack,shp,df=TRUE)
    set.seed(100)
    extr <- merge(extr,shp,by.x="ID",by.y="UID")
    depe2 <- extr[,depe]
    trainids <- createDataPartition(depe2,list = FALSE,p=0.8)
    trainDat <- extr[trainids,]
    testDat <- extr[-trainids,]
    predict_variables <- c(name11,name22,name33,name44,name55)
    response_variable <- depe
    model <- train(trainDat[,predict_variables],trainDat[,response_variable],method = input$type,
                   trControl = trainControl(method = "cv", classProbs = TRUE),importance=TRUE)
    prediction <- predict(rstack,model)
    m <- mapview(prediction)
    m@map
  })
  })
  
  observeEvent(input$run, {output$clas <- renderPrint({
    infile1 <- input$rasters
    infile2 <- input$rst2
    infile3 <- input$rst3
    infile4 <- input$rst4
    infile5 <- input$rst5
    newe <- input$training
    depe <- input$dep
    shp <- geojsonio::geojson_read(newe$datapath, what = "sp")
    
    if(is.null(infile1))
      return(NULL)
    
    if(is.null(infile2))
      return(NULL)
    
    data1 <- raster(infile1$datapath)
    name1 <- infile1$name
    name11 <- gsub(pattern = "\\.tif$", "", name1)
    data2 <- raster(infile2$datapath)
    name2 <- infile2$name
    name22 <- gsub(pattern = "\\.tif$", "", name2)
    data3 <- raster(infile3$datapath)
    name3 <- infile3$name
    name33 <- gsub(pattern = "\\.tif$", "", name3)
    data4 <- raster(infile4$datapath)
    name4 <- infile4$name
    name44 <- gsub(pattern = "\\.tif$", "", name4)
    data5 <- raster(infile5$datapath)
    name5 <- infile5$name
    name55 <- gsub(pattern = "\\.tif$", "", name5)
    rstack <- stack(data1,data2,data3,data4,data5)
    names(rstack)[names(rstack) == "X0.1"] <- name11
    names(rstack)[names(rstack) == "X0.2"] <- name22
    names(rstack)[names(rstack) == "X0.3"] <- name33
    names(rstack)[names(rstack) == "X0.4"] <- name44
    names(rstack)[names(rstack) == "X0.5"] <- name55
    extr <- extract(rstack,shp,df=TRUE)
    set.seed(100)
    extr <- merge(extr,shp,by.x="ID",by.y="UID")
    depe2 <- extr[,depe]
    trainids <- createDataPartition(depe2,list = FALSE,p=0.8)
    trainDat <- extr[trainids,]
    testDat <- extr[-trainids,]
    predict_variables <- c(name11,name22,name33,name44,name55)
    response_variable <- depe
    model <- train(trainDat[,predict_variables],trainDat[,response_variable],method = input$type,
                   trControl = trainControl(method = "cv", classProbs = TRUE),importance=TRUE)
    print(model)
  })
  })
  
  observeEvent(input$click,{
    output$varimp <- renderPlot({
      infile1 <- input$rasters
      infile2 <- input$rst2
      infile3 <- input$rst3
      infile4 <- input$rst4
      infile5 <- input$rst5
      newe <- input$training
      depe <- input$dep
      shp <- geojsonio::geojson_read(newe$datapath, what = "sp")
      
      if(is.null(infile1))
        return(NULL)
      
      if(is.null(infile2))
        return(NULL)
      
      data1 <- raster(infile1$datapath)
      name1 <- infile1$name
      name11 <- gsub(pattern = "\\.tif$", "", name1)
      data2 <- raster(infile2$datapath)
      name2 <- infile2$name
      name22 <- gsub(pattern = "\\.tif$", "", name2)
      data3 <- raster(infile3$datapath)
      name3 <- infile3$name
      name33 <- gsub(pattern = "\\.tif$", "", name3)
      data4 <- raster(infile4$datapath)
      name4 <- infile4$name
      name44 <- gsub(pattern = "\\.tif$", "", name4)
      data5 <- raster(infile5$datapath)
      name5 <- infile5$name
      name55 <- gsub(pattern = "\\.tif$", "", name5)
      rstack <- stack(data1,data2,data3,data4,data5)
      names(rstack)[names(rstack) == "X0.1"] <- name11
      names(rstack)[names(rstack) == "X0.2"] <- name22
      names(rstack)[names(rstack) == "X0.3"] <- name33
      names(rstack)[names(rstack) == "X0.4"] <- name44
      names(rstack)[names(rstack) == "X0.5"] <- name55
      extr <- extract(rstack,shp,df=TRUE)
      set.seed(100)
      extr <- merge(extr,shp,by.x="ID",by.y="UID")
      depe2 <- extr[,depe]
      trainids <- createDataPartition(depe2,list = FALSE,p=0.7)
      trainDat <- extr[trainids,]
      testDat <- extr[-trainids,]
      predict_variables <- c(name11,name22,name33,name44,name55)
      response_variable <- depe
      model <- train(trainDat[,predict_variables],trainDat[,response_variable],method = input$type,
                     trControl = trainControl(method = "cv", classProbs = TRUE),importance=TRUE)
      plot(varImp(model))
    })
  })
  


  
  #create a scatterplot object that the plotOutput function is expecting
  observeEvent(input$run_scatter,{output$scatter <- renderPlot({
    infile1 <- input$rasters
    infile2 <- input$rst2
    infile3 <- input$rst3
    infile4 <- input$rst4
    infile5 <- input$rst5
    newe <- input$training
    depe <- input$dep
    shp <- geojsonio::geojson_read(newe$datapath, what = "sp")
    
    if(is.null(infile1))
      return(NULL)
    
    if(is.null(infile2))
      return(NULL)
    
    data1 <- raster(infile1$datapath)
    name1 <- infile1$name
    name11 <- gsub(pattern = "\\.tif$", "", name1)
    data2 <- raster(infile2$datapath)
    name2 <- infile2$name
    name22 <- gsub(pattern = "\\.tif$", "", name2)
    data3 <- raster(infile3$datapath)
    name3 <- infile3$name
    name33 <- gsub(pattern = "\\.tif$", "", name3)
    data4 <- raster(infile4$datapath)
    name4 <- infile4$name
    name44 <- gsub(pattern = "\\.tif$", "", name4)
    data5 <- raster(infile5$datapath)
    name5 <- infile5$name
    name55 <- gsub(pattern = "\\.tif$", "", name5)
    rstack <- stack(data1,data2,data3,data4,data5)
    names(rstack)[names(rstack) == "X0.1"] <- name11
    names(rstack)[names(rstack) == "X0.2"] <- name22
    names(rstack)[names(rstack) == "X0.3"] <- name33
    names(rstack)[names(rstack) == "X0.4"] <- name44
    names(rstack)[names(rstack) == "X0.5"] <- name55
    extr <- extract(rstack,shp,df=TRUE)
    set.seed(100)
    extr <- merge(extr,shp,by.x="ID",by.y="UID")
    depe2 <- extr[,depe]
    updateSelectInput(session, "x",
                      label = "X-axis",
                      choices = colnames(extr),
                      selected = depe2
    )
    updateSelectInput(session, "y",
                      label = "Y-axis",
                      choices = colnames(extr),
                      selected = depe2
    )
    ggplot(data = extr,aes_string(x=input$x,y=input$y))+
      geom_point()
  })})
  
}

#Creates a shiny app object
shinyApp(ui, server)