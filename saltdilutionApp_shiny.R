##################################
# Shiny app to calculate stream discharge using a slug injection of salt and a conductivity logger
#



library(shiny)
library(shinythemes)
library(ggplot2)

#the "ui" is the "user interface"
ui <- 
  navbarPage("Salt Dilution Stream Discharge Calculator",
             theme = shinytheme("spacelab"),#yeti
             #themeSelector(),
             tabPanel("Stream Discharge",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                          helpText("Open a .csv file containing just the conductivity values from
                                   one full salt wave (start and return to background conductivity) and no column name.
                                   Logger time interval is set below."),
                          fileInput('file1', 'Choose CSV File',
                                    accept=c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv')),
                          selectInput("Inj_Type","Choose salt injection type:", choices = list("Dry Salt" = 1, "Salt Solution" = 2), selected = "Dry Salt"),
                          conditionalPanel(
                            condition = "input.Inj_Type == 1",
                            numericInput("Inj_Mass", "Injected salt mass (g):", 500,
                                         min = 0, step = 1)
                          ),
                          conditionalPanel(
                            condition = "input.Inj_Type == 2",
                            numericInput("Inj_Vol", "Injected salt solution volume (ml):", 1000,
                                         min = 0, step = 1)
                          ),
                          numericInput("Time_Interval", "Logger time interval (s):", 1,
                                       min = 0),
                          numericInput("Cal_Const", "Concentration factor:", 0.000486,
                                       min = 0),
                          numericInput("EC_bg", "Background conductivity (μS/cm):", 0,
                                       min = 0, step = 0.1),
                          h4("Time Interval Testing"),
                          checkboxInput("TestCheck","Check to select every nth sample (logger time) from dataset. Be sure to match time interval above with selected below.",value=FALSE),
                          numericInput("TimeTest", "Logger time interval (s):", 1,
                                       min = 0),
                          helpText("This app is intended to calculate discharge using a slug injection of salt, it requires 
                                        data recorded with a conductivity logger and a concentration factor of the response of
                                        conductivity against NaCl and the amount of salt injected."),    
                          
                          helpText("Shiny app developed by J. Goetz (2017).")
                          
                        ),
                        mainPanel(
                          h3(strong(textOutput("Q"))), br(),
                          plotOutput('plot'),
                          textInput('siteDate', "Enter stream name and/or date:", placeholder = "False Creek - 02 June 2017") ,
                          downloadButton('downloadPlot', 'Download Plot'),
                          downloadButton('downloadTable', 'Download Results')
                        )
                      )
             ),
             tabPanel("Concentration Factor",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     helpText("Set the secondary and calibration tank information below and then enter each calibration point in the space to the right."),
                                     selectInput("Inj_Type_Cal","Choose salt injection type:", choices = list("Dry Salt" = 1, "Salt Solution" = 2), selected = "Dry Salt"),
                                     h4("Secondary Solution"),
                                     numericInput("Sec_Vol", "Initial volume of water (ml):", 1000,
                                                  min = 0),
                                     conditionalPanel(
                                       condition = "input.Inj_Type_Cal == 1",
                                       numericInput("Inj_Mass_Cal", "Mass of injection salt added (g):", 1,
                                                    min = 0, step = 1)
                                     ),
                                     conditionalPanel(
                                       condition = "input.Inj_Type_Cal == 2",
                                       numericInput("Inj_Vol_Cal", "Volume of injection salt solution added (ml):", 10,
                                                    min = 0, step = 1)
                                     ),
                                     h4("Calibration Solution"),
                                     numericInput("Cal_Vol", "Initial volume of water (ml):", 1000,
                                                  min = 0)
                        ),
                        mainPanel(                          
                          h3(strong(textOutput("k"))),br(),
                          column(5,
                                 h4("Calibration Points"),
                                 numericInput("Vol_Added","Added volume of secondary solution (ml):",0, min=0,step = 1),
                                 numericInput("EC_Added","Conductivity after addition (μS/cm):",0, min=0, step = 0.1),
                                 actionButton("button", "Add Point"),actionButton("clearbutton", "Clear All Points"),br(),br(),
                                 tableOutput("CalibrationTable")),
                          column(7,align="center",
                                 plotOutput('calplot'),br(),
                                 textOutput("plotrsq"),br(),
                                 downloadButton('downloadCalPlot', 'Download Plot')
                                 
                          )
                        )
                      )
             ),
             tabPanel("Hydrometric Report",
                      fluidPage(
                        textInput("Stream_Name","Stream/station name:",placeholder = "False Creek near the mouth"),
                        dateInput("Date","Date of measurement:"),
                        textInput("Party","Party:")),
                      column(width=3,
                          textInput("AirTemp","Air Temperature (°C)"),
                          textInput("WaterTemp","Water Temperature (°C)"),
                          textInput("StreamWidth","Stream width (m)"),
                          textInput("StreamArea","Stream Area (m)"),
                          textInput("Velocity","Mean Velocity (m/s)"),
                          textInput("Time","Time of measurement:"),
                          numericInput("Stage","Height of gauge (m):", 0, step = 0.001)
                        ),
                        column(width=3,
                               mainPanel(downloadButton('downloadReport', 'Generate PDF Report')
                        )
                      )
             ) 
  )



#the server function is where all calculations are done
server <- function(input, output) {
  
  ##### Q Calculation #######
  
  # Read the csv and set it up for analysis 
  EC_data <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    data <- as.data.frame(read.csv(inFile$datapath, sep=",", header=FALSE,col.names = c("EC")))
   
    if (input$TestCheck==TRUE) {
      data <- as.data.frame(data[seq(1,nrow(data),input$TimeTest),])
      colnames(data) <- "EC"
    } else {
    }
     
    data$Time <- as.numeric(row.names(data))
    data$Time <- data$Time*input$Time_Interval
    data
  })
  
  # Calculate discharge and the area under the curve
  calcs <- reactive({  
    
    Area <- sum(EC_data()$EC)*input$Time_Interval - input$EC_bg*nrow(EC_data())*input$Time_Interval
    
    if (input$Inj_Type == 1) {
      Q <- round((input$Inj_Mass/1000) / (input$Cal_Const*Area),5)
    } else {
      Q <- round((input$Inj_Vol/1000000) / (input$Cal_Const*Area),5)
    }
    Q.Area <- c(Q,Area)
  })
  # Set the discharge value up for display
  output$Q <- renderText({
    paste("Stream discharge = ", calcs()[1]," cms")
  })
  
  
  # Create plot of salt wave
  plotInput <- function(){
    saltplot<- ggplot(EC_data(), aes(x = Time, y= EC)) + 
      geom_line() +
      geom_ribbon(ymin=input$EC_bg, ymax=EC_data()$"EC", fill="dodgerblue2", alpha=0.7)+
      #geom_hline(yintercept = input$EC_bg, alpha=0.4)+
      #theme_minimal() +
      ggtitle(input$siteDate) +
      theme(legend.position = "none",
            plot.title = element_text(size=18,face="italic"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=15))+
      ylab(expression(paste("Conductivity (μS/cm)")))+
      xlab("Time since start (sec)")
    print(saltplot)
    
  }
  # Render plot for display
  output$plot <- renderPlot({
    plotInput()
    })
  # Plot download button
  output$downloadPlot <- downloadHandler(
    filename = "SaltWavePlot.png",
    content = function(file) {
      png(file,width = 900, height=400)
      print(plotInput())
      dev.off()
    })    
  
  # Create a csv to save the results
  resultsQ <- reactive({
    list <- list(Name_Date=input$siteDate,
                 InjectionType=ifelse(input$Inj_Type == 1,"Dry Salt","Salt Solution"),
                 Discharge_cms=calcs()[1],
                 ConcentrationFactor=input$Cal_Const,
                 SaltWaveArea_uScm=calcs()[2],
                 TimeInterval_sec=input$Time_Interval,
                 Injection_g_or_ml=ifelse(input$Inj_Type== 1,input$Inj_Mass,input$Inj_Vol),
                 BackgroundCond_uScm=input$EC_bg,
                 MaxCond_uScm=max(EC_data()$EC)
    )
    table <- as.data.frame(list)
    table <- tidyr::gather(table,Variable, Value, 1:9)
    table
  })
  output$downloadTable <- downloadHandler(
    filename = function() { paste0('DischargeResults.csv') },
    content = function(file) {
      write.table(resultsQ(), file, sep=",",row.names = FALSE, col.names = FALSE)
    })
  
  
  
  ##### CALIBRATION #######
  
  # Create the calibration table
  values <- reactiveValues()
  values$CalTable <- data.frame("Total_Volume_Added"=numeric(),
                                "Conductivity"=numeric(),
                                stringsAsFactors=FALSE)
  # Use a button to add each pair of volume and EC values
  observeEvent(input$button, {
    if (nrow(values$CalTable)==0) { # do this is there is no data added yet
      isolate(values$CalTable[nrow(values$CalTable) + 1,] <- c(input$Vol_Added, input$EC_Added))
    } else { # do this if there is at least one row of data
      isolate(values$CalTable[nrow(values$CalTable) + 1,] <- c(values$CalTable[nrow(values$CalTable),1]+input$Vol_Added, input$EC_Added))#
    }
  })
  # Use a button to clear table/plot
  observeEvent(input$clearbutton, {
    values$CalTable <- values$CalTable[0,]
  })  
  # Render the calibration points table
  output$CalibrationTable <- renderTable({
    values$CalTable
    colnames(values$CalTable)=c("Total Secondary Solution Added (ml)","Conductivity (μS/cm)")
    values$CalTable
  })
  # Create the calibration plot
  CalPlot <- function(){
    calib.plot<- ggplot(values$CalTable, aes(x = values$CalTable[,1], y= values$CalTable[,2])) + 
      geom_point(size=4, shape=10, colour="firebrick2", stroke=1.5) +
      geom_smooth(method=lm,se=FALSE, colour="steelblue3", linetype = "dashed", size=.75) +
      #ggtitle("Calibration Points") +
      ylab("Caibration Solution Conductivity (μS/cm)")+
      xlab("Total Secondary Solution Added (ml)")+
      {if (nrow(values$CalTable)>0) {if(!is.na(kCalc()[1]) & input$Inj_Type_Cal == 1) scale_x_continuous(sec.axis = sec_axis(~.*(coef(lm(CalibrationCalc()[,3] ~ CalibrationCalc()[,1]))[2])+(coef(lm(CalibrationCalc()[,3] ~ CalibrationCalc()[,1]))[1]),name="Mass Concentration (g salt/L water)"))}}+
      {if (nrow(values$CalTable)>0) {if(!is.na(kCalc()[1]) & input$Inj_Type_Cal == 2) scale_x_continuous(sec.axis = sec_axis(~.*(coef(lm(CalibrationCalc()[,3] ~ CalibrationCalc()[,1]))[2])+(coef(lm(CalibrationCalc()[,3] ~ CalibrationCalc()[,1]))[1]),name="Relative Concentration (ml solution/L water)"))}}+
      theme(legend.position = "none",
            axis.text=element_text(size=12),
            axis.title=element_text(size=15))
    print(calib.plot)
  }
  output$calplot <- renderPlot({
   # if (nrow(values$CalTable)>0)
          CalPlot()
   # else
   #   NULL
  })
  output$downloadCalPlot <- downloadHandler(
    filename = "CalibrationPlot.png",
    content = function(file) {
      png(file)
      print(CalPlot())
      dev.off()
    })    
  
  # Create a reactive table to calculate relative/mass concentration to pull from
  CalibrationCalc <- reactive({  
    table <- values$CalTable
    if (input$Inj_Type_Cal == 1) {
      table$RC <- round(table[,1]*(input$Inj_Mass_Cal/input$Sec_Vol)/(input$Cal_Vol+table[,1])*1000,6)
    } else {
      table$RC <- round(table[,1]*(input$Inj_Vol_Cal/(input$Inj_Vol_Cal+input$Sec_Vol))/(input$Cal_Vol+table[,1]),6)
    }
    table
  })
  
  # Calculate the Concentration factor and r-squared
  kCalc <- reactive({  
    table <- data.frame(CalibrationCalc())
    if (is.na(table[1,1])) {
      slope.R2 = c("NA","NA")
    } else {
      slope.R2 = c(round(coef(lm(table$RC ~ table$Conductivity))[2],9),round(as.numeric(summary(lm(table$RC ~ table$Conductivity))[8]),4))
    }
    slope.R2
  })
  CFunits <- reactive({
    if (input$Inj_Type_Cal == 1) {
      units <- "gSalt/L / μS/cm"
    } else {
      units <- "mlSolution/L / μS/cm"
    }
    units
  })
  # Set the Concentration factor  up for display
  output$k <- renderText({paste("Concentration factor = ", kCalc()[1],CFunits())})
  # Set the r-squared value up for display
  output$plotrsq <- renderText({paste("R-squared = ", kCalc()[2] )})
  
  
  
  ##### HYDROMETRIC REPORT #######
  ##### TBD #######
  
  
  
  output$downloadReport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = input$Inj_Type)#,k = output$Q)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}

shinyApp(ui = ui, server = server)


