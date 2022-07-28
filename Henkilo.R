#install shiny & openxlsx
library(shinyBS)
library(shiny)
library(openxlsx)
library(DT)
library(shinyWidgets)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(shinydashboard)
library(shinyjs)
library(rmarkdown)
library(knitr)
library(shinyalert)
# template data

template<- data.frame(
  name = c(""),
  surname =  c(""),
  date_of_birth= c(""), 
  salary = c(""),
  currency = c(""),
  position = c(""),
  seniority_level = c(""),
  team =  c(""),
  agreement_type = c(""),
  sex = c(""),
  date_of_employment = c(""),
  experience = c(""),
  hours_of_training = c(""))

########

# minimal Shiny UI
ui <- fluidPage(
  useShinyalert(),
  setBackgroundColor(color = "#2C346B"), 
  tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: grey;  color:black;font-family:courier new}
    .tabbable > .nav > li > a[data-value='t1'] {background-color: grey;   color:black; font-family:courier new}
    .tabbable > .nav > li > a[data-value='t2'] {background-color: grey;  color:black; font-family:courier new}
    .tabbable > .nav > li > a[data-value='t3'] {background-color: grey; color:black; font-family:courier new}
    .tabbable > .nav > li[class=active]    > a {background-color: #FFF559; color:black;font-family:courier new}
")),
  titlePanel(
    title = div(img(src="small.jpg", height = '70px', width = '150px'), style="background-color: #2C346B;")
  ),
  

  tabsetPanel(type = "pills",
              tabPanel("Homepage", 
                       fluidRow(
                         column(width = 5, offset = 1,
                                p(HTML("<br> Welcome to Henkilo! We are happy to have you here. <br><br> Henkilo is a human-centric HR resource planning application that allows you
                    to get a quick overview into the financial impact of your human resources. 
                           Download your data in the <i> Data Import </i> tab and view your analysis in the <i> Summary</i> tab"), style = "color:#FFF559; font-size:18px; font-family:courier new")
                         ),
                         column(width = 3, offset = 1,
                                div(img(src="Cat astronaut-cuate.png", height = '400px', width = '400px')))
                       )
              ) ,
              tabPanel("Data Import",
                       sidebarPanel(actionButton("input_instructions", "Input Instruction"),
                                    h3("Export Template"),
                                    downloadButton(
                                      "import_template", 
                                      "Export Template"
                                    ),
                                    fileInput("file",h3("Import File")),
                                    h3("Current Exchange Rates"),
                                    DT::dataTableOutput("exchange_rates"),
                       ),
                       tags$p(
                         tags$style(HTML("
                        .shiny-output-error-validation {
                          color: #ff3333;
                           font-size:40px; 
                           font-family:courier new
                        }
                      "))
                                         ),
                       
                       mainPanel(p(HTML("DATA OVERVIEW"),style = "color:#FFF559; font-size:30px; font-family:courier new"),
                                 div(DT::dataTableOutput("sample_table"),style = "width: 100%; background-color:white;"),
                       )
              ),
              tabPanel("Summary", sidebarPanel(actionButton("show", "Tax scenarios"),
                                               selectInput("var", label = "",
                                                           choices = c("No tax", "Genovia", "Panem"),
                                                           selected = "Genovia")),
                       mainPanel(radioGroupButtons(
                         inputId = "Plot_type",
                         label = "",
                         choices = c("Salary by Position", "Salary by Team", "Salary by Seniority level", "Employer's Tax"),
                         justified = FALSE,
                         selected = "Salary by Position"),
                         plotOutput("Plot",height = '600'))),
              tabPanel("Report", sidebarPanel(
                
                # button
                fileInput(inputId = "chooseReport",label="Choose Report"),
                
                selectInput("var_report", label = "",
                            choices = c("No tax", "Genovia", "Panem"),
                            selected = "Genovia"),
                
                checkboxInput(
                  inputId = "cbx_stat",
                  label = "Include statistics",
                  value = TRUE,
                  width = "400px"
                ),

                checkboxInput(
                  inputId = "cbx_plot",
                  label = "Include plots",
                  value = TRUE,
                  width = "400px"
                ),
                checkboxInput(
                  inputId = "cbx_anonymous",
                  label = "Anonymous data",
                  value = TRUE,
                  width = "400px"
                ),
                downloadButton('downloadReport',"Download the report")
              ),mainPanel(
                div( 
                  # report
                  tags$div(id="report-wrapper",
                           tags$style("#report-wrapper p{font-size:14pt;}"),
                           htmlOutput("renderedReport"),
                  )))
               ),
              tabPanel("Contact",
                       fluidRow(
                         column(width = 3, offset = 1,
                                      div(img(src="Search engines-amico.png", height = '500px', width = '500px'))),
                         column(width = 5, offset = 1,
                                p(HTML("<br> <br> <br> If you face any issue considering the system please concact system admins:\n\n
                                    synnyline99800@gmail.com \n\n sereikyte.paulina@gmail.com"),style = "color:#FFF559; font-size:18px; font-family:courier new"))
                         
                         )
  ),
  tabPanel("User Guide",
           fluidRow(
             column(width = 5, offset = 1,
                    div(img(src="Data import.png", height = '400px', width = '800px'))),
             column(width = 4, offset = 1,
                    p(HTML("<br> <br> Firstly import the data into the app. Only files with an .xslx extention are allowed, otherwise an error message will be displayed. By clicking 'Export Template' you can download
                    the template in the required format for the analysis. After clicking 'Input instruction' the right formating of the data will be shown.
                           Once the data is imported it will be available for viewing in the 'Data Overview' field. Here you can also see the current spot exchange rates for PLN obtained via an API. <br>
                           A test dataset will be provided with the materials to showcase the functionalities of the app (Test.xslx)<br><br>
                           "),style = "color:#FFF559; font-size:18px; font-family:courier new"))
             
           ),
           fluidRow(
             column(width = 5, offset = 1,
                    div(img(src="Tax scenario select.png", height = '400px', width = '800px'))),
             column(width = 4, offset = 1,
                    p(HTML("<br> <br> After the data has been imported the tax scenarios are calculated. By clicking the 'Tax scenarios' button you can read more about the different tax scenarions.
                    There are 4 analyses conducted based on the data provided: salary distribution by team, position and seniority level, as well as the tax imposed on the employer's profits. The graphs show the average salary after tax for each group in PLN.<br><br><br>
                           "),style = "color:#FFF559; font-size:18px; font-family:courier new"))
             
           )
           ,
           fluidRow(
             column(width = 5, offset = 1,
                    div(img(src="report generation.png", height = '400px', width = '800px'))),
             column(width = 4, offset = 1,
                    p(HTML("<br> <br> Finally, there is a possibility to generate and customise an Rmarkdown report. To get started, a template has to be imported into the app. You will find the 
                    template provided together with the materials (Report_template.Rmd). Next, the preferred tax scenario and specifications can be selected and the desired report viewed or generated.<br><br><br>
                           "),style = "color:#FFF559; font-size:18px; font-family:courier new"))
             
           )
  )
  
  ))

# minimal Shiny server
server <- function(input, output) {
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "Tax Scenarios",
      h2("No Tax"),
      "A scenario without any tax will be shown.",
      h2("Genovia"),
      "A scenario with progressive tax will be shown.",
      HTML("<hr><strong> Income tax by age</strong>"),
      HTML('<table style="width:100%">
  <tr>
    <td> <i> Ages between 25-60 </i></td>
    <td>25%</td>
  </tr>
  <tr>
    <td> <i> Ages below 25 </i></td>
    <td>5%</td>
  </tr>
  <tr>
    <td> <i> Ages above 60 </i></td>
    <td>10%</td>
  </tr>
</table>'),
      HTML("<hr><strong> Income tax by income</strong>"),
      HTML('<table style="width:100%">
  <tr>
    <td> <i>  Below 1000 diamonds </i></td>
    <td>5%</td>
  </tr>
  <tr>
    <td> <i> Between 1000 - 7000 diamonds </i></td>
    <td>25%</td>
  </tr>
  <tr>
    <td> <i> Above 7000 diamonds </i></td>
    <td>40% on the difference</td>
  </tr>
</table>'),
      HTML("<hr><strong> Income tax by agreement</strong><br>"),
      "If the employee is employed under regular employee contract, regular income tax will be applied. However if it is a B2B contract, a constant rate of 15% is applied and the employee is not entitled to health benefits.",
      HTML("<hr><strong>Employer's tax</strong> <br>"),
      "Employer's tax depends on the investment to training. If the hours of training per employee exceed 50h per year, 7% tax on employer's revenues will be applied. Else, the common rate of 10% is applied",
      h2("Panem"),
      "A scenario with crazy tax",
      HTML("<hr><strong> Income tax by income</strong>"),
      HTML('<table style="width:100%">
  <tr>
    <td> <i> Income < 1000 diamonds </i></td>
    <td>25%</td>
  </tr>
  <tr>
    <td> <i> Income between 1000-5000 diamonds </i></td>
    <td>50%</td>
  </tr>
  <tr>
    <td> <i> Income >5000 diamonds </i></td>
    <td>75%</td>
  </tr>
</table>'),
      HTML("<hr><strong> Income tax lenght of employemnt</strong>"),
      HTML('<table style="width:100%">
  <tr>
    <td> <i> Length of employment < 3y </i></td>
    <td>No tax reductions</td>
  </tr>
  <tr>
    <td> <i> For every year of employment after 3y </i></td>
    <td>1% reduction in tax</td>
  </tr>
</table>'),
      HTML("<hr><strong> Employer's tax</strong>"),
      "The employer receives an additional 5% tax on revenues will be applied if more than 60% of the employees are of one gender.",
      easyClose = TRUE
    ))
  })
  output$import_template <- downloadHandler(
    filename = function() {
      "employee_data.xlsx"
    },
    content = function(file) {
      workbook <- createWorkbook()
      #Workbook ######
      
      addWorksheet(
        wb = workbook,
        sheetName = "Employee Data"
      )
      
      setColWidths(
        workbook,
        1,
        cols = 1:13,
        widths = c(12, 18, 15, 8, 8, 10, 12, 8, 15, 6, 18, 12,25)
      )
      
      writeData(
        workbook,
        sheet = 1,
        c("Employee Data"),
        startRow = 1,
        startCol = 1
      )
      
      addStyle(
        workbook,
        sheet = 1,
        style = createStyle(
          fontSize = 20,
          textDecoration = "bold"
        ),
        rows = 1,
        cols = 1
      )
      
      writeData(
        workbook,
        sheet = 1,
        template,
        startRow = 3,
        startCol = 1
      )
      

      
      addStyle(
        workbook,
        sheet = 1,
        style = createStyle(
          fgFill = "#1a5bc4",
          halign = "center",
          fontColour = "#ffffff"
        ),
        rows = 3,
        cols = 1:13,
        gridExpand = TRUE
      )
      
      addStyle(
        workbook,
        sheet = 1,
        style = createStyle(
          fgFill = "#d8d8d8",
          numFmt = "comma"
        ),
        rows = seq(from=4, to =100 , by=2),
        cols = 1:13,
        gridExpand = TRUE
      )
      
      addStyle(
        workbook,
        sheet = 1,
        style = createStyle(
          fgFill = "#9ed7ff",
          numFmt = "comma"
        ),
        rows = seq(from=5, to =101 , by=2),
        cols = 1:13,
        gridExpand = TRUE
      )
      #######
      saveWorkbook(workbook, file)
    })
  
  observeEvent(input$input_instructions, {
    showModal(modalDialog(
      title = "Input Instructions",
      h2("Name and Surname (character input)"),
      "Standard cells that needs to be specified",
      h2("Date of birth / Date of Employment (date input)"),
      "Dates specifying date of birth of the employee and the date of employment",
      h2("Salary / hours of training (numeric input)"),
      "Numbers specifying salary of the Employee and hours of training",
      h2("Currency (character input)"),
      "The specified salary will be in the inputed currency. Please refer to the list of available currencies on the left panel of Data Import. Based on currency there will be translation of the salary to PLN",
      h2("Other variables (character input"),
      "Please input data such as team of the employee, experience etc. based on your company structure. Some of variables will affect what you can see on plots.",
     ))})
  
  # formData <- reactive({
  #  read.csv(input$file,header = TRUE )
  #  })
  
  #  output$hist <- renderPlot({
  #    d<-na.omit(input$file)
  #    plot(d[,2],d[,3])
  #  })
  
  getExchangeRates <- function(){
    
    require(httr)
    
    getInfoInJson <- httr::GET('http://api.nbp.pl/api/exchangerates/tables/a/', 
                               accept_json())
    
    #safe the info in a json object 
    jsonInfoImg <- content(getInfoInJson, type="application/json")
    
    len <- length(jsonInfoImg[[1]][[4]])
    
    currTable<-data.frame("Code"=c(rep(NA,times = len)),"rate"=c(rep(NA,times = len)))
    
    for (i in 1:len){
      print(jsonInfoImg[[1]][[4]][[i]][[2]])
      currTable$currency[i] <- jsonInfoImg[[1]][[4]][[i]][[2]]
      currTable$rate[i] <- jsonInfoImg[[1]][[4]][[i]][[3]]
      
    }
    
    return(currTable)
  }
  
  df_data <- reactive({
    ex <-  getExchangeRates()
    curr <- ex$currency
    validate(need(input$file, "Please select an input file"))
    inFile <- input$file
    path <- as.character(inFile$datapath)
    #print(inFile$datapath)
    #print(grep("*.xlsx",path))
    if(grepl("*.xlsx",path)==1){
      
    if (is.null(inFile))
      return(NULL)
    df <- read.xlsx(inFile$datapath, startRow = 3, skipEmptyRows = TRUE, skipEmptyCols = TRUE,colNames = TRUE)
    if(is.numeric(as.numeric(df$salary))==1 & is.numeric(as.numeric(df$hours_of_training))==1){
    ex <- getExchangeRates()
    
    df <- merge(df,ex, all.x=TRUE)
    df$salaryPLN <- ifelse(df$currency != 'PLN',df$salary * df$rate, df$salary*1)
    
    return(df)
    }else{
      shinyalert("Oops!", "Wrong data", type = "error")
    }
    }else{
      shinyalert("Oops!", "Please choose a file with xlsx extension", type = "error")
    }
  })
  
  output$sample_table<- DT::renderDataTable({
    df <- df_data()
    df$date_of_birth <- as.Date(as.numeric(df$date_of_birth),origin = "1899-12-30")
    DT::datatable(df[,c(2:5,1,6:7,16)])
  })
  
  
  output$exchange_rates<- DT::renderDataTable({
    exchange_table <- getExchangeRates()
    DT::datatable(exchange_table)
  })
  
  # output$Position_Salary_Plot 
  my_function<- reactive({
    df <- df_data()
    exchange_table <- getExchangeRates()
    scenario <- input$var
    df$date_of_birth <- as.Date(df$date_of_birth,origin = "1899-12-30")
    df$date_of_employment <- as.Date(df$date_of_employment,origin = "1899-12-30")
    df$experience <- as.factor(df$experience)
    df$hours_of_training<- as.numeric(df$hours_of_training)
    df$salary<- as.numeric(df$salaryPLN)

    if(scenario=='Genovia'){
      salary_after_tax_1 <- NA
      salary_after_tax_1<-ifelse(df$salary <1000, df$salary * 0.95,
                                 ifelse(df$salary >= 1000 & df$salary <= 7000,df$salary * 0.75, df$salary * 0.6))
      today = as.Date(today())
      df$age <- NA
      df$age <-as.numeric(trunc((as.Date(df$date_of_birth) %--% today) / years(1)))
      salary_after_tax_2 <-NA
      salary_after_tax_2<-ifelse(df$age <25, df$salary * 0.95,
                                 ifelse(df$age > 60,df$salary * 0.90, df$salary * 0.75 ))
      salary_after_tax = c()
      for (row in 1:nrow(df)){
        x <- NA
        x <- ifelse(df$agreement_type[row] == "B2B", df$salary[row]* 0.85, min(salary_after_tax_1[row], salary_after_tax_2[row]))
        salary_after_tax <- c(salary_after_tax,x)
      }
      df$salary_after_tax <- salary_after_tax
  
      
    }
    if(scenario=='Panem'){
      salary_after_tax_1 <- NA
      df$salary_after_tax_1<-ifelse(df$salary <1000, df$salary * 0.75,
                                           ifelse(df$salary >= 1000 & df$salary <= 5000,df$salary * 0.5, df$salary * 0.25 ))
      today = as.Date(today())
      df$length_of_employment <-as.numeric(trunc((as.Date(df$date_of_employment) %--% today) / years(1)))
      df$salary_after_tax_2<-ifelse(df$length_of_employment <3, 
                                           df$salary_after_tax_1,
                                           (df$salary_after_tax_1/df$salary + (df$length_of_employment/100))*df$salary)
      
      df$salary_after_tax <- df$salary_after_tax_2
      employer_tax <- ifelse((length(which(df$sex == "M"))/length(which(df$sex == "F")))> 0.6, 0.05, 0)
    }
    if(scenario=='No tax'){
      df$salary_after_tax <- df$salary
    }
    return(df)
  })
  et_function<- reactive({
    df <- my_function()
    scenario <- input$var
    if(scenario=='Genovia'){
      employer_tax <- ifelse(mean(df$hours_of_training)> 50, 0.07, 0.1)
    }
    if(scenario=='Panem'){
      employer_tax <- ifelse((length(which(df$sex == "M"))/length(which(df$sex == "F")))> 0.6, 0.05, 0)
    }
    if(scenario=='No tax'){
      employer_tax = 0
    }
    return(employer_tax)
  })
  
  output$Plot <- renderPlot({
    df <- my_function()
    et<- et_function()
    library(extrafont) 
    loadfonts(device = "win")

    Plot_Theme = theme(
      axis.text.x = element_text(size = 20, color = "#FFF559", family = "mono", angle = 45, hjust=1),
      axis.text.y = element_text(size = 20, color = "#FFF559", family = "mono"),
      plot.title = element_text (size = 30, color = "#FFF559", family = "mono"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_line(color = "white", size = 0.3),
      panel.grid.minor = element_line(color = "white", size = 0.3),
      panel.background = element_rect(fill = "#2C346B"),
      plot.background = element_rect(fill = "#2C346B", color = "white", size = 0))
    if (input$Plot_type %in% "Salary by Position") {
      after_tax_mean<- aggregate(df$salary_after_tax, list(df$position), FUN=mean)
      ggplot(after_tax_mean, aes(x=reorder(after_tax_mean$Group.1, -after_tax_mean$x), y=after_tax_mean$x)) + 
        geom_bar(stat="identity", width=.5, fill="#FFF559") + 
        ggtitle("Average salary after tax deductions by Position") + 
        geom_text(aes(label = round(after_tax_mean$x, 0)), vjust = 2, color="#2C346B",family="mono") + 
        Plot_Theme
    }
    else if (input$Plot_type %in% "Salary by Team") {
      after_tax_mean_team<- aggregate(df$salary_after_tax, list(df$team), FUN=mean)
      ggplot(after_tax_mean_team, aes(x=reorder(after_tax_mean_team$Group.1, -after_tax_mean_team$x), y=after_tax_mean_team$x)) + 
        geom_bar(stat="identity", width=.5, fill="#FFF559") + 
        ggtitle("Average salary after tax deductions by Team") + 
        geom_text(aes(label = round(after_tax_mean_team$x, 0)), vjust = 2, color="#2C346B",family="mono") + 
        Plot_Theme
    }
    else if (input$Plot_type %in% "Salary by Seniority level"){
      after_tax_mean_sen<- aggregate(df$salary_after_tax, list(df$seniority_level), FUN=mean)
      ggplot(after_tax_mean_sen, aes(x=reorder(after_tax_mean_sen$Group.1, -after_tax_mean_sen$x), y=after_tax_mean_sen$x)) + 
        geom_bar(stat="identity", width=.5, fill="#FFF559") + 
        ggtitle("Average salary after tax deductions by Seniority Level") + 
        geom_text(aes(label = round(after_tax_mean_sen$x, 0)), vjust = 2, color="#2C346B",family="mono") + 
        Plot_Theme
    }
    else if (input$Plot_type %in% "Employer's Tax"){
      df_pie <- data.frame(
        group = c("Profit", "Tax"),
        value = c(1-et, et)
      )
      ggplot(df_pie, aes (x="", y = value, fill = factor(group))) + 
        geom_bar(width = 1, stat = "identity") +
        geom_text(aes(label = paste(round(value / sum(value) * 100, 1), "%")),
                  position = position_stack(vjust = 0.5), color="#2C346B",family="mono") +
        scale_fill_manual(values=c("#2C346B", "#FFF559"))+ 
        theme(axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              plot.title = element_text (size = 30, color = "#2C346B", family = "mono")) +
        labs(fill = "Category",
             x = NULL,
             y = NULL) + 
        ggtitle("Employer's tax and profit share")+
        coord_polar("y")
             }
  })
  
  report_path <- reactive({
    validate(need(input$chooseReport$datapath, "Please select a report template file"))
    path <- input$chooseReport$datapath
    return(path)})
  

  output$renderedReport<- renderUI({
    pth <- report_path()
    exch <- getExchangeRates()
    df <- df_data()
    div(includeHTML(
    rmarkdown::render(pth,params = list(data=df, exchange=exch, plots=input$cbx_plot, anonymous = input$cbx_anonymous, scenario=input$var_report, statss=input$cbx_stat))),
    setBackgroundColor(color = "#2C346B")
    ,style="background-color: white;")
  })
  
  output$downloadReport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      pth <- report_path()
      pth_cop <- paste0(substr(pth,1,nchar(pth)-4),"_copy.Rmd")
      tempReport <- file.path(pth)
      file.copy(pth_cop, tempReport, overwrite = TRUE)
      
      exch <- getExchangeRates()
      df <- df_data()
      
      # Set up parameters to pass to Rmd document
      params <- list(data = df, exchange=exch, plots=input$cbx_plot, anonymous = input$cbx_anonymous, scenario=input$var_report, statss=input$cbx_stat)
      
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

shinyApp(ui, server)