library(shiny)
library(shinydashboard)
library(flexdashboard)
library(dplyr)
#library(knitr)
#library(writexl)
library(DT)
#library(rmarkdown)
library(stringr)

report <- read.csv(paste0('FULLReport.csv'), header = TRUE, sep = ",")
report <- report[,-50,]
colnames(report)[14] <- 'GRYPHON_SCRIPTNAME'

report_newqc <- report %>% select(ACTIVE_STARTDATE,ACTIVE_ENDDATE,PANELID,COUNTRY_NAME,SURVEYID,SURVEYLINK,SURVEYNAME,CREATIONDATE,ENDFIELDDATE,
                                  TARGETCOMPLETES,NETSUITEPROJECTCODE,POINTSDEFAULT,POINTSMAX,GRYPHON_SCRIPTNAME,FIRSTSTARTTIMESTAMP,
                                  LATESTSTARTTIMESTAMP,COMPLETES,INCOMPLETES,QUOTAOUTS,ERROROUTS,SCREENOUTS,QCFAILS,BREAK_RATE,INCIDENCE_RATE,
                                  QCFAIL_RATE,LOI_MEDIAN_COMP,LOI_Q3_COMP,LOI_MEDIAN_SCREEN,LOI_Q3_SCREEN,LOI_MEDIAN_QUOTA,LOI_Q3_QUOTA,
                                  LOI_MEDIAN_ERR,LOI_Q3_ERR,LOI_MEDIAN_QCFAIL,LOI_Q3_QCFAIL,QC_SCORE,qc_1,qc_2,qc_3,qc_4,bad_1,bad_2,bad_3,bad_4,
                                  bad_5,bad_6,bad_7,bad_8,TEAM,QC_VERSION,AUTHORED_LANGUAGE,USED_LOCALES,QSL_SIZE,VARIABLES,WIDGET_COUNT) %>%
                            filter(!(is.na(QC_SCORE)))



panelchoices <- c('1','4','10','13','71','72','73','74','118','133','152','156','158','162')

datacolchoices <- c(unique(colnames(report)))

css <- HTML(
  "#qcnew_table > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
  transform:rotateX(180deg);
  }
  #qcnew_table > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
  transform:rotateX(180deg);
   }"
)



ui <- dashboardPage(
  dashboardHeader(title = "QC Reporting"),
  dashboardSidebar(sidebarMenu(
    menuItem("New QC Dashboard", tabName = "newqcdashboard", icon = icon("dashboard")),
    menuItem("New QC Comments", tabName = "newqccomments", icon = icon("th")),
    menuItem("Old QC Dashboard", tabName = "oldqcdashboard", icon = icon("dashboard")),
    menuItem("Old QC Comments", tabName = "oldqccomments", icon = icon("th"))
    ),
    
    selectInput("paneloptions",
                               "Select panel(s)",
                               choices= c("US/Canada"=panelchoices[1],
                                          "Germany"=panelchoices[2],
                                          "MENA"=panelchoices[3],
                                          "UK"=panelchoices[4],
                                          "Denmark"=panelchoices[5],
                                          "Finland"=panelchoices[6],
                                          "Norway"=panelchoices[7],
                                          "Sweden"=panelchoices[8],
                                          "France"=panelchoices[9],
                                          "South America"=panelchoices[10],
                                          "Europe"=panelchoices[11],
                                          "China"=panelchoices[12],
                                          "Japan"=panelchoices[13],
                                          "APAC"=panelchoices[14]),
                               selected = panelchoices[1],
                               multiple = TRUE,
                               width = '100%')
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "newqcdashboard",
              h2("New QC Reporting"),
        fluidRow(
          box(gaugeOutput("qc_score_total"), width = 12)
          ),
        fluidRow(
          tags$head(tags$style(css)),
          DT::dataTableOutput("qcnew_table")
          )
      ),
      
      # Second tab content (add a , after the above ")")
      tabItem(tabName = "newqccomments",
              h2("New QC Comments")
      )
    )
  )
)

server <- function(input, output) {
  
  show_data <- reactive({
    
    if (is.null(input$paneloptions)) {
      return(NULL)
    }
    
    report_newqc %>%
      filter(report_newqc$PANELID %in% input$paneloptions)

  })
  
  
  
  output$qc_score_total <- renderGauge({
    qcscore <- report_newqc %>%
      select(QC_SCORE) %>%
      filter(report_newqc$PANELID %in% input$paneloptions) %>%
      pull(QC_SCORE) %>%
      mean()
    
    g <- gauge(qcscore, min = 1, max = 4, label = "QC score")
    print(g)
  })
  
  
  output$qcnew_table <- DT::renderDataTable({
    
    target <- which(names(report_newqc) %in% 
                      c("ACTIVE_STARTDATE","ACTIVE_ENDDATE","CREATIONDATE","POINTSDEFAULT","POINTSMAX","FIRSTSTARTTIMESTAMP","LATESTSTARTTIMESTAMP",
                        "INCOMPLETES","QUOTAOUTS","ERROROUTS","SCREENOUTS","QCFAILS","LOI_Q3_COMP","LOI_MEDIAN_SCREEN","LOI_Q3_SCREEN","LOI_MEDIAN_QUOTA",
                        "LOI_Q3_QUOTA","LOI_MEDIAN_ERR","LOI_Q3_ERR","LOI_MEDIAN_QCFAIL","LOI_Q3_QCFAIL","qc_1","qc_2","qc_3","qc_4","bad_1","bad_2","bad_3","bad_4","bad_5","bad_6","bad_7","bad_8","QC_VERSION","VARIABLES","WIDGET_COUNT")) - 1
    
    DT::datatable(
      show_data(),
      rownames = FALSE,
      #filter = "top",
      #editable = 'cell',
      extensions = c('Buttons'),
      options = list(
        columnDefs = list(list(visible = FALSE, targets = target)),
        scrollX = TRUE,
        dom = 'Blfrtip',
        buttons = list(
          
          # insert buttons with copy and print
          # colvis includes the button to select and view only certain columns in the output table
          # from https://rstudio.github.io/DT/extensions.html 
          I('colvis'), 'copy', 'print',
          
          # code for the first dropdown download button
          # this will download only the current page only (depends on the number of rows selected in the lengthMenu)
          # using modifier = list(page = "current")
          # only the columns visible will be downloaded using the columns:":visible" option from:
          # https://stackoverflow.com/questions/72317260/how-to-download-only-the-selected-columns-in-a-dataframe-using-colvis-from-dt-in/72317607#72317607
          list(
            extend = 'collection',
            buttons = list(
              list(extend = "csv", filename = "page",exportOptions = list(
                columns = ":visible",modifier = list(page = "current"))
              ),
              list(extend = 'excel', filename = "page", title = NULL, 
                   exportOptions = list(columns = ":visible",modifier = list(page = "current")))),
            text = 'Download current page'),
          
          # code for the  second dropdown download button
          # this will download the entire dataset using modifier = list(page = "all")
          list(
            extend = 'collection',
            buttons = list(
              list(extend = "csv", filename = "data",exportOptions = list(
                columns = ":visible",modifier = list(page = "all"))
              ),
              list(extend = 'excel', filename = "data", title = NULL, 
                   exportOptions = list(columns = ":visible",modifier = list(page = "all")))),
            text = 'Download all data')
          
        ),
        # add the option to display more rows as a length menu
        lengthMenu = list(c(10,25,50,-1),
                          c('10','25','50','All'))
      ),
      class = "display")
  })
}

shinyApp(ui, server)