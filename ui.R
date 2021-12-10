library(shiny)
library(shinydashboard)

folder = getwd()
setwd(folder)

shinyUI(
  dashboardPage(
    dashboardHeader(title = "Data Project"),
    
    #### Definition of side bars
    dashboardSidebar(
      sidebarMenu(
        menuItem('Raw data', tabName = "Raw_data_tab"),
        menuItem('Varaibles Manipulation', tabName = "manipulation_tab"),
        menuItem('Data overview',tabName = "overview_tab"), 
        menuItem('Variables graphs',tabName = "variables_tab")  
      )
    ),
    
    #### Definition of Body that will appear based on side bars
    dashboardBody(
      
      tabItems(
        
        #----Body scheme for Raw data observations
        tabItem( tabName = "Raw_data_tab",
            fluidRow(
              box(title = 'Select a variable',
                  width = 5,
                  'The variable that you choose here will be displayed on the side graphics',
                  uiOutput("sel_column"),
                  actionButton("Update", "Update")
                  ),
              box(width = 7,plotOutput("variable_histogram")),
              box(width = 12,plotOutput("variable_evolution")),
              ),
            ),

      
        #-----Body scheme for Variables transformations
        tabItem( tabName = "manipulation_tab",
                 fluidRow(
                   box(
                     width = 7,
                     uiOutput("variable_name_output"),
                     box(
                       #Operations that are made with just one driver
                       width = 12,
                       title = 'Operations with 2 Drivers',
                       uiOutput("driver_output"),
                       uiOutput("one_drive_output"),
                       uiOutput("user_output"),
                       actionButton(inputId = "Create_variable1", "Create Variable")
                       ),
                     box(
                       #Operations that are made with 2 drivers
                       width = 12,
                       title = 'Operations with 2 Drivers',
                       uiOutput("two_drive_output"),
                       uiOutput("First_variable_output"),
                       uiOutput("Second_variable_output"),
                       actionButton(inputId = "Create_variable2", "Create Variable")
                       ),
                     ),
                   box(width = 5,
                       title = 'Variables available',
                       uiOutput("variables_descriptions_output"),
                       ),
                 ),
                 
            ),
        
        #-----Body scheme for Variables transformations
        tabItem( tabName = "overview_tab",
                 fluidRow(
                   box(width = 12,actionButton("Update_Overview", "Update Overview")),
                   box(width = 4,title = 'Shapiro Normality tests', uiOutput("Normality_test_output")),
                   box(width = 4,title = 'Stationary tests', uiOutput("Stationary_test_output")),
                   box(width = 4,title = 'Correlations',uiOutput("Correlation_table_output"))
                 ),
        ),
        
        #-----Body scheme for Variables transformations
        tabItem( tabName = "variables_tab",
                 fluidRow(
                   box(
                     width = 4,
                     title = 'Variables Time series',
                     uiOutput("Select_variables_output"),
                     actionButton(inputId = "show_variables", "Show Variables")
                   ),
                   box(
                     width = 8,
                     plotOutput("variables_time_series")
                     ),
                   
                 ),
        )
        
      ),  
      
      

    )
    
    
  )
  
)