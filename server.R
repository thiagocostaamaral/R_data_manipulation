library(shiny)
library(shinydashboard)
library(gamlss)
library(roll)
library(dplyr)
library(corrplot)
library(tseries)
library(ggplot2)

#Possible operations to be done
One_driver_list <- list("Rolling standard deviation",
                  "Rolling mean",
                  "Lagging",
                  "Leading",
                  "Differencing")

two_driver_list <- list("Differencing",
                        "Spread",
                        "Ratio",
                        "Product")
#==============

#------------------
#--------Helping function
form_3d_corr_df<- function(correlation){
  names <- colnames(correlation)
  column1 <- list()
  for(i in 1:length(names)){
    for(j in 1:length(names)){
      column1[[length(column1)+1]] <- c(names[i],names[j],correlation[i,j])
    }
  }
  df = as.data.frame(do.call(rbind, column1))
  colnames(df)<-c('Variable 1','Variable 2','Correlation')
  return(df)
}

form_normality_table <- function(df){
  columns <-  colnames(df)
  new_df_list <- list() 
  for(column in columns){
    result = shapiro.test(df[,column])
    new_df_list[[length(new_df_list)+1]] <- c(column,result$p.value,result$statistic[['W']])
  }
  new_df = as.data.frame(do.call(rbind, new_df_list))
  colnames(new_df)<-c('Variable','p value','W values')
  return(new_df)
}

form_stationary_table <- function(df){
  columns <-  colnames(df)
  new_df_list <- list() 
  for(column in columns){
    result = adf.test(df[,column])
    new_df_list[[length(new_df_list)+1]] <- c(column,result$p.value, result$statistic[['Dickey-Fuller']]  )
  }
  new_df = as.data.frame(do.call(rbind, new_df_list))
  colnames(new_df)<-c('Variable','p value','Dickey-Fuller statistic')
  return(new_df)
}

form_plot_table <- function(df, vector){
  df = subset(df, select=vector)
  new_df <- list()
  for(column in colnames(df)){
    size = length(df[,column])
    for(i in 1:size){
      print(i)
      new_df[[length(new_df)+1]] <- c(column, i,df[i,column])
    }
  }
  new_df = as.data.frame(do.call(rbind, new_df))
  colnames(new_df)<-c('Variable','x','y')
  
  new_df[,'x'] = as.numeric(as.character(new_df[,'x']))
  new_df[,'y'] = as.numeric(as.character(new_df[,'y']))
  print(new_df)
  return (new_df)
}

show_metadata_table <- function(output,metadata){
  #print(metadata)
  output$variables_descriptions_output <- renderUI({
    renderDataTable(metadata, options = list(
      pageLength = 40,
      width = 12
    ))
  })
}

#------------------
#--------Naming function
new_metadata_expression <-function(input,type){
  new_variable_name <- input$new_variable_name
  selected_operation <- input$one_driver_operation
  selected_operation_two <- input$two_driver_operation
  driver <- input$driver
  driver1 <- input$driver1
  driver2 <- input$driver2
  value <- input$User_input
  if (type == 1){
    if(selected_operation=='Rolling standard deviation'){
      expression <- paste("Roll_std(", driver,"; ",value ,")", sep="")
    }
    else if(selected_operation=='Rolling mean'){
      expression <- paste("Roll_mean(", driver,"; ",value ,")", sep="")
    }
    else if(selected_operation=='Lagging'){
      expression <- paste("Lagging(", driver,"; ",value ,")", sep="")
    }
    else if(selected_operation=='Leading'){
      expression <- paste("Leading(", driver,"; ",value ,")", sep="")
    }
    else if(selected_operation=='Differencing'){
      expression <- paste("Diff(", driver,"; ",value ,")", sep="")
    }
    else{ print('Something went wrong')}
  }
  else{
    print('Forming expression to driver 2')
    if(selected_operation_two=='Differencing'){
      expression <- paste("Diff(", driver1,"; ",driver2 ,")", sep="")
    }
    else if(selected_operation_two=='Ratio'){
      expression <- paste("Ratio(", driver1,"; ",driver2 ,")", sep="")
    }
    else if(selected_operation_two=='Product'){
      expression <- paste("Prod(", driver1,"; ",driver2 ,")", sep="")
    }
    else if(selected_operation_two=='Spread'){
      expression <- paste("Spread(", driver1,"; ",driver2 ,")", sep="")
    }
    else{ print('Something went wrong')}
  }
  return( expression)

}

#------------------
#--------Operations Functions
rolling_std <- function(data, new_variable_name,driver,value){
  print('Rolling standard deviation')
  rolling_value = as.integer(as.numeric(value))
  data[,new_variable_name]=roll::roll_sd(data[,driver], rolling_value)
  return(data)
}

rolling_mean <- function(data, new_variable_name,driver,value){
  print('Rolling standard deviation')
  rolling_value = as.integer(as.numeric(value))
  data[,new_variable_name]=roll::roll_mean(data[,driver], rolling_value)
  return(data)
}

lagging <- function(data, new_variable_name,driver,value){
  print('Rolling standard deviation')
  rolling_value = as.integer(as.numeric(value))
  data[,new_variable_name]=dplyr::lag(data[,driver], rolling_value)
  return(data)
}

leading <- function(data, new_variable_name,driver,value){
  print('Rolling standard deviation')
  rolling_value = as.integer(as.numeric(value))
  data[,new_variable_name]=dplyr::lead(data[,driver], rolling_value)
  return(data)
}

diff_1d <- function(data, new_variable_name,driver,value){
  print('Rolling standard deviation')
  float_value = as.numeric(value)
  data[,new_variable_name]=data[,driver] -  float_value
  return(data)
}

diff_2d <- function(data, new_variable_name,driver1,driver2){
  print('Differencing..')
  data[,new_variable_name]=data[,driver1] -  data[,driver2] 
  return(data)
}

ratio <- function(data, new_variable_name,driver1,driver2){
  print('Ratio')
  data[,new_variable_name]=data[,driver1]/data[,driver2] 
  return(data)
}

product <- function(data, new_variable_name,driver1,driver2){
  print('Ratio')
  data[,new_variable_name]=data[,driver1]*data[,driver2] 
  return(data)
}

spread <- function(data, new_variable_name,driver1,driver2){
  print('Ratio')
  data[,new_variable_name]=data[,driver1]+ data[,driver2]#! still have to look this 
  return(data)
}

#------------------
#--------Driver Functions
one_drive_operation <- function(input,complete_data,metadata){
  new_variable_name <- input$new_variable_name
  selected_operation <- input$one_driver_operation
  print('-- Creating new variable based in 1 driver --')
  driver <- input$driver
  value <- input$User_input
  if (new_variable_name == ""){
    print('W: Variable need a name')
    return(complete_data)
    }
  else if(is.na(as.numeric(value))){
    print('W: You need to insert a valid number')
    return(complete_data)
    }
  else if(new_variable_name %in% colnames(complete_data)){
    print('W: You are trying to create a variable that already exists')
    return(complete_data)
    }
  else {
    print('Creating new variable....')
    if(selected_operation=='Rolling standard deviation'){
      data <- rolling_std(complete_data, new_variable_name,driver,value)
      }
    else if(selected_operation=='Rolling mean'){
      data <- rolling_mean(complete_data, new_variable_name,driver,value)
      }
    else if(selected_operation=='Lagging'){
      data <- lagging(complete_data, new_variable_name,driver,value)
      }
    else if(selected_operation=='Leading'){
      data <- leading(complete_data, new_variable_name,driver,value)
    }
    else if(selected_operation=='Differencing'){
      data <- diff_1d(complete_data, new_variable_name,driver,value)
      }
    else{ print('Something went wrong')}
    
    return(data)
    
  }
}

two_drive_operation <- function(input,complete_data,metadata){
  new_variable_name <- input$new_variable_name
  selected_operation <- input$two_driver_operation
  print('-- Creating new variable based in 1 driver --')
  driver1 <- input$driver1
  driver2 <- input$driver2
  if (new_variable_name == ""){
    print('W: Variable need a name')
    return(complete_data)
  }
  else if(new_variable_name %in% colnames(complete_data)){
    print('W: You are trying to creat a variable that already exists')
    return(complete_data)
  }
  else {
    print('Creating new variable....')
    if(selected_operation=='Ratio'){
      data <- ratio(complete_data, new_variable_name,driver1,driver2)
    }
    else if(selected_operation=='Differencing'){
      data <- diff_2d(complete_data, new_variable_name,driver1,driver2)
    }
    else if(selected_operation=='Product'){
      data <- product(complete_data, new_variable_name,driver1,driver2)
    }
    else if(selected_operation=='Spread'){
      data <- spread(complete_data, new_variable_name,driver1,driver2)
    }

    else{ print('Something went wrong')}
    
    return(data)
    
  }
}

#-------
#------------------
shinyServer(
  function(input,output){
    raw_data <- gamlss.data::oil
    input_variables <- colnames(raw_data)
    complete_data <- data.frame(raw_data)

    reactive_values = reactiveValues()
    reactive_values$Data = complete_data
    
    #Creating metadata that will contain main information
    metadata = data.frame(input_variables)
    names(metadata) <- c('Variable Name')
    metadata[,'Variable Type'] <- 'Input data'
    metadata[,'Expression'] <- metadata[,'Variable Name']
    reactive_values$Metadata = metadata
    #---------
    #--- RAW Data overview structure
    #---------
    output$sel_column <- renderUI({
      selectInput(inputId = "selected_variable", label = "Variables Options", choices = input_variables, multiple = F)
    })
    
    #---Event to plot graphs
    observeEvent(input$Update, {
      ## Update the nodes list 
      selected_variable <- input$selected_variable
      print(selected_variable)
      
      ##Plot variable evolution based on user input
      output$variable_evolution <- renderPlot(
        plot(
          raw_data[,selected_variable],
          main="Variable Evolution",
          ylab=selected_variable,
          xlab="Observed Points"
        )
      )
      
      ##Plot variable histogram based on user input
      output$variable_histogram <- renderPlot(
        hist(
          raw_data[,selected_variable],
          main="Variable Hystogram",
          ylab=selected_variable,
          xlab="Variable Value"
        )
      )
    })
    
    #---------
    #--- Variables transformations structure
    #---------
    #Defining new variable name
    output$variable_name_output <- renderUI({
      textInput(inputId = "new_variable_name", label = "Define name of new variable")
    }) 
    
    ##----Table
    show_metadata_table(output,metadata)
    ############

    #List of options to 1 driver operation
    output$one_drive_output <- renderUI({
      selectInput(inputId = "one_driver_operation", label = "Operation", choices = One_driver_list, multiple = F)
    })
    
    
    #Give variables options to 1 driver operations
    output$driver_output <- renderUI({
      selectInput(inputId = "driver", label = "Driver", choices = colnames(reactive_values$Data), multiple = F)
    })
   
    output$user_output <- renderUI({
      textInput(inputId = "User_input", label = "Define number to apply operation above")
    }) 
    
    #---------------------------
    #List of Options to 2 drivers operation
    output$two_drive_output <- renderUI({
      selectInput(inputId = "two_driver_operation", label = "Operation", choices = two_driver_list, multiple = F)
    })
    
    #List of options to 1 driver operation
    output$First_variable_output <- renderUI({
      selectInput(inputId = "driver1", label = "Driver 1 ", choices = colnames(reactive_values$Data), multiple = F)
    })
    output$Second_variable_output <- renderUI({
      selectInput(inputId = "driver2", label = "Driver 2 ", choices = colnames(reactive_values$Data), multiple = F)
    })
    
    #---Dealing with creation of variable based in 1 driver
    observeEvent(input$Create_variable1, {
      new_variable_name <- input$new_variable_name
      if(colnames(reactive_values$Data)[length(reactive_values$Data)]!=new_variable_name){
        data <- one_drive_operation(input,reactive_values$Data,metadata)
        reactive_values$Data <- data
        #print(reactive_values$Data)
        new_variable_name <- input$new_variable_name
        if(colnames(reactive_values$Data)[length(reactive_values$Data)]==new_variable_name){
          expression <-new_metadata_expression(input,1)
          print('Expression: ')
          print(expression)
          
          metadata <-reactive_values$Metadata
          metadata[nrow(metadata)+1,] <- c(new_variable_name,'User Defined',expression)
          reactive_values$Metadata<-metadata
        }
        
        show_metadata_table(output,reactive_values$Metadata)
      }
    })
    
    #---Dealing with creation of variable based in 2 driver
    observeEvent(input$Create_variable2, {
      new_variable_name <- input$new_variable_name
      if(colnames(reactive_values$Data)[length(reactive_values$Data)]!=new_variable_name){
        data <- two_drive_operation(input,reactive_values$Data,metadata)
        reactive_values$Data <- data
        #print(reactive_values$Data)
        if(colnames(reactive_values$Data)[length(reactive_values$Data)]==new_variable_name){
          expression <-new_metadata_expression(input,2)
          print('Expression: ')
          print(expression)
          
          metadata <-reactive_values$Metadata
          metadata[nrow(metadata)+1,] <- c(new_variable_name,'User Defined',expression)
          reactive_values$Metadata<-metadata
        }
        show_metadata_table(output,reactive_values$Metadata)
      }
    })
    
    
    
    #---------
    #--- Variable overview
    #---------
    observeEvent(input$Update_Overview, {
      data <- reactive_values$Data
      correlation <- cor(data,use = 'pairwise')
      correlation_3d <- form_3d_corr_df(correlation)
      normality_table <- form_normality_table(data)
      stationaty_table <- form_stationary_table(data_oil)
      #--- Render normality table
      output$Normality_test_output <- renderUI({
        renderDataTable(normality_table, options = list(
          pageLength = 40,
          width = 12
        ))
      })
      
      #--- Render Stationary table
      output$Stationary_test_output <- renderUI({
        renderDataTable(stationaty_table, options = list(
          pageLength = 40,
          width = 12
        ))
      })
      
      #--- Render correlation table
      output$Correlation_table_output <- renderUI({
        renderDataTable(correlation_3d, options = list(
          pageLength = 30,
          width = 12
        ))
      })
    })
    
    
    #---------
    #--- Variable overview
    #---------  
    output$Select_variables_output <- renderUI({
      selectInput(inputId = "drivers", label = "Drivers ", choices = colnames(reactive_values$Data), multiple = T)
    })
    
    observeEvent(input$show_variables, {

      print(input$drivers)
      df <- reactive_values$Data
      vector <- c(input$drivers)
      plot_df <- form_plot_table(df, vector)

      output$variables_time_series <- renderPlot({
        ggplot(plot_df, aes(x, y,colour=Variable,group = 1))+geom_point()

    
      })
      
      
      
    })
    
    
  }
  
)