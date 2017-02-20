library(caret)

library(randomForest)
library(plyr)

shinyServer(function(input, output) {
  
  output$monsterpred <- renderPrint({
    
    if (is.null(input$color))
      return(NULL)
    
    ###########################################################
    ## load reference data
    ###########################################################    
    train = read.csv("data/train.csv", header = TRUE, sep = ",")
    
    
    ###########################################################
    ## build test set data frame
    ###########################################################

    test = data.frame(bone_length   = as.numeric(input$bonelength),
                      rotting_flesh = as.numeric(input$rotflesh),
                      hair_length   = as.numeric(input$hairlength),
                      has_soul      = as.numeric(input$hassoul), 
                      color         = as.factor(input$color) 
    )
    
    
    ###########################################################
    ## train model
    ###########################################################
    
    control = trainControl(
      method="cv",
      number=10,
      savePredictions = 'final'
    )
    
    train$id = NULL
    
    ######################################################################
    ## Model was generated and saved as a file  to save processing time ##
    ######################################################################
    # model = train(type~., 
    #               data = train, # Use the trainSet dataframe as the training data
    #               method = "rf",
    #               trControl = control
    # )
    # saveRDS(model, file = "rf_mon.rds")
    #######################################################################
    
    model = readRDS("model/rf_mon.rds")
    
    # make predictions
    dt_pred = predict(model, test)
    
    # Result
    cat(paste0("This is a ", dt_pred[1], ".") )
    
  })
  
})