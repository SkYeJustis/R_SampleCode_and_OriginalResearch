#################################################################################
#Wearables Study Batch Processing Utility Script                                #
# Purpose: Create summary visualizations for all 72 participant's wearable data #
#Date: 1.11.2016                                                                #
#################################################################################
library(ggplot2)
library(reshape2)
library(plyr)
library(scales)
library(gridExtra)
library(grid)

#Loop through all 72 participant files available in the studyData folder
file.names = list.files(path="studyData/", pattern="*.csv", full.names=TRUE, recursive=FALSE)

for(i in 1:length(file.names)){
  file = file.names[i]
  print("File Name")
  print(file)
  fname = gsub("(.*/)", "", file)
  title = as.character(gsub("(.csv$)","",fname))
  
  #Read in entire file
  df = read.csv(file, header = TRUE)
  
  #Condition to check whether datasheet has sleep information (some files do not)
  if(length(df$SleepPhaseMins) == 0 ){
    #Condition for when there is no additional sleep information
    #print("This is null.") #test statement
    
    # Create summary time series charts only
    data = subset(df, select = c(timestamp, heartrate, steps, 
                                 calories, gsr, skintemp, airtemp))
    
    # Convert data to a readable timestamp format
    data$timestamp = as.POSIXct(data$timestamp, format="%m/%d/%Y %H:%M")
    
    #Rename columns for resultant pdf publication clarity
    colnames(data) <- c("timestamp", "Heart Rate", "Steps","Calories","Galvanic Skin Response",
                        "Skin Temperature", "Air Temperature")
    
    #Designate time as the common axis across all graphs
    df_melt = melt(data, id.vars = 'timestamp')
    
    #Create a grid plot that shows separate plots for each measure 
    # (e.g., "Heart Rate", "Steps","Calories","Galvanic Skin Response", "Skin Temperature", "Air Temperature")
    #Scale is adjusted for viewability and aesthetic reasons.
    myPlot = ggplot(df_melt, aes(x = timestamp, y = value)) + 
      geom_point(size = 1) +
      facet_wrap(~ variable, scales = 'free_y', ncol = 1)+
      ggtitle(title) +
      labs(x="Time",y="Values") +
      scale_x_datetime(breaks = date_breaks("1 day"), minor_breaks = date_breaks("4 hour"))+
      theme(axis.text.x = element_text( angle = 45, hjust = 1))

    #add pdf extension for PDF output
    title = paste(title, ".pdf", sep = "")
    
    
    #export and view graphic within RStudio
    pdf(title, height=10, width = 14)
    print(myPlot)
    dev.off()
    
  } else {
    #Create graphic that includes sleep summary
    #Same as above- except the sleep summary information is added
    data = subset(df, select = c(timestamp, heartrate, steps, 
                                 calories, gsr, skintemp, airtemp))
    
    data$timestamp = as.POSIXct(data$timestamp, format="%m/%d/%Y %H:%M")
    
    
    colnames(data) <- c("timestamp", "Heart Rate", "Steps","Calories","Galvanic Skin Response",
                        "Skin Temperature", "Air Temperature")
    
    ##Additional Calculations##
    #Include AVERAGE TABLE for sleep data that is available
    Avg_Hours = mean(df$SleepPhaseMins[!is.na(df$SleepPhaseMins)])/60 #hours
    Days = length(df$SleepPhaseMins[!is.na(df$SleepPhaseMins)]) #number of days recorded
    #avg light
    LightSleep = mean(df$Light_Sleep_Percentage[!is.na(df$Light_Sleep_Percentage)])*100
    #avg rem
    REMSleep = mean(df$REM_Sleep_Percentage[!is.na(df$REM_Sleep_Percentage)])*100
    #avg deep
    DeepSleep = mean(df$Deep_Sleep_Percentage[!is.na(df$Deep_Sleep_Percentage)])*100
    #avg tossturn
    TossTurns = mean(df$Tosses_and_Turns[!is.na(df$Tosses_and_Turns)])
    #avg interruptions
    Avg_Interruptions = mean(df$Interruptions[!is.na(df$Interruptions)])
    
    #avg calories burned while sleeping
    Avg_calories = mean(df$Calories[!is.na(df$Calories)])
    
    #avg heart rate while sleeping
    avg_hr = mean(df$HR_avg[!is.na(df$HR_avg)])
    
    #Create additional table for sleep information display
    avg_data = data.frame(Days, Avg_Hours, LightSleep , REMSleep,
                          DeepSleep, TossTurns, Avg_Interruptions, Avg_calories, avg_hr)
    
    avg_data = format(avg_data, digits=2, nsmall=2)
    
    colnames(avg_data) = c("Days", "Average Hours", "Avg. Light Sleep %", "Avg. REM Sleep %", 
                           "Avg. Deep Sleep %", "Avg. Tosses and Turns", "Avg. Interruptions", 
                           "Avg. Calories", "Avg. Heart Rate")
    
    thm <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
    tabObj = tableGrob(avg_data, theme = thm, rows = NULL)
    
    df_melt = melt(data, id.vars = 'timestamp')
    
    ##Same as above##
    #Create the overlaid grid graphs 
    myPlot =  
      ggplot(df_melt, aes(x = timestamp, y = value)) + 
      geom_point(size = 1) +
      facet_wrap(~ variable, scales = 'free_y', ncol = 1)+
      ggtitle(title) +
      labs(x="Time",y="Values") +
      scale_x_datetime(breaks = date_breaks("1 day"), minor_breaks = date_breaks("4 hour"))+
      theme(axis.text.x = element_text( angle = 45, hjust = 1))

    #add pdf extension
    title = paste(title, ".pdf", sep = "")
    
    
    pdf(title, height=10, width = 14)      
    plots = grid.arrange(myPlot, tabObj, ncol=1, heights = c(9, 1))
    #grid.draw(tabObj)
    print(plots)
    dev.off()
  }
}

########################################################################
#Exclusive Case: Summary graph for file with empty metrics column(s)   #
#Date: 1.11.2016                                                       #
########################################################################


file.names = list.files(path="studyData_issue/", pattern="*.csv", full.names=TRUE, recursive=FALSE)
for(i in 1:length(file.names)){
  file = file.names[i]
  print("File Name")
  print(file)
  fname = gsub("(.*/)", "", file)
  title = as.character(gsub("(.csv$)","",fname))
  
  #Read in entire file
  df = read.csv(file, header = TRUE)
  
  #check whether datasheet has sleep information
  if(length(df$SleepPhaseMins) == 0 ){
    
    #Calories and Steps are missing (likely due to wearable device)
    data = subset(df, select = c(timestamp, heartrate, gsr, skintemp, airtemp))
    
    
    colnames(data) <- c("timestamp", "Heart Rate", "Galvanic Skin Response",
                        "Skin Temperature", "Air Temperature")
    
    data$timestamp = as.POSIXct(data$timestamp, format="%m/%d/%Y %H:%M")
    
    
    
    df_melt = melt(data, id.vars = 'timestamp')
    myPlot = ggplot(df_melt, aes(x = timestamp, y = value)) + 
      geom_point(size = 1) +
      facet_wrap(~ variable, scales = 'free_y', ncol = 1)+
      ggtitle(paste(title, "\n", sep = " ")) +
      labs(x="Time",y="Values") +
      scale_x_datetime(breaks = date_breaks("1 day"), minor_breaks = date_breaks("4 hour"))+
      theme(axis.text.x = element_text( angle = 45, hjust = 1))


    #add pdf extension
    title = paste(title, ".pdf", sep = "")
    
    #export and view graphic
    pdf(title, height=10, width = 14)
    print(myPlot)
    dev.off()
    
  } else {
    
    #Create graphic that includes sleep summary
    #Calories and Steps are missing (likely due to study device issues)
    
    data = subset(df, select = c(timestamp, heartrate, gsr, skintemp, airtemp))
    
    
    colnames(data) <- c("timestamp", "Heart Rate", "Galvanic Skin Response",
                        "Skin Temperature", "Air Temperature")
    
    data$timestamp = as.POSIXct(data$timestamp, format="%m/%d/%Y %H:%M")

    
    #Include AVERAGE TABLE
    Avg_Hours = mean(df$SleepPhaseMins[!is.na(df$SleepPhaseMins)])/60 #hours
    Days = length(df$SleepPhaseMins[!is.na(df$SleepPhaseMins)]) #number of days recorded
    #avg light
    LightSleep = mean(df$Light_Sleep_Percentage[!is.na(df$Light_Sleep_Percentage)])*100
    #avg rem
    REMSleep = mean(df$REM_Sleep_Percentage[!is.na(df$REM_Sleep_Percentage)])*100
    #avg deep
    DeepSleep = mean(df$Deep_Sleep_Percentage[!is.na(df$Deep_Sleep_Percentage)])*100
    #avg tossturn
    TossTurns = mean(df$Tosses_and_Turns[!is.na(df$Tosses_and_Turns)])
    #avg interruptions
    Avg_Interruptions = mean(df$Interruptions[!is.na(df$Interruptions)])
    
    #avg calories burned while sleeping
    Avg_calories = mean(df$Calories[!is.na(df$Calories)])
    
    #avg heart rate while sleeping
    avg_hr = mean(df$HR_avg[!is.na(df$HR_avg)])
    
    avg_data = data.frame(Days, Avg_Hours, LightSleep , REMSleep,
                          DeepSleep, TossTurns, Avg_Interruptions, Avg_calories, avg_hr)
    
    avg_data = format(avg_data, digits=2, nsmall=2)
    colnames(avg_data) = c("Days", "Average Hours", "Avg. Light Sleep %", "Avg. REM Sleep %", 
                           "Avg. Deep Sleep %", "Avg. Tosses and Turns", "Avg. Interruptions", 
                           "Avg. Calories", "Avg. Heart Rate")
    
    thm <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
    
    tabObj = tableGrob(avg_data, theme = thm, rows = NULL)
    
    df_melt = melt(data, id.vars = 'timestamp')
    myPlot =  
      ggplot(df_melt, aes(x = timestamp, y = value)) + 
      geom_point(size = 1) +
      facet_wrap(~ variable, scales = 'free_y', ncol = 1)+
      ggtitle(paste(title, "\n", sep = " ")) +
      labs(x="Time",y="Values") +
      scale_x_datetime(breaks = date_breaks("1 day"), minor_breaks = date_breaks("4 hour"))+
      theme(axis.text.x = element_text( angle = 45, hjust = 1))
    
    #add pdf extension
    title = paste(title, ".pdf", sep = "")
    
    pdf(title, height=10, width = 14)      
    plots = grid.arrange(myPlot, tabObj, ncol=1, heights = c(9, 1))
    print(plots)
    dev.off()
  }
}
