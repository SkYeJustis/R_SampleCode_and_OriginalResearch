########################################################################
# Programatically adding variables based on a key for variable groupings
########################################################################

# Instructive data example
Data = read.csv("SampleCategoryTest.csv", header = TRUE, sep = ",")

# Key for variable groupings
CatKey = read.csv("CategoryKey.csv", header = TRUE, sep = ",")

# Making sure that the names of the variables are the same
## Note: Original variables have spaces
colnames(Data) = gsub(".", "", colnames(Data), fixed = TRUE)
CatKey$Category = gsub(" ", "", CatKey$Category, fixed = TRUE)

# Merging the list of variables to get a list of groupings that correspond to data
## Note: Real-world data may have differing var names in the dataset and the grouping key
cols = as.data.frame(colnames(Data))
colnames(cols) = "Category"
mrg = merge(cols, CatKey, by = c("Category"))


# For loop to add variables based on category key
subs = NULL
for (i in 1:length(unique(mrg$MajorCategory))) {
#for (i in 1:2) {
  print(unique(mrg$MajorCategory)[i])
  #Get col names based on key grouping
  subs = subset(CatKey, 
                MajorCategory == as.character(unique(mrg$MajorCategory)[i]))
  # Sum the columns based on the relevant columns
  if (nrow(subs) == 1){
    Data[, 
         as.character(unique(mrg$MajorCategory)[i])] = Data[,subs$Category[i]]
  } else {
    Data[, 
         as.character(unique(mrg$MajorCategory)[i])] = rowSums(Data[,c(subs$Category)])
  }
}

# Relevant to sample data to check results
table(mrg$MajorCategory)

#Check the results for the appended data in the resulting dataframe
Data
