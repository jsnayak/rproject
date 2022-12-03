################################# 
# You can use this template to draft the script for your Assessment 2.
# More clarification and related resources can be found at
# https://d2l.deakin.edu.au/d2l/le/content/1191557/viewContent/6267637/View
#################################

#############################################################################################
# save your code as "name-code.R" (where “name” is replaced with your surname or first name).
#############################################################################################

##################################
#Question 1 - Understand the Data
##################################

# ----------------------------- FUNCTIONS ---------------------------------
# Read the forest input file and set the needed column names
import_forest_data <- function(){
  forest_data <- read.table("Forest_2022.txt", 
                            quote="\"", 
                            comment.char="")
  colnames(forest_data) <- column.name
  print(paste("import_forest_data: Col Num:", length(forest_data), 
              "Row Num:", nrow(forest_data)))
  
  return(forest_data)
}

# Fetch the sample set of the data as per specified sample size
# Seed to be used if needed before sample function is invoked
sample_forest_data <- function(data, sample_size, transformed.file, 
                               seed_value = 1, set_seed = FALSE){
  if(set_seed == TRUE){
    set.seed(seed_value)
  }
  
  data.sampleset <- data[sample(1:nrow(data), 
                                sample_size, 
                                replace=FALSE), 
                         c(1:length(data))]
  print(paste("sample_forest_data: Col Num:", length(data.sampleset), 
            "Row Num:", nrow(data.sampleset)))

  write.table(data.sampleset, transformed.file)
  return(data.sampleset)
}

# Store the summary of the dataset
summary_data <- function(data, summary.file){
  summary.data <- summary(data)
  write.table(summary.data, summary.file)
}

# Plot Feature Data Histogram
plot_features_histogram <- function(data, col.name, col.number, mfrow=c(1,1)){
  # Save Feature Data Histogram
  for(i in col.number){
    jpeg(file=paste(col.name[i],"_hist.jpeg", sep=""))
    hist(x=unlist(data[col.name[i]]), 
         main=paste("Histogram: " , col.name[i]),
         xlab=col.name[i])
    dev.off()
  }

  # See all histograms in one display
  par(mfrow=mfrow)
  
  # Plot columns as specified
  for(i in col.number){
    hist(x=unlist(data[col.name[i]]), 
         main=paste("Histogram: " , col.name[i]),
         xlab=col.name[i],
         pch=16)
  }
}


# Plot Feature Data Box Plots
plot_features_box <- function(data, col.name, col.number, mfrow=c(1,1)){
  # Plot columns as specified and save them
  for(i in col.number){
    jpeg(file=paste(col.name[i],"_boxplot.jpeg", sep=""))
    boxplot(data[,i], 
         ylab=col.name[i], 
         main=paste("Box Plot:" , col.name[i]),
         pch=16)
    dev.off()
  }
  
  # See all histograms in one display
  par(mfrow=mfrow)
  
  # Plot columns as specified
  for(i in col.number){
    boxplot(data[,i], 
         ylab=col.name[i], 
         main=paste("Box Plot:" , col.name[i]),
         pch=16)
  }
}

# Plot Feature Data Scatter Plots
plot_features_scatter <- function(data, col.name, 
                                  col.number, ycol,  
                                  mfrow=c(1,1), save.image=TRUE){
  # Plot columns as specified and save them
  if(save.image){
    for(i in col.number){
      jpeg(file=paste(col.name[i],"_scatter.jpeg", sep=""))
      plot(data[,ycol]~data[,i],
           xlab=col.name[i],
           ylab="Y: Burned Area",
           main=paste("Scatter Plot:" , col.name[i], "/ Y Burned Area"),
           pch=16)
      abline(lm(data[,ycol]~data[,i]), col="blue")
      dev.off()
    }
  }

  # See all histograms in one display
  par(mfrow=mfrow)

  # Plot columns as specified
  for(i in col.number){
    plot(data[,ycol]~data[,i],
         xlab=col.name[i],
         ylab="Y: Burned Area",
         main=paste("Scatter Plot:" , col.name[i], "/ Y Burned Area"),
         pch=16)
    abline(lm(data[,ycol]~data[,i]), col="blue")
  }
}

# Plot Pair Plot
plot_features_pairplot <- function(data, sequence, file_name){
  jpeg(file_name)
  pairs(data[,sequence],panel = panel.smooth)
  dev.off()
  pairs(data[,sequence],panel = panel.smooth)
}

# ----------------------------- OPERATIONS ---------------------------------
# Set Working Directory
setwd("C:/JSN/Deakin/RWA/R-Basics/Midterm1/midterm/")

# Keep Column Names Ready
column.name <- c("x1.XAxis", "x2.YAxis", "X3.Month", "X4.Day", 
                "x5.FFMC", "x6.DMC", "x7.DC", "x8.ISI", 
                "x9.Temp", "x10.RH", "x11.Wind", "x12.Rain",
                "y.BurnedArea")


# Complete the data read operation
# Read data is stored in variable forest.rawdata
forest.rawdata <- import_forest_data()

# Fetch sample set of size 330
# Setting seed value is optional
# Sample data is written to file name provided as input
forest.sampleset <- sample_forest_data(forest.rawdata, 
                                       330, 
                                       "sample-data.txt",
                                       TRUE,
                                       223143728)

# Use scatter plots and histograms to understand the relationship between each of the 
# variables X5, X6, X7, X8, X9, X10, X11, X12, and your variable of interest Y.

# Plot Pair Plot
plot_features_pairplot(forest.sampleset,
                       c(5:13),
                       "forest_pairplot.jpg")

# Create 9 histograms for each X variable and Y
# Plot the 8 features histogram
plot_features_histogram(forest.sampleset,
               column.name,
               c(5:12),
               c(2,4))

# Plot the Y column histogram
plot_features_histogram(forest.sampleset,
                        column.name,
                        c(13))

# Create 8 scatterplots function (for each X variable against the variable of interest Y) 
# Plot Scatter Plots
plot_features_scatter(forest.sampleset,
                      column.name,
                      c(5:12),
                      13,
                      c(2,4),
                      TRUE)

# Create 8 box  function for each X variable
plot_features_box(forest.sampleset,
                  column.name,
                  c(5:12),
                  c(2,4))



################################
#Question 2 - Transform the Data
################################
# ----------------------------- FUNCTIONS ---------------------------------

# Detect and Cap Outliers
treat_outlier <- function(data){
  quartile.25 <- quantile(data, probs=c(.25))
  quartile.75 <- quantile(data, probs=c(.75))
  iqr <- IQR(data)
  UL <- quartile.75 + (1.5 * iqr)
  LL <- quartile.25 - (1.5 * iqr)
  data[data < LL] <- LL
  data[data > UL] <- UL
  return (data)
}


# Create Chosen data columns and store in file
create_model_dataset <- function(data, columns){
  selected.data = data[,columns]
  return (selected.data)
}

# Perform Linear Transformation on Selected Column
linear_feature_scaling <-function(data, col){
  data[,col] <- (data[,col] - min(data[,col]))/
    (max(data[,col]) - min(data[,col]))
  return(data[,col])
}

# Perform Standard Transformation to unit interval on Selected Column 
zscore.unitinterval <-function(data, col){
  data[,col] <- 0.15* ((data[,col] - mean(data[,col]))/sd(data[,col])) + 0.5
  return(data[,col])
}

# Perform Log Transformation on Selected Column
log_feature_scaling <-function(data, col){
  data[,col] <- log(data[,col])
  return(data[,col])
}

# Complete Data Transformations on each column
transfrom_forest_data <- function(data, transform.function){
  data[,1] = transform.function[[1]](data, 1)
  data[,2] = transform.function[[2]](data, 2)
  data[,3] = transform.function[[3]](data, 3)
  data[,4] = transform.function[[4]](data, 4)
  return (data)
}

# ----------------------------- OPERATIONS ---------------------------------

# Chosen Data Columns
v1 <- c("x5.FFMC", "x7.DC", "x8.ISI", "x9.Temp", "y.BurnedArea")

# Create model data set
model.data.v1 = create_model_dataset(forest.sampleset, v1)

# for each variable, you need to figure out a good data transformation method, 
# such as Polynomial, log and negation transformation. The k-S test and Skewness 
# calculation may be helpful to select the transformation method

# you need to manually check outliers and impute or remove them with reasonable 
# judgement, before applying any transformation
for (i in c(1:4)){
  outlier.stats = boxplot.stats(model.data.v1[,i])
  print(paste("Number of Outlier:", length(outlier.stats$out)))
  if (length(outlier.stats$out) > 0){
    model.data.v1[,i] = treat_outlier(model.data.v1[,i])
  }
}

plot_features_scatter(model.data.v1,
                      v1,
                      c(1:4),
                      5,
                      c(2,2),
                      FALSE)

transformed.data.v1 <- transfrom_forest_data(model.data.v1, 
                      list(zscore.unitinterval, zscore.unitinterval,
                        zscore.unitinterval, zscore.unitinterval))

plot_features_scatter(model.data.v1,
                      v1,
                      c(1:4),
                      5,
                      c(2,2),
                      FALSE)

# Write the transformed data into output file
head(model.data.v1)
write.table(model.data.v1, "jnayak-outlier-clean-data.txt")
write.table(transformed.data.v1, "jnayak-transformed1.txt")
summary_data(model.data.v1, "summary_model-sampleset1.txt")


##########################################
#Question 3 - Build models and investigate
##########################################

source("AggWaFit718.R")

# import saved transformed data that will be used to create models
data.transformed_copy <- as.matrix(read.table("jnayak-transformed1.txt"))  

head(data.transformed_copy)

plot_features_scatter(data.transformed_copy,
                      v1,
                      c(1:4),
                      5,
                      c(2,2),
                      FALSE)

# Get weights for Weighted Arithmetic Mean with fit.QAM() 
fit.QAM(data.transformed_copy[,c(1:4,5)],
        "WAMoutput_Forest.txt", "WAMstats_Forest.txt", 
        g=AM, g.inv= invAM)


# Get weights for Power Mean p=0.5 and p=2 with fit.QAM()
fit.QAM(data.transformed_copy[,c(1:4,5)],
        "PowerMean0.5Output.txt", 
        "PowerMean0.5Stats.txt", 
        g=PM05, g.inv= invPM05)

fit.QAM(data.transformed_copy[,c(1:4,5)],
        "QuadraticMeanOutput.txt", 
        "QuadraticMeanStats.txt", 
        g=QM, g.inv= invQM)

# Get weights for Ordered Weighted Average with fit.OWA()
fit.OWA(data.transformed_copy[,c(1:4,5)],
        "OrderedWeightedAvgOutput.txt", 
        "OrderedWeightedAvgStats.txt")


# Get weights for Choquet Integral with fit.choquet() - Optional
fit.choquet(data.transformed_copy[,c(1:4,5)],
            "choquet4ColOutput.txt", 
            "choquet4ColStats.txt")


#######################################
#Question 4 - Use Model for Prediction
#######################################
# new_input has X5=96.1; X6=181.1; X7=671.2; X8=14.3; X9=20.7; X10=69; X11=4.9; X12=0.4

# new_input_to_transform <- c("choose the same four X variables as in Q2")
# Selected Parameters: ("x5.FFMC", "x7.DC", "x8.ISI", "x9.Temp")
value.to.predict <- c(96.1, 671.2, 14.3, 20.7)
value.to.predict

# 7 5 10 6 92.8 73.2 713 22.6 19.3 38 4 0 0.0125730500011219


# import saved transformed data that will be used to create models
data.cleansample <- as.matrix(read.table("jnayak-outlier-clean-data.txt"))  

head(data.cleansample)

# transforming the four variables in the same way as in question 2 
value.to.predict[1] <- 0.15* ((value.to.predict[1] - 
                                 mean(data.cleansample[,"x5.FFMC"]))/
                                sd(data.cleansample[,"x5.FFMC"])) + 0.5
value.to.predict[2] <- 0.15* ((value.to.predict[1] - 
                                 mean(data.cleansample[,"x7.DC"]))/
                                sd(data.cleansample[,"x7.DC"])) + 0.5
value.to.predict[3] <- 0.15* ((value.to.predict[1] - 
                                 mean(data.cleansample[,"x8.ISI"]))/
                                sd(data.cleansample[,"x8.ISI"])) + 0.5
value.to.predict[4] <- 0.15* ((value.to.predict[1] - 
                                 mean(data.cleansample[,"x9.Temp"]))/
                                sd(data.cleansample[,"x9.Temp"])) + 0.5

# applying the transformed variables to the best model selected from Q3 for Y prediction

# Get weights for Weighted Arithmetic Mean with fit.QAM() 
WAM.Weights = c(0,0.99999999999998,0,0)
PM.05.Weights = c(0,0.999999999999909,0,0)
PM.2.Weights = c(0,0,1,0)
OWA.Weights = c(1,0,0,0)
Choquet.Weights = c(0.249999999999993, 0.249999999999993, 0.249999999999993, 0.249999999999993)

# Reverse the transformation to convert back the predicted Y to the original scale and then round it to integer

QAM(value.to.predict, WAM.Weights)
QAM(value.to.predict, PM.05.Weights)
QAM(value.to.predict, PM.2.Weights)
OWA(value.to.predict, OWA.Weights)
choquet(value.to.predict, Choquet.Weights)


# Compare your prediction with the measured value of Y, Y=0.0146.

