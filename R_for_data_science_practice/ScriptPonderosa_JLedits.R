# Forest canopy response to different experimental management regimes
# Ecology Lab 8: Montane forest ecology
# 09 March 2017
# Ecology lab class Spring 2017

# Step 1: Save a .csv file of the class data to your working directory. 

#################
#Import the data#
#################
# Below are two options for importing a .csv file. 
# Both create an R object named "mydat" based on the .csv file containing our data

# The first option, "file.choose", does not need to be altered. 
# Just uncomment and highlight the line to run.  
# It will open your working directory where you can select your file to upload. 
mydat<-read.csv(file.choose())

# The second option requires you to enter the name of the file you are trying to upload. 
# Because you have already set the working directory (the folder to look in), all you need is the file name.
mydat<-read.csv("Ponderosa_All_Data.csv",header=T)

# Tip: You can check out your Global Environment to see if you have successfully uploaded the data. 

##################
#Explore the data#
##################
# Examine the structure of the dataset, which is now named "mydat"
# This helps to make sure that everything is set up correctly
object1<-str(mydat)

### This code shows you the first lines of an object
head(mydat)

#YOUR TURN: What type of object is mydat?  (Hint: This is the first term you see when you call str(mydat)
#YOUR TURN: How many rows does mydat have? (Hint: This is your number of observations (obs.) )
#YOUR TURN: What type of values are in the nativeRich column?  (Hint: This is found in the str(mydat) output)

# Before we get into it, let's get a feel for R's indexing format

# R uses a [row, column] indexing format 
# to get a feel for it, try calling the first 8 rows of the second column
mydat[1:8,2]

# You can call the same thing using the name of the second column
mydat[1:8, "Semester"]

# Or you can specify the column with a $, then select rows
mydat$Semester[1:8]

###############
#DATA ANALYSIS#
###############

#### One-way ANOVA####

# Run an ANOVA to measure the statistical differences between your treatments
# The following code is running an "analysis of variance" or ANOVA. 
# The symbol "~" means "as predicted by." 
# So here we are looking at "canopy cover as predicted by treatment"
# The code "data=mydat" tells R to use the datasheet "mydat" that you previously loaded
anova_model1 <- aov(Cover~Treatment, data=mydat)

# View the results of the ANOVA
# If this summary reports a 'significant' difference with p <0.05, 
# that means that at least two of the treatment types have significantly different 
# canopy cover values
summary(anova_model1)

#### Do a follow-up test to help interprete the ANOVA results 
###This test, the Honestly Significant Difference test, compares each pair of
###treatments to each other to isolate the comparisons of greatest interest.  
TukeyHSD(anova_model1)

###This code breaks the datasheet object "mydat" down into four separate
### objects based on treatment type
Reference <- subset( mydat, mydat[,5] == 'Reference')
Thinned <- subset( mydat, mydat[,5] == 'Thinned')
ThinnedandBurned <- subset( mydat, mydat[,5] == 'ThinnedandBurned')
Burned <- subset( mydat, mydat[,5] == 'Burned')

###This code provides some descriptive stats for the different treatment groups
summary(Reference)
summary(Thinned)
summary(ThinnedandBurned)
summary(Burned)

####################
#Visualize the data#
####################

### Display the data with a boxplot
boxplot(Cover~Treatment,  data=mydat)

### Display the data with a notched box plot
# If the notches, don't overlap this suggests that the treatments are different
# You can see that none of the notches overlap, this agrees with the results of the TukeyHSD

boxplot(Cover~Treatment, notch=T, data=mydat)

### Now we are going to produce a boxplot with color,
# and appropriate title and axis labels.
# You can use color to help describe your data, here green is reference,
# and higher disturbance gets a darker red
# Visit http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf for full R color list

### HIGHLIGHT all lines at once when running multiple lines of code

boxplot(Cover~Treatment, data=mydat,
        col=c("firebrick4","orange3", "yellow", "green"),
        main="Effect of fire management technique on canopy cover", 
        xlab="Fire treatment type",
        ylab= " Canopy cover (%)",
        names=c("Burned","Reference","Thinned","Thinned & Burned"))


####################
#Visualize the data#
####################
# First we need to calculate mean values for a bargraph
# Keep in mind that it is always important to have meaningful object names. 
# The following "CovMean" is shorthand for the "mean of cover"

CovMean<-aggregate(Cover~Treatment, data=mydat, mean)



#now look at the object you created
str(CovMean)

# In R, the <- nomenclature is used to define the name given on the left side
# Here, CovMean is defined as aggregate(Cover~Treatment, data=mydat, mean)   

# Next we need to calculate the standard error of the mean 
# se = sqrt(variance/n) = standard deviation/sqrt(n) 
# where n is sample size
# There is no built-in function in R for standard error
# So let's write one!

## To run this custom code, select everything from calcSE to the last } symbol

calcSE<-function(x){
    sd(x)/sqrt(length(x))}

# Now we'll apply our calcSE function to our data
CovSE<-aggregate(Cover~Treatment, data=mydat, calcSE)

# Look at the object you just created, which is the standard error of cover.
CovSE

## Let's graph it!
# To add standard error bars we need a package called "plotrix"
# You only need to run the "install.packages" command once 

install.packages("plotrix")

#But you do have to load the package each session you use it

library(plotrix)

# As a demo, let's first just plot the graph without error bars. Highlight all of the following three lines and run it.
barplot(CovMean[,2], beside=T, ylim=c(0,60), 
        ylab="Tree Canopy Cover (%)", xlab="Treatment",
        col=c("firebrick4","orange3", "yellow", "green"),
        names.arg=c("Burned", "Reference", "Thinned", "Thinned & Burned"))

# Now let's add the standard error bars
# Note how the barplot command is included inside the plotCI command
plotCI(barplot(CovMean[,2], beside=T, ylim=c(0,60
                                             ), 
               ylab="Tree Canopy Cover", xlab="Treatment", 
               col=c("firebrick4","orange3", "yellow", "green"),
               names.arg=c("Burned", "Reference", "Thinned", "Thinned & Burned")),
       CovMean[,2],uiw=CovSE[,2],add=T, pch=NA)



########
#INDEPENDENT EXERCISE
########

#What happens if we only look at the FALL data (i.e. your data!)?

#Let's pull out only data from the fall using the 'subset' function (pulls out a subset of data)
fall <- subset(mydat, Semester =="Fall")
str(fall)

#Now YOU write the code to use One-Way ANOVA and Tukey's test
#Just like you did above, you will try to predict cover as a function of treatment
#But NOW you will use only the 'fall' data

#Give it a go!
anova_model1 <- aov(Cover~Treatment, data=fall)
aov(Cover~Treatment, data=fall)
anova_model2 <- aov(Cover~Treatment, data=fall)
summary(anova_model2)
