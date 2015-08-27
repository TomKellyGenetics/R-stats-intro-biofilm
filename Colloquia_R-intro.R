# '#' marks comments for people to explain code
# set up R: https://cran.r-project.org/
# set up RStudio: https://www.rstudio.com/products/rstudio/download/

# tell the computer where to find your files in the "working directory"
setwd("~/Downloads/Help")

#import files: from "csv" spreadsheet (can save these from excel)
biofilm_data1 <- read.csv(file="BIOFILM COUPON ASSAYS 1.csv", header=T, skip=1)
?read.csv #shows the "help" options for the 'read.csv' command (press 'q' to return to R)

#or this method works well for large and complex data tables (automatically detects many settings)
install.packages("data.table") # install package (first time on each computer)
library("data.table") # load package (every use in new R session)
biofilm_data1<-data.table::fread("BIOFILM COUPON ASSAYS 1.csv", skip=1, data.table=F)

#export data
write.csv(biofilm_data1, file="BIOFILM COUPON ASSAYS 1 (R copy).csv") #export to csv (can open in excel)
save(biofilm_data1, file="BIOFILM_COUPON_ASSAYS_1.RData") #save in R format, quick and easy to load again (note we avoid spaces in names with R):
load(file="BIOFILM_COUPON_ASSAYS_1.RData")
save.image(file="BIOFILM_COUPON_ASSAYS_1.RData") # saves all ojects in the 'workspace':
ls() #see current objects

#See what data looks like
biofilm_data1 #call datafile
head(biofilm_data1) #see top of file (check columns and labels correct, etc)
tail(biofilm_data1) #see bottom of file (quick check of file end, esp if modified)
dim(biofilm_data1) #show size of dataset
#Could also extract specific columns or rows
biofilm_data1[1,] #row 1
biofilm_data1[,2:3] #columns 2 to 3
biofilm_data1[c(1,4,5:7),] #rows 1,4,5,6,7 run to check:
c(1,4,5:7)
#useful to see what the data format is (good to tell people helping you this or to search google for errors)
str(biofilm_data1)
class(biofilm_data1)
class(biofilm_data1[, 1])
#note the structure of the data: CFU is continuous measure, the others are discrete (1 = treated, 0 = not). So controls have both 0s.
biofilm_data1$Alginate.Lyase #dataframe also allow extracting columns by name

#t-test: formula of column names, data for which object to use
t.test(CFU.LOG ~ Alginate.Lyase, data=biofilm_data1) #CFU (continuous) is predicted by Alginate (discrete class)
t.test(CFU.LOG ~ Gentamicin, data=biofilm_data1)
t.test(CFU.LOG ~ Gentamicin, data=biofilm_data1)$p.value #extract p-value if very small
#note t-tests only work for comparing two classes 

#boxplot: same formula method works
boxplot(CFU.LOG ~ Alginate.Lyase, data=biofilm_data1) 
boxplot(CFU.LOG ~ Gentamicin, data=biofilm_data1) # here the difference shown by the t-test is clear - basic plots aren't pretty but good to 'eyeball' the data

#we can customise plots in R extensively (here's an example of a more complex plot, this is extension material, publication worthy plots in R are possible but not the focus today: check out the 'ggplot2' if interested in more)
boxplot(CFU.LOG ~ Gentamicin, data=biofilm_data1, main="title", ylab="CFU measure", xlab="Gentamicin Treatment", names=c("control", "treated"), col=c("red", "blue"))
#add legend
legend(x=0, y=4.5, fill=c("red", "blue"), legend=c("control", "treated")) # either specific location...
legend(x="bottomleft", fill=c("red", "blue"), legend=c("control", "treated")) #or oriented to corner/side
#add significance bar/star (literally straight off google/stackoverflow)
par(xpd=TRUE)
yrange<-par("usr")[3:4] #extract dimensions of plot
ypos<-yrange[2]-diff(yrange)/50 #height of comparison bar
segments(1,ypos,2,ypos) #columns to mark sign.
text(x=1.5,ypos+diff(yrange)/100,"***",cex=1) #mark stars half way between bar and top of chart (cex is star size, x is halfway between columns to centre stars)
par(xpd=FALSE)
#R can make many different plots: scatter, line, heatmap, circos... plenty of help and examples on the web
plot(x=biofilm_data1[1:20, 1], y=biofilm_data1[21:40, 1]) #e.g. scatter for continuous data

#we can compare discrete variables
Alginate <- biofilm_data1$Alginate.Lyase
Gentamicin <- biofilm_data1$Gentamicin
table(Alginate) # table is a good summary of how many of each class there are (for any discrete object)
table(Alginate, Gentamicin) #table can compare multiple discrete variables
#both of these tests are useful for tables
fisher.test(table(Alginate, Gentamicin)) #Fisher's Exact test is more accurate for small sample size
chisq.test(table(Alginate, Gentamicin)) #Chi-Squared works on more complex tables (variables with more classes)
#Chi-Squared works just like we did in undergrad
chisq.test(table(Alginate, Gentamicin))$obs #observed table
chisq.test(table(Alginate, Gentamicin))$exp #expected table
chisq.test(table(Alginate, Gentamicin))$p.value

#fit a linear model: formula of column names, data for which object to use
fit <- lm(CFU.LOG ~ Alginate.Lyase, data=biofilm_data1) #here CFU is predicted by Alginate lyase
anova(fit) #then computing ANOVA is only 2 lines!
summary(fit) #other information about the model (incl. R-squared)
fit
#here predictors can also be continuous (e.g. dosage effects)

#modifications: add predictor
fit <- lm(CFU.LOG ~ Alginate.Lyase + Gentamicin, data=biofilm_data1)
anova(fit)
summary(fit)
#modifications: test interactions (requires some to be treated with both predictors)
fit <- lm(CFU.LOG ~ Alginate.Lyase * Gentamicin, data=biofilm_data1)
anova(fit)
summary(fit)

#repeat this analysis on another dataset: read new dataset and repeat above commands with new column names
biofilm_data2 <- read.csv(file="BIOFILM COUPON ASSAYS 2.csv", header=T, skip=1)
