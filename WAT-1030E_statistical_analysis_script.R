##############################################################
###                                                        ###
### STATISTICAL ANALYSIS OF GLOBAL DEMOGRAPHIC DATA WITH R ###
###                                                        ###
##############################################################

# Original script 2016 by Timo Räsänen. Revised 2017 by Joseph Guillaume

#INTRODUCTION TO EXERCISE ----

#This exercise has four parts: 
#1. Reading data into R
#2. Plotting data in R with ggplot2
#3. Correlation analysis
#4. Regression analysis
#5. Descriptive analysis of variances 

#Follow the given exercises instructions and the instructions in the R script.
#In some exercises, script is missing and you need to fill in. 
#In the end provide a brief report which answers the questions given for each exercise. 


#DATA USED IN THE EXERCISE ----

#Population [thousands]. Source: United Nations. 
#Life expectancy [years]. Source: United Nations.
#Under five year mortality [deaths under age five per 1,000 live births]. Source: United Nations. 
#Gross domestic product (GDP) per capita [USD]. Source: United Nations.
#Human development index (HDI) [-]. Source: United Nations.


#HELP WITH WRITING R SCRIPTS ----

#Help is available in the lower right window (e.g. search for a function by name).
#Internet provides solutions for many problems. Google 'function name' and 'r'
# There are often answers on Stackoverflow: https://stackoverflow.com/documentation/r/topics


#START WITH THIS ----

#1. Open RStudio
#2. Create a new R project from Version Control using Git (File -> New project -> Version Control -> Git), 
#3. Open R script: WAT-1030E_statistical_analysis_script.R


#RUNNING THE R SCRIPTS: ----

#By line: Place the prompt on the line you want to run, and press 'Run' in the upper right corner of this window
#or press Ctrl+Enter.
#Several lines: Paint the lines and press 'Run' in the upper right corner of this window or press Ctrl+Enter.


#EXPORTING PLOTS FROM R ----

#Lower right window -> Plots -> Export -> Save as image/Save as PDF



##############################################################################################################
#1. READING DATA INTO R ----
#   READ POPULATION, LIFE EXPECTANCY AND UNDER FIVE YEAR MORTALITY data FROM CSV FILES 
#   INTO R DATA.FRAME OBJECTS.
##############################################################################################################

#Read the first few lines to see what the data looks like
readLines("data/Population_UN.csv",n=5)

# Set arguments for 'read.csv' function: read header (header), value separator (sep), decimal separator
#(dec). Then read data from 'Population_UN.csv', 'Life_expectancy_UN.csv' and 'Under_five_year_mortality_UN.csv'
#into R data.frame objects. 
population <- read.csv("data/Population_UN.csv", header = TRUE, sep = ";",dec = ".") 
# MISSING SCRIPT. ----
life_expectancy
under_five_year_mortality


#Examine the read data
population
head(population)
str(population)
summary(population)
# MISSING SCRIPT.----
# check that life_expectancy and under_five_year_mortality have also been read in correctly



###############################################################################################################
#2. PLOTTING WITH GGPLOT2 ----
#   PLOT POPULATION, LIFE EXPECTANCY AND FIVE YEAR MORTALITY data FOR WORLD, AND HIGH, 
#   MIDDLE AND LOW INCOME COUNTRIES.
###############################################################################################################


#Open ggplot2 library (from ggplot2 package, already installed in Aalto computers)
# For help on ggplot2, see: http://ggplot2.tidyverse.org/
library(ggplot2)

#Plot population data
ggplot(data=population)+
  #"aes" specifies an "aesthetic mapping" describing how visual properties (aesthetics) relate to variables/data values
  #"geom" specifies the geometric object to be used, here lines
  geom_line(aes(x=Year,y=World,color="World"))+
  geom_line(aes(x=Year,y=High_income_countries,color="High_income_countries"))+
  geom_line(aes(x=Year,y=Middle_income_countries,color="Middle_income_countries"))+
  geom_line(aes(x=Year,y=Low_income_countries,color="Low_income_countries"))+
  #override defaults
  ylab("Population [thousands]")+
  labs(title="Population")+
  scale_x_continuous(breaks= seq(1950, 2015, by = 10))


# MISSING SCRIPT. ----
# Plot life expectancy data similarly to population data. Use an x-axis scale 1960-2014 instead of 1950-2015.











 

#Plot under five year mortality data
ggplot(data=under_five_year_mortality)+
  geom_point(aes(x=Period,y=World,color="World"))+
  geom_point(aes(x=Period,y=High_income_countries,color="High_income_countries"))+
  geom_point(aes(x=Period,y=Middle_income_countries,color="Middle_income_countries"))+
  geom_point(aes(x=Period,y=Low_income_countries,color="Low_income_countries"))+
  ylab("Under five year mortality [deaths under age five per 1,000 live births]")+
  labs(title="Under five year mortality")+
  # see help("scale_color_brewer")
  scale_color_brewer(type = "qual")+
  theme(axis.text.x=element_text(angle=90)) #rotate x-axis labels



################################################################################################################
#3.  CORRELATION ANALYSIS ----
#    EXAMINE THE RELATIONSHIP OF GROSS DOMESTIC PRODUCT (GDP) WITH LIFE EXPECTANCY AND 
#    UNDER FIVE YEAR MORTALITY. USE PEARSON'S AND KENDALL'S CORRELATIONS.
################################################################################################################


#READ IN NEW COUNTRY LEVEL DATA

#Read data
under_five_year_mortality_all  <- read.csv("data/Under_five_year_mortality_all_countries_UN.csv", header = TRUE, sep = ";",dec = ".")
life_expectancy_all  <- read.csv("data/Life_expectancy_all_countries_UN.csv", header = TRUE, sep = ";",dec = ".")
gdp_all  <- read.csv("data/GDP_all_countries_UN.csv", header = TRUE, sep = ";",dec = ".")



#GDP AND LIFE EXPECTANCY


#Check relationship between correlation variables. Plot scatter plot.
plot(gdp_all[,"X2011"],life_expectancy_all[,"X2011"], main="Life expectancy vs. GDP", ylab="Life expectancy [years]", xlab="GDP [USD per capita]")


#Run correlation tests for GDP and life expectancy using Pearson's and Kendall's correlations. Use data from year 2011.
cor.test(gdp_all[,"X2011"], life_expectancy_all[,"X2011"], method = c("pearson")) #H0: variables statistically independent, (r=0)
cor.test(gdp_all[,"X2011"], life_expectancy_all[,"X2011"], method = c("kendall")) #H0: variables statistically independent, (tau=0)


#Check normality assumption. Plot histograms and normal probability plots and use 
#Shapiro-Wilk's test to analyse normality.
hist(gdp_all[,"X2011"], xlab="GDP [USD per capita]", main="Gross Domestic Propduct")
hist(life_expectancy_all[,"X2011"], xlab="Life expectancy [years]", main="Life expectancy")

qqnorm(gdp_all[,"X2011"], main="GDP - Normal probability plot");qqline(gdp_all[,"X2011"])
qqnorm(life_expectancy_all[,"X2011"], main="Life expectancy - Normal probability plot");qqline(life_expectancy_all[,"X2011"])

shapiro.test(gdp_all[,"X2011"]) #Shapiro-Wilk's test; H0: data is of normal distribution
shapiro.test(life_expectancy_all[,"X2011"])



#GDP AND UNDER FIVE YEAR MORTALITY. 

#Calculate correlation for GDP and under five year mortality using Pearson and Kendall similarly as for life 
#expectancy. Use data from year 2011. 
# MISSING SCRIPT. ----















################################################################################################################
#4. REGRESSION ANALYSIS: EXAMINE FURTHER THE RELATIONSHIP BETWEEN GROSS DOMESTIC PRODUCT AND LIFE EXPECTANCY
#   USING LINEAR REGRESSION: HOW MUCH GDP EXPLAINS THE VARIATION IN LIFE EXPECTANCY BETWEEN COUNTRIES?
###############################################################################################################

#Open lm library (from zoo package, already installed in Aalto Computers)
library(lmtest)



#DEFINE data FOR LINEAR REGRESSION (y = a + bx)

#Read data into new objects y and x
y <- life_expectancy_all[,"X2011"] #dependent variable
x <- gdp_all[,"X2011"] # independent variable



#ANALYSIS OF DATA

#Plot y and x
plot(y, xlab="Countries", ylab="Life expectancy [years]", main="Life expectancy (y)")
plot(x, xlab="Countries", ylab="GDP [USD per capita]", main="Gross Domestic Product (GDP) (x)")

#Check normality
hist(y, xlab="Life expectancy [years]", main="Life expectancy (y)")
hist(x, xlab="GDP [USD per capita]", main="Gross Domestic Product (GDP) (x)")
shapiro.test(y) #Shapiro-Wilk's test; H0: data is of normal distribution
shapiro.test(x) #Shapiro-Wilk's test; H0: data is of normal distribution

#Check relationship of y and x (linearity)
plot(x, y, ylab="Life expectancy [years]", xlab="GDP [USD per capita]", main= "Relationship")



#TRANSFORMATIONS

#Find appropriate transformation to linearise relationship of y and x
transf_y <- (y)
transf_x <- log10(x) #(for other transformations see for example: http://stattrek.com/regression/linear-transformation.aspx?Tutorial=AP)

#Check normality
hist(transf_y, xlab="Life expectancy [years]", main="Life expectancy (y)")
hist(transf_x, xlab="GDP [USD per capita]", main="Gross Domestic Product (GDP) (x)")

shapiro.test(transf_y) #Shapiro-Wilk's test; H0: data is of normal distribution
shapiro.test(transf_x)

#Check relationship of transf_y and transf_x (linearity)
plot(transf_x,transf_y, ylab="Log10(Under 5-yr mortality)", xlab="HDI", main= "Relationship with transformed data")
harvtest(transf_y ~ transf_x) #Harvey-Collier test; H0: regression is linear

#Fit linear model (regression) to data usin lm function
lm.fit <- lm(transf_y ~ transf_x) 

#Examine estimated model
summary(lm.fit)  
#T-test for slope parameter: H0: slope =0 (i.e. no linear correlation). 
#F-test H0: The fit of the intercept-only model and your model are equal. You can also conclude that if H0 is 
#rejected (p<alpha) the R^2 is different from zero (i.e. there is statistically siginificant relationship between
#the variables).


#REGRESSION DIAGNOSTICS

#Assumptions for adequate regression model:
#1. The relationship of y and x is linear
#2. The residuals are independent. In other words, there is no autocorrelation in residuals.
#3. The residuals follow normal distribution
#4. The residuals have equal/constant variance, in other words, residuals are homoscedastic/not heteroscedastic .


#Test for Independence of model residuals (i.e. lack of autocorrelation)
res <- resid(lm.fit) #get residuals
acf(res) #plot autocorrelation function (acf) to detect autocorrelation with 95% confidence level

bgtest(lm.fit) #Breusch_godfrey test; H0: Autocorrelation is 0


#Test for normality of residuals with normal probability plot (use standardises residuals) and Shapiro Wilk's test 
stdres <- rstandard(lm.fit) #standardise residuals
qqnorm(stdres, ylab="Standardised residuals", xlab="Normal scores", main="Normal probability plot");qqline(stdres, col = 2) #plot normal probability plot

shapiro.test(res) #Shapiro-Wilk's test; H0: data is of normal distribution


#Test for heteroscedastisity of residuals using 'Standardized predicted Values vs.Standardized residuals plot' and Breusch-Pagan test 
predict <- predict(lm.fit) # get predicted y
stdpredict <- predict-(mean(predict)/sd(predict)) # standardise predicted y
plot(stdpredict, stdres, main = "Testing heteroscedasticity", xlab = "Standardized predicted Values", ylab = "Standardized residuals")
abline(0,0)

bptest(lm.fit) # Breusch-Pagan Test; H0: data is homoscedastic 



#PLOT DATA WITH ESTIMATED REGRESSION LINE

plot(transf_x,transf_y, ylab="Life expectancy [years]", xlab="Log10(GDP) [USD per capita]", main= "Life expectancy vs. Log10(GDP) ")
abline(21.439, 12.515, col="Darkred") #get estimated model parameters a and b into abline function 


#NOTE: It is common that all model assumptions are not initially satisfied, and there are ways and methods to satisfy the 
#model assumptions. For example, data can be treated and transformed and the regression model can be modified. However, these 
#measures are beyond the scope of this exercise.


################################################################################################################
#5. VARIANCE ANALYSIS ----
#   DEVELOPMENT OF GLOBAL EQUALITY IN GROSS DOMESTIC PRODUCT (GDP), HUMAN DEVELOPMENT INDEX 
#   (HDI), LIFE EXPECTANCY AND UNDER FIVE YEAR MORTALITY - USE VARIABILITY BETWEEN COUNTRIES AS AN INDICATOR OF
#   EQUALITY (LOW (HIGH) VARIANCE BETWEEN COUNTRIES SUGGESTS EQUALITY (INEQUALITY)).
################################################################################################################


#Read one more data: Human development Index (HDI)
hdi_all  <- read.csv("data/hdi_all_countries_UN.csv", header = TRUE, sep = ";",dec = ".")


#PLOTTING WITH VARIANCE


# Alternative 1: Using base graphics
#Calculate variance across countries for each year
mort_var <- apply(under_five_year_mortality_all[,2:77], 2, var, na.rm = TRUE)
expect_var <- apply(life_expectancy_all[,2:77], 2, var, na.rm = TRUE) 
hdi_var <- apply(hdi_all[,2:10], 2, var, na.rm = TRUE)
gdp_var <- apply(gdp_all[,2:77], 2, var, na.rm = TRUE) 

#Plot variances
par(mfrow=c(2,2))
plot(mort_var, pch=20, col="Darkblue", xaxt="n", xlab="Time", ylab="Variance [-]", main="Under 5-year mortality - Variance between countries")
axis(1, at=2:77, labels=1940:2015) # put new x-axis with years

plot(expect_var, pch=20, col="Darkgreen", xaxt="n", xlab="Time", ylab="Variance [-]", main="Life expectancy - Variance between countries")
axis(1, at=2:77, labels=1940:2015) # put new x-axis with years

plot(hdi_var, pch=20, col="Darkorange", xaxt="n", xlab="Time", ylab="Variance [-]", main="HDI - Variance between countries")
axis(1, at=2:10, labels=c("1980", "1990", "2000", "2005", "2006", "2007", "2008", "2009", "2011")) # put new x-axis with years

plot(gdp_var, pch=20, col="Darkred", xaxt="n", xlab="Time", ylab="Variance [-]", main="GDP - Variance between countries")
axis(1, at=2:77, labels=1940:2015) # put new x-axis with years




# Alternative 2: using ggplot2 and other packages
library(reshape2) #for melt
library(plyr) #for rename
library(magrittr) #for %>% (can be inserted with ctrl-shift-m) 


# Test of converting data from wide to long format
test=melt(under_five_year_mortality_all,id.vars = c("Under.five.mortality"),variable.name="year")
head(test)
test=rename(test,c(Under.five.mortality="country"))
head(test)
test=mutate(test,variable="Under.five.mortality")
head(test)

# Same test with output from one command used as input for the next
melt(under_five_year_mortality_all,id.vars = c("Under.five.mortality"),variable.name="year") %>% 
  rename(.,c(Under.five.mortality="country")) %>% 
  head

# Convert all data into long format
all_data <- rbind(
  melt(under_five_year_mortality_all,id.vars = c("Under.five.mortality"),variable.name="year") %>% 
    rename(.,c(Under.five.mortality="country")) %>% 
    mutate(.,variable="Under.five.mortality"),
  melt(life_expectancy_all,id.vars = c("Life.expectancy.with.projections"),variable.name="year") %>% 
    rename(.,c(Life.expectancy.with.projections="country")) %>% 
    mutate(.,variable="Life.expectancy.with.projections"),
  melt(hdi_all,id.vars = c("HDI"),variable.name="year") %>% 
    rename(.,c(HDI="country")) %>% 
    mutate(.,variable="HDI"),  
  melt(gdp_all,id.vars = c("GDP.per.capita"),variable.name="year") %>% 
    rename(.,c(GDP.per.capita="country")) %>% 
    mutate(.,variable="GDP.per.capita")
)

# Convert years to numbers
#  remove "X", convert to number, and then replace the original year column using %<>% 
all_data$year %<>%  gsub("X","",.) %>% as.numeric


# With the data in long format, we can calculate all variances at once.
#For each variable and each year, summarise the variance (across countries)
all_var=ddply(all_data,~variable+year,summarise,var=var(value,na.rm=T))
head(all_var)

ggplot(data=all_var)+
  geom_point(aes(x=year,y=var,color=variable))+
  facet_wrap(~variable,scales = "free")


#PLOTTING WITH COEFFICIENT OF VARIATION (CV). 

#Calculate CV for each year. First, calculate mean (mean) and standard deviation (sd) for each year using 'apply' function, then
#calculate coefficient_of_variation (cv=(sd/mean)*100) and plot CV.
# MISSING SCRIPT. ----




