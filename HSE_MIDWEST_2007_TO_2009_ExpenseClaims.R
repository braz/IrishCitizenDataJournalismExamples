# File-Name:				HSE_MIDWEST_2007_TO_2009_ExpenseClaims.R
# Date:							2010-12-24
# Author:						Eoin Brazil
# Email:						eoin.brazil@gmail.com
# Purpose:					Format and explore the HSE Mid-West Expense Claims for 2007-2009
# Data Used:				2007MidWest.csv, 2008MidWest.csv, 2009MidWest.csv
# R version Used:		2.11.1
# Packages Used:		car, ggplot2, vcd
# Output Files:			newres_extrafields.csv
# Data Output:			

# Version:					1.1
# Change log:				Minor corrections for path simplification and variable cleanup.

# Copyright (c) 2010, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All rights reserved.                                                         

# For data manipulation and visualization
library(ggplot2)
library(car)
library(vcd)

# Replace the path here with the appropriate one for your machine
mydatapath = "/Users/eoinbrazil/Desktop/interestingRstuff/hse/"

# Set the working directory to the current location for the data files
setwd(mydatapath)

# Read the cleaned reservations datafile
midwestHSE2007 <- read.csv(paste(mydatapath, "2007MidWest.csv", sep=""))
Area <- unlist(lapply(strsplit(as.character(midwestHSE2007$Pers.Area), split=" "), function(x) unlist(x[1])))

# Format the data adding the new variable, Area
midwestHSE2007 <- data.frame(cbind(midwestHSE2007, Area))

# Given that each consultant seems to be indicated by CONSULTANT/XXX where XXX is their area of speciality we can use this to find just these expense claims
midwestHSE2007_consultants <- midwestHSE2007[grep('^CONSULTANT', midwestHSE2007$Grade.Title),]

# In order to remove the CONSULTANT so we can just present the area of speciality, the information still needs to be cleaned up a little
gradeTitle2007 <- unlist(strsplit(as.character(midwestHSE2007_consultants$Grade.Title), 'CONSULTANT/'))
gradeTitle2007 <- gradeTitle2007[which(gradeTitle2007!="")]
midwestHSE2007_consultants = transform(midwestHSE2007_consultants, Speciality=gradeTitle2007)

# The expenses can be better understood by grouping them
expensesizegroup = recode(midwestHSE2007_consultants$Total, "0:250='€0-€250'; 251:500='€251-€500'; 501:1000='€501-€1000'; 1001:2000='€1001-€2000'; 2001:5000='€2001-€5000'; else='€5000+'")
midwestHSE2007_consultants = transform(midwestHSE2007_consultants, Expense.Category=expensesizegroup)
midwestHSE2007_consultants$Expense.Category <- factor(midwestHSE2007_consultants$Expense.Category,levels=c('€0-€250','€251-€500','€501-€1000','€1001-€2000','€2001-€5000','€5000+'))

# The only way to really look at them is to visualise them
qplot(Area, Total, data=midwestHSE2007_consultants,shape=expensesizegroup, position = position_jitter(width = 0.2)) + facet_wrap(~ Speciality, ncol=4) + opts(title="2007 MidWest HSE Expenses for Consultants by Area and by Speciality") + theme_grey()

# To prevent repetition, it is trivial to place the processing into a function that we can reuse for processing data from other years 
formatExpenseData <- function(filenameToFormat) {
	currentHSEFile <- c()
	currentHSEFile <- read.csv(paste(mydatapath, filenameToFormat, sep=""))
	Area <- unlist(lapply(strsplit(as.character(currentHSEFile$Pers.Area), split=" "), function(x) unlist(x[1])))
	# Format the data adding the new variable, Area
	currentHSEFile <- data.frame(cbind(currentHSEFile, Area))

	# Given that each consultant seems to be indicated by CONSULTANT/XXX where XXX is their area of speciality we can use this to find just these expense claims
	currentHSEFile_consultants <- currentHSEFile[grep('^CONSULTANT', currentHSEFile$Grade.Title),]

	# In order to remove the CONSULTANT so we can just present the area of speciality, the information still needs to be cleaned up a little
	gradeTitle <- unlist(strsplit(as.character(currentHSEFile_consultants$Grade.Title), 'CONSULTANT/'))
	gradeTitle <- gradeTitle[which(gradeTitle!="")]
	currentHSEFile_consultants = transform(currentHSEFile_consultants, Speciality=gradeTitle)

	# The expenses can be better understood by grouping them
	expensesizegroup = recode(currentHSEFile_consultants$Total, "0:250='€0-€250'; 251:500='€251-€500'; 501:1000='€501-€1000'; 1001:2000='€1001-€2000'; 2001:5000='€2001-€5000'; else='€5000+'")
	currentHSEFile_consultants = transform(currentHSEFile_consultants, Expense.Category=expensesizegroup)
	currentHSEFile_consultants$Expense.Category <- factor(currentHSEFile_consultants$Expense.Category,levels=c('€0-€250','€251-€500','€501-€1000','€1001-€2000','€2001-€5000','€5000+'))
	
	currentHSEFile_consultants[, drop=TRUE]
	return(currentHSEFile_consultants)
}

# Clean up the earlier use of the 2007 data
rm(midwestHSE2007, midwestHSE2007_consultants, Area, gradeTitle2007, expensesizegroup)

midwestHSE2007_consultants <- formatExpenseData("2007MidWest.csv")
midwestHSE2008_consultants <- formatExpenseData("2008MidWest.csv")
midwestHSE2009_consultants <- formatExpenseData("2009MidWest.csv")

# Lets look at the macro patterns by combining the information from 2007, 2008 and 2009 but firstly add the relevant year as a field to each of the existing data frames
midwestHSE2007_consultants = transform(midwestHSE2007_consultants, Year="2007")
midwestHSE2008_consultants = transform(midwestHSE2008_consultants, Year="2008")
midwestHSE2009_consultants = transform(midwestHSE2009_consultants, Year="2009")

midwestHSE_consultants <- rbind(midwestHSE2007_consultants,midwestHSE2008_consultants,midwestHSE2009_consultants)
midwestHSE_consultants_fullnames <- as.vector(midwestHSE_consultants$Full.Name)
midwestHSE_consultants <- midwestHSE_consultants[,-3]
midwestHSE_consultants = transform(midwestHSE_consultants, Full.Name=midwestHSE_consultants_fullnames)

midwestHSE_consultants <- midwestHSE_consultants[,-9]
expensesizegroup = recode(midwestHSE_consultants$Total, "0:250='€0-€250'; 251:500='€251-€500'; 501:1000='€501-€1000'; 1001:2000='€1001-€2000'; 2001:5000='€2001-€5000'; else='€5000+'")
midwestHSE_consultants = transform(midwestHSE_consultants, Expense.Category=expensesizegroup)
midwestHSE_consultants$Expense.Category <- factor(midwestHSE_consultants$Expense.Category,levels=c('€0-€250','€251-€500','€501-€1000','€1001-€2000','€2001-€5000','€5000+'))

# A example of some queries on the data
midwestHSE_consultants[midwestHSE_consultants$Full.Name=="Syed Naqvi",]
midwestHSE_consultants[midwestHSE_consultants$Area=="Clare" & midwestHSE_consultants$Speciality=="SURGERY",]

# The only way to really look at them is to visualise them
qplot(Year, Area, data=midwestHSE_consultants,shape=Expense.Category, color=Full.Name, position = position_jitter(width = 0.2)) + facet_wrap(~ Speciality, ncol=4) + opts(legend.position="none",title="Mid-West HSE Expenses for Consultants by Area and by Speciality") + theme_grey()

# Drill down just to the Clare region
midwestHSE_consultants_clare <- subset(midwestHSE_consultants, Area=="Clare", drop=TRUE)

midwestHSE_consultants_clare_fullnames <- as.vector(midwestHSE_consultants_clare$Full.Name)
midwestHSE_consultants_clare <- midwestHSE_consultants_clare[,-10]
midwestHSE_consultants_clare = transform(midwestHSE_consultants_clare, Full.Name=midwestHSE_consultants_clare_fullnames)

# The only way to really look at them is to visualise them
qplot(Year, Total, data=midwestHSE_consultants_clare,shape=Expense.Category, color=Full.Name, position = position_jitter(width = 0.2)) + facet_wrap(~ Speciality, ncol=4) + opts(legend.position="none",title="Mid-West HSE Expenses for Consultants for Clare Region by Speciality") + theme_grey()

# Continue to rill down from to the Clare region to just the SURGERY grades in this area
midwestHSE_consultants_clare_surgery <- subset(midwestHSE_consultants_clare, Speciality=="SURGERY", drop=TRUE)

midwestHSE_consultants_clare_surgery_fullnames <- as.vector(midwestHSE_consultants_clare_surgery$Full.Name)
midwestHSE_consultants_clare_surgery <- midwestHSE_consultants_clare_surgery[,-10]
midwestHSE_consultants_clare_surgery = transform(midwestHSE_consultants_clare_surgery, Full.Name=midwestHSE_consultants_clare_surgery_fullnames)

# The only way to really look at them is to visualise them
qplot(Year, Total, data=midwestHSE_consultants_clare_surgery,shape=Expense.Category, color=Full.Name, position = position_jitter(width = 0.2)) + opts(legend.position="none",title="Mid-West HSE Expenses for Surgery Consultants for Clare Region") + theme_grey()

# A table version of the same information
midwestHSE_consultants_clare_surgery_table <- structable(Year ~ Full.Name + Expense.Category, data=midwestHSE_consultants_clare_surgery)

# Clean up the variables used for this script
rm(midwestHSE2007_consultants, midwestHSE2008_consultants, midwestHSE2009_consultants, midwestHSE_consultants_fullnames, expensesizegroup, midwestHSE_consultants_clare, midwestHSE_consultants_clare_surgery, midwestHSE_consultants_clare_surgery_table)