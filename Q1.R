library(data.table)
library(reshape2)

# create a tempfile and a tempdir
tmpdir <- tempdir()

urlDomain = "https://d396qusza40orc.cloudfront.net/"
urlFile = "exdata%2Fdata%2FNEI_data.zip"
url <- paste(urlDomain, urlFile)
localFile <- paste(tmpdir, urlFile, sep="/")

# download the data if it doesn't exist
if(!file.exists(localFile)){
  download.file(url,destfile=localFile, method="curl")
}

# unzip to tmpDir
unzip(localFile, exdir = tmpdir)

# name the extracted files
NEI_file <- paste(tmpdir, "summarySCC_PM25.rds", sep="/")
SCC_file <- paste(tmpdir, "Source_Classification_Code.rds", sep="/")

# read the data in
NEI <- readRDS(NEI_file)
SCC <- readRDS(SCC_file)


# Q1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources
# for each of the years 1999, 2002, 2005, and 2008.

# not needed for the question, just interesting.
# # look at the summaries for each year
# for (i in 1:length(unique(NEI$year))){
#   print(unique(NEI$year)[i])
#   print(summary(NEI[NEI$year==unique(NEI$year)[i],"Emissions"]))
# }
# 
# #
# # boxplot the range of values for each year
# boxplot(split(log10(NEI$Emissions), NEI$year),
#         las="1",
#         xlab="year",
#         ylab="log10 emitted PM2.5, in tons",
#         main="PM2.5 emission from all sources\nfor the years 1999, 2002, 2005, and 2008")
# 

# take the flat data and re-pivot to show the sum of emissions for each year
PM25SumByYear <- dcast(NEI, year ~ Pollutant, value.var="Emissions", sum)

# Convert tons to thousands of tons
PM25SumByYear$"PM25-PRI" <- PM25SumByYear$"PM25-PRI"/1000
png(file="plot1.png")
plot(PM25SumByYear, 
     type="l",
     lwd ="4",
     col="red",
     xaxp=c(1999,2008,9), 
     las=1, 
     ylab="total emitted PM2.5, in thousands of tons", 
     main="total PM2.5 emission from all sources\nfor the years 1999, 2002, 2005, and 2008")
dev.off()
