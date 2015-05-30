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



# Q2 Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510")
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.
#
# take the flat and re-pivot show the sum of emissions for each year for fips == "24510" only
PM25SumByYearBaltimore <- dcast(NEI[NEI$fips=="24510",], year ~ Pollutant, value.var="Emissions", sum)

PM25SumByYearBaltimore$"PM25-PRI" <- PM25SumByYearBaltimore$"PM25-PRI"/1000
png(file="plot2.png")
plot(PM25SumByYearBaltimore,
     type="l",
     lwd ="4",
     col="red",
     xaxp=c(1999,2008,9),
     las=1,
     ylab="total emitted PM2.5, in thousands of tons",
     main="total PM2.5 emission from all sources in Baltimore City\nfor the years 1999, 2002, 2005, and 2008")
dev.off()

