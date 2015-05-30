library(data.table)
library(reshape2)
library(ggplot2)

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

# Q3 Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make
# a plot answer this question.
#
# take the flat and re-pivot show the sum of emissions for each year and type for fips == "24510" only

PM25SumByYearByTypeBaltimore <- dcast(NEI[NEI$fips=="24510",], year + type ~ Pollutant, value.var="Emissions", sum)

#fix the names
names(PM25SumByYearByTypeBaltimore) <- make.names(names(PM25SumByYearByTypeBaltimore))

# Convert tons to thousands of tons
PM25SumByYearByTypeBaltimore$PM25.PRI <- PM25SumByYearByTypeBaltimore$PM25.PRI/1000

png(file="plot3.png")
plot3 <- qplot(year, PM25.PRI,
      data=PM25SumByYearByTypeBaltimore,
      geom="line",
      colour=type,
      ylab="total emitted PM2.5, in thousands of tons",
      main="total PM2.5 emission from all sources in Baltimore City\nfor the years 1999, 2002, 2005, and 2008\nby source type")
print(plot3)
dev.off()

