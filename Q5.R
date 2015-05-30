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

# Q5 How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City? 
SCC_Vehicle <- SCC[SCC$EI.Sector %like% "Vehicle",]
NEI_Vehicle <- merge(NEI[NEI$fips=="24510",],SCC_Vehicle[,c("SCC", "EI.Sector")], by = "SCC")
PM25SumByYearVehiclesBoltimore <- dcast(NEI_Vehicle, year ~ Pollutant, value.var="Emissions", sum)

#fix the names
names(PM25SumByYearVehiclesBoltimore) <- make.names(names(PM25SumByYearVehiclesBoltimore))

# assign NA to zeros
NEI_Vehicle$Emissions[NEI_Vehicle$Emissions == 0] <- NA

# convert from tons to thousands of tons
PM25SumByYearVehiclesBoltimore$PM25.PRI <- PM25SumByYearVehiclesBoltimore$PM25.PRI/1000

# look at the summaries for each year
for (i in 1:length(unique(NEI_Vehicle$year))){
  print(unique(NEI_Vehicle$year)[i])
  print(summary(NEI_Vehicle[NEI_Vehicle$year==unique(NEI_Vehicle$year)[i],"Emissions"]))
}

png(file="plot5.png", width=480, height=1500, res=96)   # need to set to res or the fonts look small
par(mfrow = c(3, 1))

#
# boxplot the range of values for each year
boxplot(split(NEI_Vehicle$Emissions, NEI_Vehicle$year),
        las="1",
        xlab="year",
        outline=FALSE,  # remove the outliers
        range=2,        # reduce the whiskers
        ylab="emitted PM2.5, in tons",
        main="PM2.5 emission from vehicle sources\nfor the years 1999, 2002, 2005, and 2008")

# also show the max emitted PM25
PM25MaxByYear <- data.frame(sapply(split(NEI_Vehicle$Emissions, NEI_Vehicle$year), max, na.rm=TRUE))
PM25MaxByYear$year <- rownames(PM25MaxByYear)
names(PM25MaxByYear) <- c("PM25.max","year")

plot(PM25MaxByYear[,c(2,1)],      # plot in the right order
     type="l",
     lwd ="4",
     col="red",
     xaxp=c(1999,2008,9),
     las=1,
     ylab="max emitted PM2.5, in tons",
     main="maximum PM2.5 emission from vehicle sources in Baltimore City\nfor the years 1999, 2002, 2005, and 2008")

plot(PM25SumByYearVehiclesBoltimore,
     type="l",
     lwd ="4",
     col="red",
     xaxp=c(1999,2008,9),
     las=1,
     ylab="total emitted PM2.5, in thousands of tons",
     main="total PM2.5 emission from vehicle sources in Baltimore City\nfor the years 1999, 2002, 2005, and 2008")
dev.off()
