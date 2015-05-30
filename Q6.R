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

# Q6 Compare emissions from motor vehicle sources in Baltimore City with emissions from
# motor vehicle sources in Los Angeles County, California (fips == "06037").
# Which city has seen greater changes over time in motor vehicle emissions?

SCC_Vehicle <- SCC[SCC$EI.Sector %like% "Vehicle",]
NEI_Vehicle_Boltimore <- merge(NEI[NEI$fips=="24510",],SCC_Vehicle[,c("SCC", "EI.Sector")], by = "SCC")
NEI_Vehicle_LosAngeles <- merge(NEI[NEI$fips=="06037",],SCC_Vehicle[,c("SCC", "EI.Sector")], by = "SCC")
PM25SumByYearVehiclesBoltimore <- dcast(NEI_Vehicle_Boltimore, year ~ Pollutant, value.var="Emissions", sum)
PM25SumByYearVehiclesLosAngeles <- dcast(NEI_Vehicle_LosAngeles, year ~ Pollutant, value.var="Emissions", sum)


#fix the names
names(PM25SumByYearVehiclesBoltimore) <- make.names(names(PM25SumByYearVehiclesBoltimore))
names(PM25SumByYearVehiclesLosAngeles) <- make.names(names(PM25SumByYearVehiclesLosAngeles))

# assign NA to zeros
NEI_Vehicle_Boltimore$Emissions[NEI_Vehicle_Boltimore$Emissions == 0] <- NA
NEI_Vehicle_LosAngeles$Emissions[NEI_Vehicle_LosAngeles$Emissions == 0] <- NA

# convert from tons to thousands of tons
PM25SumByYearVehiclesBoltimore$PM25.PRI <- PM25SumByYearVehiclesBoltimore$PM25.PRI/1000
PM25SumByYearVehiclesLosAngeles$PM25.PRI <- PM25SumByYearVehiclesLosAngeles$PM25.PRI/1000

# look at the summaries for each year
print("Boltimore")
for (i in 1:length(unique(NEI_Vehicle_Boltimore$year))){
  print(unique(NEI_Vehicle_Boltimore$year)[i])
  print(summary(NEI_Vehicle_Boltimore[NEI_Vehicle_Boltimore$year==unique(NEI_Vehicle_Boltimore$year)[i],"Emissions"]))
}

print("Los Angeles")
for (i in 1:length(unique(NEI_Vehicle_LosAngeles$year))){
  print(unique(NEI_Vehicle_LosAngeles$year)[i])
  print(summary(NEI_Vehicle_LosAngeles[NEI_Vehicle_LosAngeles$year==unique(NEI_Vehicle_LosAngeles$year)[i],"Emissions"]))
}

png(file="plot6.png", width=1000, height=1500, res=96)   # need to set to res or the fonts look small
par(mfrow = c(3, 2))

#
# boxplot the range of values for each year
boxplot(split(NEI_Vehicle_Boltimore$Emissions, NEI_Vehicle_Boltimore$year),
        las="1",
        xlab="year",
        outline=FALSE,  # remove the outliers
        range=2,        # reduce the whiskers
        ylab="emitted PM2.5, in tons",
        main="PM2.5 emission from vehicle sources in Boltimore City\nfor the years 1999, 2002, 2005, and 2008")

boxplot(split(NEI_Vehicle_LosAngeles$Emissions, NEI_Vehicle_LosAngeles$year),
        las="1",
        xlab="year",
        outline=FALSE,  # remove the outliers
        range=2,        # reduce the whiskers
        ylab="emitted PM2.5, in tons",
        main="PM2.5 emission from vehicle sources in Los Angeles\nfor the years 1999, 2002, 2005, and 2008")

# also show the max emitted PM25
PM25MaxByYearBoltimore <- data.frame(sapply(split(NEI_Vehicle_Boltimore$Emissions, NEI_Vehicle_Boltimore$year), max, na.rm=TRUE))
PM25MaxByYearBoltimore$year <- rownames(PM25MaxByYearBoltimore)
names(PM25MaxByYearBoltimore) <- c("PM25.max","year")

plot(PM25MaxByYearBoltimore[,c(2,1)],      # plot in the right order
     type="l",
     lwd ="4",
     col="red",
     xaxp=c(1999,2008,9),
     las=1,
     ylab="max emitted PM2.5, in tons",
     main="maximum PM2.5 emission from vehicle sources in Baltimore City\nfor the years 1999, 2002, 2005, and 2008")


PM25MaxByYearLosAngeles <- data.frame(sapply(split(NEI_Vehicle_LosAngeles$Emissions, NEI_Vehicle_LosAngeles$year), max, na.rm=TRUE))
PM25MaxByYearLosAngeles$year <- rownames(PM25MaxByYearLosAngeles)
names(PM25MaxByYearLosAngeles) <- c("PM25.max","year")

plot(PM25MaxByYearLosAngeles[,c(2,1)],      # plot in the right order
     type="l",
     lwd ="4",
     col="red",
     xaxp=c(1999,2008,9),
     las=1,
     ylab="max emitted PM2.5, in tons",
     main="maximum PM2.5 emission from vehicle sources in Los Angeles\nfor the years 1999, 2002, 2005, and 2008")

plot(PM25SumByYearVehiclesBoltimore,
     type="l",
     lwd ="4",
     col="red",
     xaxp=c(1999,2008,9),
     las=1,
     ylab="total emitted PM2.5, in thousands of tons",
     main="total PM2.5 emission from vehicle sources in Baltimore City\nfor the years 1999, 2002, 2005, and 2008")

plot(PM25SumByYearVehiclesLosAngeles,
     type="l",
     lwd ="4",
     col="red",
     xaxp=c(1999,2008,9),
     las=1,
     ylab="total emitted PM2.5, in thousands of tons",
     main="total PM2.5 emission from vehicle sources in Los Angeles\nfor the years 1999, 2002, 2005, and 2008")
dev.off()
