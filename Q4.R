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


# Q4 Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

SCC_Coal <- SCC[SCC$EI.Sector %like% "Coal",]
NEI_Coal <- merge(NEI,SCC_Coal[,c("SCC", "EI.Sector")], by = "SCC")
PM25SumByYearCoal <- dcast(NEI_Coal, year ~ Pollutant, value.var="Emissions", sum)

#fix the names
names(PM25SumByYearCoal) <- make.names(names(PM25SumByYearCoal))

# assign NA to zeros
NEI_Coal$Emissions[NEI_Coal$Emissions == 0] <- NA

# convert from tons to thousands of tons
PM25SumByYearCoal$PM25.PRI <- PM25SumByYearCoal$PM25.PRI/1000

# look at the summaries for each year
for (i in 1:length(unique(NEI_Coal$year))){
  print(unique(NEI_Coal$year)[i])
  print(summary(NEI_Coal[NEI_Coal$year==unique(NEI_Coal$year)[i],"Emissions"]))
}

png(file="plot4.png", width=480, height=1500, res=96) # need to set to res or the fonts look small
par(mfrow = c(3, 1))

#
# boxplot the range of values for each year
boxplot(split(NEI_Coal$Emissions, NEI_Coal$year),
        las="1",
        outline=FALSE,  # remove the outliers
        range=2,        # reduce the whiskers
        xlab="year",
        ylab="log10 emitted PM2.5, in tons",
        main="PM2.5 emission from coal sources\nfor the years 1999, 2002, 2005, and 2008")

# also show the max emitted PM25
PM25MaxByYear <- data.frame(sapply(split(NEI_Coal$Emissions, NEI_Coal$year), max, na.rm=TRUE))
PM25MaxByYear$year <- rownames(PM25MaxByYear)
names(PM25MaxByYear) <- c("PM25.max","year")
PM25MaxByYear$PM25.max <- PM25MaxByYear$PM25.max/1000

plot(PM25MaxByYear[,c(2,1)],      # plot in the right order
     type="l",
     lwd ="4",
     col="red",
     xaxp=c(1999,2008,9),
     las=1,
     ylab="max emitted PM2.5, in thousands of tons",
     main="maximum PM2.5 emission from coal sources\nfor the years 1999, 2002, 2005, and 2008")

plot(PM25SumByYearCoal,
     type="l",
     lwd ="4",
     col="red",
     xaxp=c(1999,2008,9),
     las=1,
     ylab="total emitted PM2.5, in thousands of tons",
     main="total PM2.5 emission from coal sources\nfor the years 1999, 2002, 2005, and 2008")
dev.off()
