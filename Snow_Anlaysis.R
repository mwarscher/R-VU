# Files
#file_clim = "Meteo_Patscherkofel.dat"
#file_snow = "Snow_Patscherkofel.csv"
file_clim = "Meteo_Ramsau.dat"
file_snow = "Snow_Ramsau.csv"

# Read climate file
mydata_clim = read.table(file_clim, sep=",", skip="4", dec = ".", header=TRUE)

# Convert to date
mydata_clim$date = as.Date(mydata_clim$date)

# Generate daily series
temp_d = aggregate(temp ~ date, data=mydata_clim, FUN=mean, na.rm=TRUE)
precip_d = aggregate(precip ~ date, data=mydata_clim, FUN=sum, na.rm=TRUE)

# Read snow file
mydata_snow = read.table(file_snow, sep=",", skip="0", dec = ".", header=TRUE)

# Convert to date
mydata_snow$datum = as.Date(as.character(mydata_snow$datum), "%Y%m%d")

# Clean -1 values in snow series
mydata_snow$schnee[mydata_snow$schnee < 0] = 0.
mydata_snow$neuschnee[mydata_snow$neuschnee < 0] = 0.

# Generate daily series
snow_d = aggregate(schnee ~ datum, data=mydata_snow, FUN=mean, na.rm=TRUE)
newsnow_d = aggregate(neuschnee ~ datum, data=mydata_snow, FUN=sum, na.rm=TRUE)

# Cut out 1995-2015
temp_d = temp_d [temp_d$date>="1995-01-01" & temp_d$date<="2015-12-31",]
precip_d = precip_d[precip_d$date>="1995-01-01" & precip_d$date<="2015-12-31",]
snow_d = snow_d[snow_d$datum>="1995-01-01" & snow_d$datum<="2015-12-31",]
newsnow_d = newsnow_d[newsnow_d$datum>="1995-01-01" & newsnow_d$datum<="2015-12-31",]

# Calculate snow cover days
snow_d$scd = snow_d$schnee
snow_d$scd[snow_d$scd > 0] = 1
snow_d$scd[snow_d$scd <= 0] = 0

# Calculate yearly values: precipitation
precip_d$y = format(precip_d$date, "%Y")
precip_y = aggregate(precip ~ y, data=precip_d, FUN=sum, na.rm=TRUE)

# Calculate yearly values: temperature
temp_d$y = format(temp_d$date, "%Y")
temp_y = aggregate(temp ~ y, data=temp_d, FUN=mean, na.rm=TRUE)

# Calculate yearly values: snow cover days
snow_d$y = format(snow_d$datum, "%Y")
scd_y = aggregate(scd ~ y, data=snow_d, FUN=sum, na.rm=TRUE)

# Perform regression : temperature
scd_y$y = as.numeric(scd_y$y)                    # convert year to numeric
fit_scd = lm(scd_y$scd ~ scd_y$y)                # perform regression
scd_y$scd_calculated = fitted.values(fit_scd)     # write calculated values in column

# Plot regression: temperature
plot(scd_y$y, scd_y$scd, type="l", col="grey", ann=FALSE)
lines(scd_y$y, scd_y$scd_calculated, type="l", col="black")
title(main="Verlauf der Schneedeckentage")
title(xlab="Jahr")
title(ylab="Schneedeckentage (Tage)")
slope = round(coefficients(fit_scd)[2], 2)
intercept = round(coefficients(fit_scd)[1], 2)
regress_function = paste("y = ", intercept, " + ", slope, " * x")
text(2000,100,regress_function)

# Perform regression : precipitation
precip_y$y = as.numeric(precip_y$y)                    # convert year to numeric
fit_precip = lm(precip_y$precip ~ precip_y$y)                # perform regression
precip_y$precip_calculated = fitted.values(fit_precip)     # write calculated values in column

# Plot regression: precipitation
plot(precip_y$y, precip_y$precip, type="l", col="blue", ann=FALSE)
lines(precip_y$y, precip_y$precip_calculated, type="l", col="black")
title(main="Verlauf des Jahresniederschlags")
title(xlab="Jahr")
title(ylab="Niederschlagssumme (mm)")
slope = round(coefficients(fit_precip)[2], 2)
intercept = round(coefficients(fit_precip)[1], 2)
regress_function = paste("y = ", intercept, " + ", slope, " * x")
text(2000,950,regress_function)

# Perform regression : temperature
temp_y$y = as.numeric(temp_y$y)                    # convert year to numeric
fit_temp = lm(temp_y$temp ~ temp_y$y)                # perform regression
temp_y$temp_calculated = fitted.values(fit_temp)     # write calculated values in column

# Plot regression: temperature
plot(temp_y$y, temp_y$temp, type="l", col="red", ann=FALSE)
lines(temp_y$y, temp_y$temp_calculated, type="l", col="black")
title(main="Verlauf der Jahresmitteltemperatur")
title(xlab="Jahr")
title(ylab="Jahresmitteltemperatur (°C)")
slope = round(coefficients(fit_temp)[2], 2)
intercept = round(coefficients(fit_temp)[1], 2)
regress_function = paste("y = ", intercept, " + ", slope, " * x")
text(2000,4.5,regress_function)

# Calculate values for 12 months of the year: temperature (not needed)
temp_d$m = format(temp_d$date, "%m")
temp_m = aggregate(temp ~ m, data=temp_d, FUN=mean, na.rm=TRUE)

# Calculate values for 12 months of the year: precipitation (not needed)
precip_d$y = format(precip_d$date, "%Y") 
precip_d$m = format(precip_d$date, "%m") 
precip_ym = aggregate(precip ~ y+m, data=precip_d, FUN=sum, na.rm=TRUE)
precip_m = aggregate(precip ~ m, data=precip_ym, FUN=mean, na.rm=TRUE)

# Calculate values for 12 months of the year: snow cover days (not needed)
snow_d$y = format(snow_d$datum, "%Y") 
snow_d$m = format(snow_d$datum, "%m") 
snow_ym = aggregate(scd ~ y+m, data=snow_d, FUN=sum, na.rm=TRUE)
snow_m = aggregate(scd ~ m, data=snow_ym, FUN=mean, na.rm=TRUE)

# Prepare time slices 1995-2004
temp_d_slice1 = temp_d [temp_d$date>="1995-01-01" & temp_d$date<="2004-12-31",]
precip_d_slice1 = precip_d[precip_d$date>="1995-01-01" & precip_d$date<="2004-12-31",]
snow_d_slice1 = snow_d[snow_d$datum>="1995-01-01" & snow_d$datum<="2004-12-31",]
newsnow_d_slice1 = newsnow_d[newsnow_d$datum>="1995-01-01" & newsnow_d$datum<="2004-12-31",]

# Calculate values for 12 months of the year (slice 1): temperature
temp_d_slice1$m = format(temp_d_slice1$date, "%m")
temp_m_slice1 = aggregate(temp ~ m, data=temp_d_slice1, FUN=mean, na.rm=TRUE)

# Calculate values for 12 months of the year (slice 1): precipitation
precip_d_slice1$y = format(precip_d_slice1$date, "%Y") 
precip_d_slice1$m = format(precip_d_slice1$date, "%m") 
precip_ym_slice1 = aggregate(precip ~ y+m, data=precip_d_slice1, FUN=sum, na.rm=TRUE)
precip_m_slice1 = aggregate(precip ~ m, data=precip_ym_slice1, FUN=mean, na.rm=TRUE)

# Calculate values for 12 months of the year (slice 1): snow cover days
snow_d_slice1$y = format(snow_d_slice1$datum, "%Y") 
snow_d_slice1$m = format(snow_d_slice1$datum, "%m") 
snow_ym_slice1 = aggregate(scd ~ y+m, data=snow_d_slice1, FUN=sum, na.rm=TRUE)
snow_m_slice1 = aggregate(scd ~ m, data=snow_ym_slice1, FUN=mean, na.rm=TRUE)

# Prepare time slices 2005-2014
temp_d_slice2 = temp_d [temp_d$date>="2005-01-01" & temp_d$date<="2014-12-31",]
precip_d_slice2 = precip_d[precip_d$date>="2005-01-01" & precip_d$date<="2014-12-31",]
snow_d_slice2 = snow_d[snow_d$datum>="2005-01-01" & snow_d$datum<="2014-12-31",]
newsnow_d_slice2 = newsnow_d[newsnow_d$datum>="2005-01-01" & newsnow_d$datum<="2014-12-31",]

# Calculate values for 12 months of the year (slice 2): temperature
temp_d_slice2$m = format(temp_d_slice2$date, "%m")
temp_m_slice2 = aggregate(temp ~ m, data=temp_d_slice2, FUN=mean, na.rm=TRUE)

# Calculate values for 12 months of the year (slice 2): precipitation
precip_d_slice2$y = format(precip_d_slice2$date, "%Y") 
precip_d_slice2$m = format(precip_d_slice2$date, "%m") 
precip_ym_slice2 = aggregate(precip ~ y+m, data=precip_d_slice2, FUN=sum, na.rm=TRUE)
precip_m_slice2 = aggregate(precip ~ m, data=precip_ym_slice2, FUN=mean, na.rm=TRUE)

# Calculate values for 12 months of the year (slice 2): snow cover days
snow_d_slice2$y = format(snow_d_slice2$datum, "%Y") 
snow_d_slice2$m = format(snow_d_slice2$datum, "%m") 
snow_ym_slice2 = aggregate(scd ~ y+m, data=snow_d_slice2, FUN=sum, na.rm=TRUE)
snow_m_slice2 = aggregate(scd ~ m, data=snow_ym_slice2, FUN=mean, na.rm=TRUE)

# Generate Plot with the different slices: temperature
plot(temp_m_slice1$m, temp_m_slice1$temp, type="l", col="orangered", ann=FALSE, lwd=2)
lines(temp_m_slice2$m, temp_m_slice2$temp, type="l", col="orangered4", ann=FALSE, lwd=2)
title(main="Monatsmittel der Temperatur")
title(xlab="Monat")
title(ylab="Temperatur (°C)")
legend("bottom", inset=.05, c("1995-2004","2005-2014"), col=c("orangered","orangered4"), lty=c('solid','solid'))

# Generate Plot with the different slices: precipitation
plot(precip_m_slice2$m, precip_m_slice2$precip, type="l", col="royalblue4", ann=FALSE, lwd=2)
lines(precip_m_slice1$m, precip_m_slice1$precip, type="l", col="royalblue", ann=FALSE, lwd=2)
title(main="Monatssumme des Niederschlags")
title(xlab="Monat")
title(ylab="Niederschlag (mm)")
legend("bottom", inset=.05, c("1995-2004","2005-2014"), col=c("royalblue4","royalblue"), lty=c('solid','solid'))

# Generate Plot with the different slices: snow cover days
plot(snow_m_slice2$m, snow_m_slice2$scd, type="l", col="snow3", ann=FALSE, lwd=2)
lines(snow_m_slice1$m, snow_m_slice1$scd, type="l", col="snow4", ann=FALSE, lwd=2)
title(main="Monatssumme der Schneedeckentage")
title(xlab="Monat")
title(ylab="Schneedeckentage (Tage)")
legend("top", inset=.05, c("1995-2004","2005-2014"), col=c("snow3","snow4"), lty=c('solid','solid'))

# Plotten
# plot(temp_m$m, temp_m$temp, type="h", col="red", ann=FALSE, lwd=20)
# plot(temp_m$m, temp_m$temp, type="h", col="blue", ann=FALSE, lwd=20)
# plot(snow_m$m, snow_m$scd, type="h", col="black", ann=FALSE, lwd=20)