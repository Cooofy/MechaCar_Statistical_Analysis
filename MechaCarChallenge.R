library(dplyr)
mechacar <- read.csv('MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
linearmodel <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, mechacar)
summary(linearmodel)

# Which variables/coefficients provided a non-random amount of variance to the mpg values in the dataset?

# vehicle_length and ground_clearance since their p-values are less than a significance level of 5%, therefore we reject the null hypothesis that their coefficient values are zero.

# Is the slope of the linear model considered to be zero? Why or why not?

# The slope of the linear model is not zero since the p-value of 5.35e-11 is less than a significance level of 5% meaning failing to reject the null hypothesis test of the slope = 0.

# Does this linear model predict mpg of MechaCar prototypes effectively? Why or why not?

# The linear model does predict the mpg of MechaCar prototypes since the R-squared value is 0.7149 meaning that most of the variability in mpg can be explained by the other columns.




suspensioncoil <- read.csv('Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
total_summary <- suspensioncoil %>% summarise(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI))
lot_summary <- suspensioncoil %>% group_by(Manufacturing_Lot) %>% summarise(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI))

# Looking at the lots in aggregate, we see that the variance is less than 100 pounds per square inch. However looking at the lots individually, it's shown that lot 3 has a variance that exceeds 100.





t.test(x = suspensioncoil$PSI, mu = 1500, alternative = 'two.sided')

lot1 <- subset(suspensioncoil, suspensioncoil[,2] == "Lot1", select = PSI)
lot2 <- subset(suspensioncoil, suspensioncoil[,2] == "Lot2", select = PSI)
lot3 <- subset(suspensioncoil, suspensioncoil[,2] == "Lot3", select = PSI)

t.test(x = lot1, mu = 1500, alternative = 'two.sided')

# P-value is 1 meaning that we fail to reject the null hypothesis of the mean of lot1's PSI being equal to 1500.

t.test(x = lot2, mu = 1500, alternative = 'two.sided')

# P-value is 0.6072 meaning that we fail to reject the null hypothesis of the mean of lot2's PSI being equal to 1500.

t.test(x = lot3, mu = 1500, alternative = 'two.sided')

# P-value is 0.04168 meaning that we reject the null hypothesis of the mean of lot3's PSI being equal to 1500. Additionally the confidence interval for the mean doesn't include 1500.






# Metrics

# Miles per gallons, horse power, maintenance cost and safety rating.

# The null hypothesis would be the following for the miles per gallon metric:

# Mean MechaCar vechicles' miles per gallon >= Mean Competitor vechicles' miles per gallon

# The null hypothesis for other metrics would be the same, but replace gallon with the other metric.

# The statistical test used would be the two sample, one sided t-test since it's a comparison between the means from two difference samples.

# The data needed to perform this test would be a sample of cars from MechaCar and a competitor with the miles per gallon, horse power, maintenance cost and safety rating for each car.