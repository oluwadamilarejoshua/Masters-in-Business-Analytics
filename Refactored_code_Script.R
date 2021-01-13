
# Part 1 ------------------------------------------------------------------

# Loading required packages
#-------------------------------------------
# Uncomment and run the line below if tidyverse is not installed already
# install.packages("tidyverse")
#-------------------------------------------
library(tidyverse)

# Readding in the data into R
rail_data <- read.csv("Data\dataToAnalyze.csv", header = T, na.strings = c("", " "))
str(rail_data)
view(rail_data)


# SECTION 1 ---------------------------------------------------------------

# Network Rail Region Variable --------------------------------------------
  # Class description Table ----
NRR_table <- rail_data %>% 
  select(colnames(.)[2]) %>% 
  table(.)

  # Bar Plot ----------
NRR_barplot <- ggplot(data = rail_data) + 
  geom_bar(mapping = aes(x = Network.Rail.Region.of.station))

  # Summary table -----
NRR_sum <- summary(rail_data$Network.Rail.Region.of.station)

# EEE_Full ----------------------------------------------------------------

Entries_and_Exit_Full_Price <- rail_data$Entries...Exits_Full

  # Histogram of Full ------
eeeFull_hist <- ggplot(rail_data) + 
  geom_histogram(mapping = aes(Entries_and_Exit_Full_Price))

  # Summary of Full -------
eeeFull_sum <- summary(Entries_and_Exit_Full_Price)

  # Boxplot of Full -------
eeeFull_boxplot <- ggplot(rail_data) + 
  geom_boxplot(mapping = aes(Entries_and_Exit_Full_Price))

# EEE_Reduced -------------------------------------------------------------

Entries_and_Exit_Reduced_Price <- rail_data$Entries...Exits_Reduced

  # Histogram of Reduced -------
eeeReduced_hist <- ggplot(rail_data) + 
  geom_histogram(mapping = aes(Entries_and_Exit_Reduced_Price))

  # Summary of Reduced --------
eeeReduced_sum <- summary(Entries_and_Exit_Reduced_Price)

  # Boxplot of Reduced ---------
eeeReduced_boxplot <- ggplot(rail_data) + 
  geom_boxplot(mapping = aes(Entries_and_Exit_Reduced_Price))

# EEE_Season -------------------------------------------------------------

Entries_and_Exit_Season <- rail_data$Entries...Exits_Season

  # Histogram of Season -------
eeeSeason_hist <- ggplot(rail_data) + 
  geom_histogram(mapping = aes(Entries_and_Exit_Season))

  # Summary of Season --------
eeeSeason_sum <- summary(Entries_and_Exit_Season)

  # Boxplot of Season ---------
eeeSeason_boxplot <- ggplot(rail_data) + 
  geom_boxplot(mapping = aes(Entries_and_Exit_Season))

# EEE_Station1819 -----------------------------------------------------

Entries_and_Exit_Station1819 <- rail_data$X1819.Entries...Exits

  # Histogram of Station1819 -------
eeeStation1819_hist <- ggplot(rail_data) + 
  geom_histogram(mapping = aes(Entries_and_Exit_Station1819))

  # Summary of Station1819 --------
eeeStation1819_sum <- summary(Entries_and_Exit_Station1819)

  # Boxplot of Station1819 ---------
eeeStation1819_boxplot <- ggplot(rail_data) + 
  geom_boxplot(mapping = aes(Entries_and_Exit_Station1819))

# Intechange_Station1819 ----------------------------------------------

Interchange_Station1819 <- rail_data$X1819.Interchanges

  # Histogram of Intechange_Station1819 -------
interchangeStation1819_hist <- ggplot(rail_data) + 
  geom_histogram(mapping = aes(Interchange_Station1819))

  # Summary of Intechange_Station1819 --------
interchangeStation1819_sum <- summary(Interchange_Station1819)

  # Boxplot of Intechange_Station1819 ---------
interchangeStation1819_boxplot <- ggplot(rail_data) + 
  geom_boxplot(mapping = aes(Interchange_Station1819))



# SECTION 2 --------------------------------------------------------



# Relationship between Network Rail Region and other variables ------------

NRR <- rail_data$Network.Rail.Region.of.station

  # NRR and EEE_Full -------
NRR_eeeFull <- tapply(Entries_and_Exit_Full_Price, NRR, mean)

  # NRR and EEE_Redced -----
NRR_eeeReduced <- tapply(Entries_and_Exit_Reduced_Price, NRR, mean)

  # NRR nd EEE_Season ------
NRR_eeeSeason <- tapply(Entries_and_Exit_Season, NRR, mean)

 # NRR nd EEE_Station1819 ------
NRR_eeeStation1819 <- tapply(Entries_and_Exit_Station1819, NRR, mean)

# NRR nd Interchange_Station1819 ------
NRR_interchangeStation1819 <- tapply(Interchange_Station1819, NRR, mean)


# Relationship between the continuous variables ---------------------------

  # Scatteplot Matrix of the variables --------
continuous_var <- rail_data[, -c(1,2)]
colnames(continuous_var) <- 
  c("EEE_Full", "EEE_Reduced", "EEE_Season", "EEE_Station1819", 
    "Interchange_Station1819")
str(continuous_var)
scat_plot_mat <- plot(continuous_var)

  # Correlation Analysis --------
cor.test(continuous_var$EEE_Season, continuous_var$EEE_Reduced)
cor.test(continuous_var$EEE_Season, continuous_var$EEE_Station1819)
cor.test(continuous_var$EEE_Season, continuous_var$EEE_Full)
cor.test(continuous_var$EEE_Season, continuous_var$Interchange_Station1819)
cor.test(continuous_var$EEE_Reduced, continuous_var$Interchange_Station1819)
cor.test(continuous_var$EEE_Full, continuous_var$Interchange_Station1819)
cor.test(continuous_var$Interchange_Station1819, continuous_var$EEE_Station1819)



# SECTION 3 ---------------------------------------------------------------

test_data <- as.data.frame(cbind(NRR, continuous_var))
head(test_data)

#----- Paired samples comparison test ------------------
wilcox.test(x = continuous_var$EEE_Station1819, y = continuous_var$EEE_Season, 
            alternative = "two.sided", paired = T)

#----- Independent sample comparison test --------------
# The grouping factr must have exacly 2 levels; so we filter the 2 groups we want as follow

rail.region <- rail_data$Network.Rail.Region.of.station
# The above gets the variable "Network Rail Region of Stations" fom the original data (rail_data)
# and stores it in a new variable named "rail.region". This is done to give the variable a short
# name compared to its original name.


dataToFilter <- as.data.frame(cbind(rail.region, continuous_var))
# The above joins the vector "rail.region" to the data frame "continous_var" and the resulting
# data frame is stored in the new variable named "dataToFilter" which will be used for the test.



# head(dataToFilter)

indSamp_data <- dataToFilter %>% 
  filter(., rail.region == "Eastern" | rail.region == "Wales & Western")
View(indSamp_data)
# The above filters out the rows with value "Eastern" or "Wales & Western" as these are the 2
# regions to beconsidered in the independent sample test.

wilcox.test(indSamp_data$Interchange_Station1819 ~ indSamp_data$rail.region,
            alternative = "two.sided")

