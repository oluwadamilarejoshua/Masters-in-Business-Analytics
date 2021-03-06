
# The development of the multiple Regression models -----------------------

# Readding the data into R
rail_data <- read.csv("../Masters in Business Analytics/Data/dataToAnalyze.csv", 
                      header = T, na.strings = c("", " "))

continuous_var <- rail_data[, -c(1,2)]
colnames(continuous_var) <- 
  c("EEE_Full", "EEE_Reduced", "EEE_Season", "EEE_Station1819", "Interchange_Station1819")
rail.region <- rail_data$Network.Rail.Region.of.station
model_data <- as.data.frame(cbind(rail.region, continuous_var))

# Data preparation --------------------------------------------------------

# The na.omit() function was used to remove all missing values in the data in a way that anay row
# that contains a missing value is dropped

indSamp_data_modified <- na.omit(model_data)
summary(indSamp_data_modified)

# Season Ticket usage

SeasonUsage_FitAll <- lm(EEE_Season ~ factor(rail.region) + EEE_Full + EEE_Reduced + 
                           Interchange_Station1819, data = indSamp_data_modified)
summary(SeasonUsage_FitAll)

# Interchange usage

InterchangeUsage_FitAll <- lm(Interchange_Station1819 ~ factor(rail.region) + EEE_Full + 
                                EEE_Reduced + EEE_Season, data = indSamp_data_modified)
summary(InterchangeUsage_FitAll)


# Model selection procedures ----------------------------------------------

# Season ticket's model selections -------------------------------------

# Forward selection

model_NoIndependent_seasonTicket <- lm(EEE_Season ~ 1, data = indSamp_data_modified)
summary(model_NoIndependent_seasonTicket)

fs_season <- step(model_NoIndependent_seasonTicket, 
                  scope = list(upper = SeasonUsage_FitAll, 
                               lower = model_NoIndependent_seasonTicket, 
                               direction = "forward", trace = FALSE))
summary(fs_season)

# Backward Elimination

be_season <- step(SeasonUsage_FitAll, scope = 
                    list(upper = SeasonUsage_FitAll, lower = 
                           model_NoIndependent_seasonTicket, direction = 
                           "backward", trace = FALSE))
summary(be_season)

# SeasonUsage_FitAll is a perfect fit, hence, model selection thros an error specifying that 
# the procedure is unnecassary and describes it as "nonsense"

# Stepwise Regression

sr_season <- step(model_NoIndependent_seasonTicket, scope = 
                    list(upper = SeasonUsage_FitAll, lower = 
                           model_NoIndependent_seasonTicket, direction = "both", trace = FALSE))
summary(sr_season)

# Interchange usage's model selections -------------------------------------

# Forward selection

model_NoIndependent_interchange <- lm(Interchange_Station1819 ~ 1, data = indSamp_data_modified)
summary(model_NoIndependent_interchange)

fs_interchange <- step(model_NoIndependent_interchange, scope = 
                         list(upper = InterchangeUsage_FitAll, lower = 
                                model_NoIndependent_interchange, direction = 
                                "forward", trace = FALSE))

# Backward Elimination

be_interchange <- step(InterchangeUsage_FitAll, scope = 
                         list(upper = InterchangeUsage_FitAll, lower = 
                                model_NoIndependent_interchange, direction = 
                                "backward", trace = FALSE))

# Stepwise Regression

sr_interchange <- step(model_NoIndependent_interchange, 
                       scope = list(upper = InterchangeUsage_FitAll, lower = 
                                      model_NoIndependent_interchange, direction = 
                                      "both", trace = FALSE))

# Checking the selected vaiables to model ----------------------------------------#

optimized <- 
  list(fs_season, be_season, sr_season, fs_interchange, be_interchange, sr_interchange)
for (i in optimized) {
  print(i)
  print(summary(i))
}
#------------------------------------------------------------------------------#

# Final Models ------------------------------------------------------------

# Final model for Season Ticket usage -------------------------------------

# The three model selection procedures applied here selected the same set of variables as the 
# optimal. The model is presented below

finalModel_season <- lm(EEE_Season ~ EEE_Full + Interchange_Station1819, 
                        data = indSamp_data_modified)
summary(finalModel_season)

# Final model for the interchanges made at the station ---------------------------

# The three model selection procedures applied here selected the same set of variables as the 
# optimal. The model is presented below

finalModel_interchange <- lm(Interchange_Station1819 ~ EEE_Full + EEE_Reduced + 
                               EEE_Season, data = indSamp_data_modified)
summary(finalModel_interchange)

# Assumptions of the multiple linear regression ---------------------------

# Residual examination----------  

plot(finalModel_season)

plot(finalModel_interchange)
