
# The development of the multiple Regression models -----------------------

# Season Ticket usage

SeasonUsage_FitAll <- lm(EEE_Season ~ factor(rail.region) + EEE_Full + EEE_Reduced +
                           EEE_Station1819 + Interchange_Station1819, data = indSamp_data)
summary(SeasonUsage_FitAll)

# Interchange usage

InterchangeUsage_FitAll <- lm(Interchange_Station1819 ~ factor(rail.region) + EEE_Full + EEE_Reduced +
                                EEE_Station1819 + EEE_Season, data = indSamp_data)
summary(InterchangeUsage_FitAll)


# Model selection procedures ----------------------------------------------

# Season ticket's model selections -------------------------------------

# Forward selection

model_NoIndependent_seasonTicket <- lm(EEE_Season ~ 1, data = indSamp_data)
summary(model_NoIndependent_seasonTicket)

fs_season <- step(model_NoIndependent_seasonTicket, scope = 
                    list(upper = SeasonUsage_FitAll, lower = model_NoIndependent_seasonTicket,
                         direction = "forward", trace = FALSE))

# Backward Elimination

be_season <- step(SeasonUsage_FitAll, scope = 
                    list(upper = SeasonUsage_FitAll, lower = model_NoIndependent_seasonTicket,
                         direction = "backward", trace = FALSE))
# SeasonUsage_FitAll is a perfect fit, hence, model selection thros an error specifying that 
# the procedure is unnecassary and describes it as "nonsense"

# Stepwise Regression

sr_season <- step(model_NoIndependent_seasonTicket, scope = 
                    list(upper = SeasonUsage_FitAll, lower = model_NoIndependent_seasonTicket,
                         direction = "both", trace = FALSE))

# Interchange usage's model selections -------------------------------------

# Forward selection

model_NoIndependent_interchange <- lm(Interchange_Station1819 ~ 1, data = indSamp_data)
summary(model_NoIndependent_interchange)

fs_interchange <- step(model_NoIndependent_interchange, scope = 
                         list(upper = InterchangeUsage_FitAll, lower = model_NoIndependent_interchange,
                              direction = "forward", trace = FALSE))

# Backward Elimination

be_interchange <- step(InterchangeUsage_FitAll, scope = 
                         list(upper = InterchangeUsage_FitAll, lower = model_NoIndependent_interchange,
                              direction = "backward", trace = FALSE))
summary(be_interchange)

# Stepwise Regression

sr_interchange <- step(model_NoIndependent_interchange, scope = 
                          list(upper = InterchangeUsage_FitAll, lower = model_NoIndependent_interchange,
                               direction = "both", trace = FALSE))

#------------------------------------------------------------------------------#
# for (i in seq_along(list(fs_interchange, be_interchange, sr_interchange))) {
#   print(summary(i))
# }
#------------------------------------------------------------------------------#

# Final Models ------------------------------------------------------------

# Final model for Season Ticket usage

SeasonUsage_FitAll # This is the  final model as it is a perfect fit for the entire data points

# Final model for the interchanges made at the station

interchange_final.model <- lm(Interchange_Station1819 ~ EEE_Station1819, data = indSamp_data)
summary(interchange_final.model)


# Assumptions of the multiple linear regression ---------------------------

# The 4 main assumptions of the multiple linear regression are:

# 1) Linear relationship between dependent variable and each of the independent variables
# 2) Multivariate Normality
# 3) No Multicollinearity
# 4) Homoscedasticity

