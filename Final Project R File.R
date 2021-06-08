library(tidyverse)
library(ivreg)
library(clusterSEs)
library(estimatr)
library(miceadds)
library(sandwich)
library(stargazer)
library(texreg)
library(modelsummary)

############ DATA PREP #######################################################
#first we import the tax haven data
IRS_data_path <- '/Users/zachmariani/Desktop/Metrics/Final Project/Data/bycountry.csv'
tax_haven_data <- read.csv(IRS_data_path)


#then we select down the data that we need 
tax_haven_data <- tax_haven_data %>% select('country_code', 'Year', 'country', 
     'Number.of.foreign.corporations', 'Number.of.US.corporations.returns', 
    'Total.Assets', 'Total.Receipts', 
    'Current.earnings.and.profits..less.deficit..before.taxes')
    

#Now we have a clean panel of CFC data from the IRS ready to work with 


head(tax_haven_data)

#We are going to measure the "goodness" of a tax haven with these four data points
#we set them up here 

#percent of total number of foreign corporations in this country 
tax_haven_data$percent_Number.of.foreign.corporations = 0

#percent of total assets in this country
tax_haven_data$percent_Total.Assets = 0 

#percent of total receipts in this country 
tax_haven_data$percent_Total.Receipts = 0

#percent of total current earnings and profits in this country 
tax_haven_data$percent_Current.earnings.and.profits..less.deficit..before.taxes = 0

#store it in a vector to speed things up 
vars_of_interest <- c('percent_Number.of.foreign.corporations', 'percent_Total.Assets', 
                    'percent_Total.Receipts', 'percent_Current.earnings.and.profits..less.deficit..before.taxes')

#we need this to get the percentages!
alls <- tax_haven_data[tax_haven_data$country_code == 'ALL', ]

#albeit the double loop is definetly not the fastest way to do this, I would 
#have definitely preferred Pandas, but I'm not that familiar with R, and this 
#was the best way I knew to do something like this 

#now for every variable we're interested in 
for (var in vars_of_interest){
 
  #go through every country
  for (row in 1:nrow(tax_haven_data)){
 
    #get the year
    yr <- tax_haven_data[row, 'Year']

    #get the yr specific value
    var_no_per <- substr(var,9,nchar(var))
    
    yr_spec <- tax_haven_data[row, var_no_per]
    
    #filter alls by year
    all_value <- alls[alls$Year == yr,]
    
    #get the right value 
    all_value <- all_value[1, var_no_per]
    
    #get the percentage to store
    pct_to_store <- yr_spec / all_value 
    
    #store it back in the dataframe 
    tax_haven_data[row, var] <- pct_to_store
  }
  
}


#We're interested in countries that over the time period have become better or worse 
# tax havens. In order to figure out how much better or worse a country has become, 
# we will regress the variable of interest against time, then we will sort by the 
# $\beta$ parameter and also keep track of the R^2 value which gives us some idea of how
# uniform the improvement / de-improvement has been. Because this data is a bit messy 
# i.e. some rows are missing things, not all data there, etc. we will do this in Python 

# Further, now that we have generated our four percent params, we need to figure out 
# what threshold to use to determine who is becoming better and who is becoming worse.

# We do this by plotting each of the four vars of interest for ech country over time 
# Given that R's data visualization in this case is inferior to Python's we will switch over to 
# Python for now. 

write.csv(tax_haven_data, '/Users/zachmariani/Desktop/Metrics/Final Project/Data/tax_haven1.csv')


################# IMPORTING CLEANED PANEL FROM PYTHON ##########################

# Now that we have cleaned and prepared our panel in Python, 
# we will begin our analysis. 

clean_panel <- read.csv('/Users/zachmariani/Desktop/Metrics/Final Project/Data/full_panel.csv')
head(clean_panel)

##################### CHECKING FOR WEAK INSTRUMENTS ############################
# In order to make sure that we don't have weak instruments, we do a first stage 
# regression for each X on Z, we then take the F statistic of this first stage regression
# and make sure that it is above 10. 
# https://www.statisticshowto.com/weak-instrument-definition/

fs_IV_x_p_cur_EP <- lm.cluster(data = clean_panel, formula = x_p_cur_EP ~ IV_interaction + c_land + c_resource_endowment + c_population + 
                             c_rGDP + factor(country_code) + factor(year), cluster = 'country_code')

summary(fs_IV_x_p_cur_EP)$fstatistic[1]
#reported 11.25

fs_IV_x_p_total_receipts <- lm.cluster(data = clean_panel, formula = x_p_total_receipts ~ IV_interaction + c_land + c_resource_endowment + c_population + 
                                 c_rGDP + factor(country_code) + factor(year), cluster = 'country_code')
summary(fs_IV_x_p_total_receipts)$fstatistic[1]
#reported 20.27

fs_IV_x_p_total_assets <- lm.cluster(data = clean_panel, formula = x_p_total_assets ~ IV_interaction + c_land + c_resource_endowment + c_population + 
                               c_rGDP + factor(country_code) + factor(year) + factor(year), cluster = 'country_code')
summary(fs_IV_x_p_total_assets)$fstatistic[1]
#reported 8.76

fs_IV_x_p_num_for_corps <- lm.cluster(data = clean_panel, formula = x_p_num_for_corps ~ IV_interaction + c_land + c_resource_endowment + c_population + 
                                c_rGDP + factor(country_code) + factor(year), cluster = 'country_code')
summary(fs_IV_x_p_num_for_corps)$fstatistic[1]
#reported 71.55


############# CHECKING FOR PRE TRENDS #########################################

#import the shifted panel 
shifted_panel <- read.csv('/Users/zachmariani/Desktop/Metrics/Final Project/Data/full_panel_shifted.csv')
head(shifted_panel)


############################### X P CURR EP ############################
s1_x_p_curr_EP <- lm.cluster(formula=shift1x_p_cur_EP ~  IV_interaction + c_land + c_resource_endowment + c_population + 
                             c_rGDP + factor(country_code) + factor(year), data = shifted_panel, cluster = 'country_code')
coeftest(s1_x_p_curr_EP, vcov = vcov(s1_x_p_curr_EP, type = 'HC0'))
#NOT statistically significant 


s2_x_p_curr_EP <- lm.cluster(formula=shift2x_p_cur_EP ~  IV_interaction + c_land + c_resource_endowment + c_population + 
                               c_rGDP + factor(country_code) + factor(year), data = shifted_panel, cluster = 'country_code')
coeftest(s2_x_p_curr_EP, vcov = vcov(s2_x_p_curr_EP, type = 'HC0'))
#NOT statistically significant 

############################### X P TOTAL RECEIPTS  ############################

s1_x_p_total_receipts <- lm.cluster(formula=shift1x_p_total_receipts ~  IV_interaction + c_land + c_resource_endowment + c_population + 
                               c_rGDP + factor(country_code) + factor(year), data = shifted_panel, cluster = 'country_code')
coeftest(s1_x_p_total_receipts, vcov = vcov(s1_x_p_total_receipts, type = 'HC0'))
#NOT statistically significant 


s2_x_p_total_receipts <- lm.cluster(formula=shift2x_p_total_receipts ~  IV_interaction + c_land + c_resource_endowment + c_population + 
                               c_rGDP + factor(country_code) + factor(year), data = shifted_panel, cluster = 'country_code')
coeftest(s2_x_p_total_receipts, vcov = vcov(s2_x_p_total_receipts, type = 'HC0'))
#NOT statistically significant 

############################### X P TOTAL ASSETS  ############################

s1_x_p_total_assets <- lm.cluster(formula=shift1x_p_total_assets ~  IV_interaction + c_land + c_resource_endowment + c_population + 
                                      c_rGDP + factor(country_code) + factor(year), data = shifted_panel, cluster = 'country_code')
coeftest(s1_x_p_total_assets, vcov = vcov(s1_x_p_total_assets, type = 'HC0'))
#NOT statistically significant 


s2_x_p_total_assets <- lm.cluster(formula=shift2x_p_total_assets ~  IV_interaction + c_land + c_resource_endowment + c_population + 
                                      c_rGDP + factor(country_code) + factor(year), data = shifted_panel, cluster = 'country_code')
coeftest(s2_x_p_total_assets, vcov = vcov(s2_x_p_total_assets, type = 'HC0'))
#NOT statistically significant


############################### X P TOTAL ASSETS  ############################

s1_x_p_total_assets <- lm.cluster(formula=shift1x_p_total_assets ~  IV_interaction + c_land + c_resource_endowment + c_population + 
                                    c_rGDP + factor(country_code) + factor(year), data = shifted_panel, cluster = 'country_code')
coeftest(s1_x_p_total_assets, vcov = vcov(s1_x_p_total_assets, type = 'HC0'))
#NOT statistically significant 


s2_x_p_total_assets <- lm.cluster(formula=shift2x_p_total_assets ~  IV_interaction + c_land + c_resource_endowment + c_population + 
                                    c_rGDP + factor(country_code) + factor(year), data = shifted_panel, cluster = 'country_code')
coeftest(s2_x_p_total_assets, vcov = vcov(s2_x_p_total_assets, type = 'HC0'))
#NOT statistically significant


############################### X P NUM FOR CORPS  ############################

s1_x_p_num_for_corps <- lm.cluster(formula=shift1x_p_num_for_corps ~  IV_interaction + c_land + c_resource_endowment + c_population + 
                                    c_rGDP + factor(country_code) + factor(year), data = shifted_panel, cluster = 'country_code')
coeftest(s1_x_p_num_for_corps, vcov = vcov(s1_x_p_num_for_corps, type = 'HC0'))
#NOT statistically significant 


s2_x_p_num_for_corps <- lm.cluster(formula=shift2x_p_num_for_corps ~  IV_interaction + c_land + c_resource_endowment + c_population + 
                                    c_rGDP + factor(country_code) + factor(year), data = shifted_panel, cluster = 'country_code')
coeftest(s2_x_p_num_for_corps, vcov = vcov(s2_x_p_num_for_corps, type = 'HC0'))
#NOT statistically significant 




################## RUNNING FULL IV REGRESSIONS #################################

# NB: I've included SE by default here, it uses se_type = 'CR2' which is the stata 
# equivalent 

################# X as Percent of Current Earnings and Profits #################

# In this section, we regress the primary x variable (percent of all current earnings and profits 
# booked globally in any given tax haven) against the Y variables of interest.

#https://declaredesign.org/r/estimatr/reference/iv_robust.html
#IV_robust is just a better version of R's default ivreg() 
#it allows us to cluster by country_code which is important for our project! 
IV_gov_expend <- iv_robust(y_gov_expend ~ x_p_cur_EP + c_land + c_resource_endowment + c_population + 
                             c_rGDP | c_land + c_resource_endowment + c_population + 
                             c_rGDP + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_gov_expend)

IV_HDI <- iv_robust(y_HDI ~ x_p_cur_EP + c_land + c_resource_endowment + c_population + 
                      c_rGDP | c_land + c_resource_endowment + c_population + 
                      c_rGDP + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_HDI)

IV_life_expectancy <- iv_robust(y_life_expectancy ~ x_p_cur_EP + c_land + c_resource_endowment + c_population + 
                                  c_rGDP | c_land + c_resource_endowment + c_population + 
                                  c_rGDP + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_life_expectancy)

IV_homicides <- iv_robust(y_homicides ~ x_p_cur_EP + c_land + c_resource_endowment + c_population + 
                            c_rGDP | c_land + c_resource_endowment + c_population + 
                            c_rGDP + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_homicides)

IV_deaths_under_14 <- iv_robust(y_deaths_under_14 ~ x_p_cur_EP + c_land + c_resource_endowment + c_population + 
            c_rGDP | c_land + c_resource_endowment + c_population + 
            c_rGDP + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_deaths_under_14)


######################### X as Percent of Total Receipts #######################

IV_gov_expend_1 <- iv_robust(y_gov_expend ~ x_p_total_receipts + c_land + c_resource_endowment + c_population + 
                             c_rGDP | c_land + c_resource_endowment + c_population + 
                             c_rGDP + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_gov_expend_1)

IV_HDI_1 <- iv_robust(y_HDI ~ x_p_total_receipts + c_land + c_resource_endowment + c_population + 
                      c_rGDP | c_land + c_resource_endowment + c_population + 
                      c_rGDP + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_HDI_1)

IV_life_expectancy_1 <- iv_robust(y_life_expectancy ~ x_p_total_receipts + c_land + c_resource_endowment + c_population + 
                                  c_rGDP | c_land + c_resource_endowment + c_population + 
                                  c_rGDP + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_life_expectancy_1)

IV_homicides_1 <- iv_robust(y_homicides ~ x_p_total_receipts + c_land + c_resource_endowment + c_population + 
                            c_rGDP | c_land + c_resource_endowment + c_population + 
                            c_rGDP + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_homicides_1)

IV_deaths_under_14_1 <- iv_robust(y_deaths_under_14 ~ x_p_total_receipts + c_land + c_resource_endowment + c_population + 
                                  c_rGDP | c_land + c_resource_endowment + c_population + 
                                  c_rGDP + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_deaths_under_14_1)

####################### X as Percent of Total Assets ###########################

IV_gov_expend_2 <- iv_robust(y_gov_expend ~ x_p_total_assets + c_land + c_resource_endowment + c_population + 
                               c_rGDP | c_land + c_resource_endowment + c_population + 
                               c_rGDP + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_gov_expend_2)

IV_HDI_2 <- iv_robust(y_HDI ~ x_p_total_assets + c_land + c_resource_endowment + c_population + 
                        c_rGDP | c_land + c_resource_endowment + c_population + 
                        c_rGDP + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_HDI_2)

IV_life_expectancy_2 <- iv_robust(y_life_expectancy ~ x_p_total_assets + c_land + c_resource_endowment + c_population + 
                                    c_rGDP | c_land + c_resource_endowment + c_population + 
                                    c_rGDP + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_life_expectancy_2)

IV_homicides_2 <- iv_robust(y_homicides ~ x_p_total_assets + c_land + c_resource_endowment + c_population + 
                              c_rGDP | c_land + c_resource_endowment + c_population + 
                              c_rGDP + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_homicides_2)

IV_deaths_under_14_2 <- iv_robust(y_deaths_under_14 ~ x_p_total_assets + c_land + c_resource_endowment + c_population + 
                                    c_rGDP | c_land + c_resource_endowment + c_population + 
                                    c_rGDP + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_deaths_under_14_2)

##################### X as Percent of Number of Corporations ###################

IV_gov_expend_3 <- iv_robust(y_gov_expend ~ x_p_num_for_corps + c_land + c_resource_endowment + c_population + 
                               c_rGDP | c_land + c_resource_endowment + c_population + 
                               c_rGDP + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_gov_expend_3)

IV_HDI_3 <- iv_robust(y_HDI ~ x_p_num_for_corps + c_land + c_resource_endowment + c_population + 
                        c_rGDP | c_land + c_resource_endowment + c_population + 
                        c_rGDP + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
library(modelsummary)
modelsummary(IV_HDI_3)
#THIS IS OUR ONLY SIG RESULT, EXPORT IT! 

IV_life_expectancy_3 <- iv_robust(y_life_expectancy ~ x_p_num_for_corps + c_land + c_resource_endowment + c_population + 
                                    c_rGDP | c_land + c_resource_endowment + c_population + 
                                    c_rGDP + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_life_expectancy_3)

IV_homicides_3 <- iv_robust(y_homicides ~ x_p_num_for_corps + c_land + c_resource_endowment + c_population + 
                              c_rGDP | c_land + c_resource_endowment + c_population + 
                              c_rGDP + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_homicides_3)

IV_deaths_under_14_3 <- iv_robust(y_deaths_under_14 ~ x_p_num_for_corps + c_land + c_resource_endowment + c_population + 
                                    c_rGDP | c_land + c_resource_endowment + c_population + 
                                    c_rGDP + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_deaths_under_14_3)





################################# ################################# #################################







################## RUNNING BAD CONTROLS REGRESSIONS #################################

# NB: I've included SE by default here, it uses se_type = 'CR2' which is the stata 
# equivalent 

################# X as Percent of Current Earnings and Profits #################

# In this section, we regress the primary x variable (percent of all current earnings and profits 
# booked globally in any given tax haven) against the Y variables of interest.

#https://declaredesign.org/r/estimatr/reference/iv_robust.html
#IV_robust is just a better version of R's default ivreg() 
#it allows us to cluster by country_code which is important for our project! 

IV_gov_expend <- iv_robust(y_gov_expend ~ x_p_cur_EP + c_land + c_resource_endowment + c_population 
                              | c_land + c_resource_endowment + c_population + 
                              IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_gov_expend)



IV_HDI <- iv_robust(y_HDI ~ x_p_cur_EP + c_land + c_resource_endowment + c_population 
                     | c_land + c_resource_endowment + c_population + 
                      IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_HDI)

IV_life_expectancy <- iv_robust(y_life_expectancy ~ x_p_cur_EP + c_land + c_resource_endowment + c_population  
                                  | c_land + c_resource_endowment + c_population + 
                                   + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_life_expectancy)

IV_homicides <- iv_robust(y_homicides ~ x_p_cur_EP + c_land + c_resource_endowment + c_population  
                             | c_land + c_resource_endowment + c_population + 
                             IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_homicides)

IV_deaths_under_14 <- iv_robust(y_deaths_under_14 ~ x_p_cur_EP + c_land + c_resource_endowment + c_population  
                                 | c_land + c_resource_endowment + c_population 
                                   + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_deaths_under_14)


######################### X as Percent of Total Receipts #######################

IV_gov_expend_1 <- iv_robust(y_gov_expend ~ x_p_total_receipts + c_land + c_resource_endowment + c_population 
                                | c_land + c_resource_endowment + c_population 
                                + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_gov_expend_1)

IV_HDI_1 <- iv_robust(y_HDI ~ x_p_total_receipts + c_land + c_resource_endowment + c_population  
                         | c_land + c_resource_endowment + c_population + 
                          IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_HDI_1)

IV_life_expectancy_1 <- iv_robust(y_life_expectancy ~ x_p_total_receipts + c_land + c_resource_endowment + c_population 
                                     | c_land + c_resource_endowment + c_population 
                                     + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_life_expectancy_1)

IV_homicides_1 <- iv_robust(y_homicides ~ x_p_total_receipts + c_land + c_resource_endowment + c_population  
                               | c_land + c_resource_endowment + c_population + 
                                IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_homicides_1)

IV_deaths_under_14_1 <- iv_robust(y_deaths_under_14 ~ x_p_total_receipts + c_land + c_resource_endowment + c_population  
                                     | c_land + c_resource_endowment + c_population  
                                     + IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_deaths_under_14_1)

####################### X as Percent of Total Assets ###########################

IV_gov_expend_2 <- iv_robust(y_gov_expend ~ x_p_total_assets + c_land + c_resource_endowment + c_population  
                                | c_land + c_resource_endowment + c_population + 
                                IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_gov_expend_2)

IV_HDI_2 <- iv_robust(y_HDI ~ x_p_total_assets + c_land + c_resource_endowment + c_population  
                         | c_land + c_resource_endowment + c_population + 
                          IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_HDI_2)

IV_life_expectancy_2 <- iv_robust(y_life_expectancy ~ x_p_total_assets + c_land + c_resource_endowment + c_population  
                                     | c_land + c_resource_endowment + c_population + 
                                      IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_life_expectancy_2)

IV_homicides_2 <- iv_robust(y_homicides ~ x_p_total_assets + c_land + c_resource_endowment + c_population 
                               | c_land + c_resource_endowment + c_population + 
                               IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_homicides_2)

IV_deaths_under_14_2 <- iv_robust(y_deaths_under_14 ~ x_p_total_assets + c_land + c_resource_endowment + c_population 
                                     | c_land + c_resource_endowment + c_population + 
                                      IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_deaths_under_14_2)

##################### X as Percent of Number of Corporations ###################

IV_gov_expend_3 <- iv_robust(y_gov_expend ~ x_p_num_for_corps + c_land + c_resource_endowment + c_population 
                                | c_land + c_resource_endowment + c_population + 
                                IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_gov_expend_3)

IV_HDI_3 <- iv_robust(y_HDI ~ x_p_num_for_corps + c_land + c_resource_endowment + c_population 
                         | c_land + c_resource_endowment + c_population + 
                         IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_HDI_3)

IV_life_expectancy_3 <- iv_robust(y_life_expectancy ~ x_p_num_for_corps + c_land + c_resource_endowment + c_population 
                                     | c_land + c_resource_endowment + c_population + 
                                      IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_life_expectancy_3)

IV_homicides_3 <- iv_robust(y_homicides ~ x_p_num_for_corps + c_land + c_resource_endowment + c_population 
                               | c_land + c_resource_endowment + c_population + 
                               IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_homicides_3)

IV_deaths_under_14_3 <- iv_robust(y_deaths_under_14 ~ x_p_num_for_corps + c_land + c_resource_endowment + c_population 
                                     | c_land + c_resource_endowment + c_population +
                                      IV_interaction + factor(year) + factor(country_code), data = clean_panel, clusters = country_code)
summary(IV_deaths_under_14_3)






############## ONLY 2SLS REGRESSING Y ON X AND THE CONTROLS W OUT IV ###########
# We regress Y on X with the controls here without the IV 


################# X as Percent of Current Earnings and Profits #################

# In this section, we regress the primary x variable (percent of all current earnings and profits 
# booked globally in any given tax haven) against the Y variables of interest.

#https://declaredesign.org/r/estimatr/reference/iv_robust.html
#IV_robust is just a better version of R's default ivreg() 
#it allows us to cluster by country_code which is important for our project! 
TSLS_gov_expend <- lm.cluster(clean_panel, y_gov_expend ~ x_p_cur_EP + c_land + c_resource_endowment + c_population + 
                                c_rGDP + factor(year) + factor(country_code), 'country_code')
summary(TSLS_gov_expend)


TSLS_HDI <- lm.cluster(clean_panel, y_HDI ~ x_p_cur_EP + c_land + c_resource_endowment + c_population + 
                         c_rGDP + factor(year) + factor(country_code), 'country_code')
summary(TSLS_HDI)

TSLS_life_expectancy <- lm.cluster(clean_panel, y_life_expectancy ~ x_p_cur_EP + c_land + c_resource_endowment + c_population + 
                                     c_rGDP + factor(year) + factor(country_code), 'country_code')
summary(TSLS_life_expectancy)

TSLS_homicides <- lm.cluster(clean_panel, y_homicides ~ x_p_cur_EP + c_land + c_resource_endowment + c_population + 
                               c_rGDP + factor(year) + factor(country_code), 'country_code')
summary(TSLS_homicides)

TSLS_deaths_under_14 <- lm.cluster(clean_panel, y_deaths_under_14 ~ x_p_cur_EP + c_land + c_resource_endowment + c_population + 
                               c_rGDP + factor(year) + factor(country_code), 'country_code')
summary(TSLS_deaths_under_14)


######################### X as Percent of Total Receipts #######################

TSLS_gov_expend_1 <- lm.cluster(clean_panel, y_gov_expend ~ x_p_total_receipts + c_land + c_resource_endowment + c_population + 
                                c_rGDP + factor(year) + factor(country_code), 'country_code')
summary(TSLS_gov_expend_1)


TSLS_HDI_1 <- lm.cluster(clean_panel, y_HDI ~ x_p_total_receipts + c_land + c_resource_endowment + c_population + 
                         c_rGDP + factor(year) + factor(country_code), 'country_code')
summary(TSLS_HDI_1)

TSLS_life_expectancy_1 <- lm.cluster(clean_panel, y_life_expectancy ~ x_p_total_receipts + c_land + c_resource_endowment + c_population + 
                                     c_rGDP + factor(year) + factor(country_code), 'country_code')
summary(TSLS_life_expectancy_1)

TSLS_homicides_1 <- lm.cluster(clean_panel, y_homicides ~ x_p_total_receipts + c_land + c_resource_endowment + c_population + 
                               c_rGDP + factor(year) + factor(country_code), 'country_code')
summary(TSLS_homicides_1)

TSLS_deaths_under_14_1 <- lm.cluster(clean_panel, y_deaths_under_14 ~ x_p_total_receipts + c_land + c_resource_endowment + c_population + 
                                     c_rGDP + factor(year) + factor(country_code), 'country_code')
summary(TSLS_deaths_under_14_1)
####################### X as Percent of Total Assets ###########################

TSLS_gov_expend_2 <- lm.cluster(clean_panel, y_gov_expend ~ x_p_total_assets + c_land + c_resource_endowment + c_population + 
                                  c_rGDP + factor(year) + factor(country_code), 'country_code')
summary(TSLS_gov_expend_2)


TSLS_HDI_2 <- lm.cluster(clean_panel, y_HDI ~ x_p_total_assets + c_land + c_resource_endowment + c_population + 
                           c_rGDP + factor(year) + factor(country_code), 'country_code')
summary(TSLS_HDI_2)

TSLS_life_expectancy_2 <- lm.cluster(clean_panel, y_life_expectancy ~ x_p_total_assets + c_land + c_resource_endowment + c_population + 
                                       c_rGDP + factor(year) + factor(country_code), 'country_code')
summary(TSLS_life_expectancy_2)

TSLS_homicides_2 <- lm.cluster(clean_panel, y_homicides ~ x_p_total_assets + c_land + c_resource_endowment + c_population + 
                                 c_rGDP + factor(year) + factor(country_code), 'country_code')
summary(TSLS_homicides_2)

TSLS_deaths_under_14_2 <- lm.cluster(clean_panel, y_deaths_under_14 ~ x_p_total_assets + c_land + c_resource_endowment + c_population + 
                                       c_rGDP + factor(year) + factor(country_code), 'country_code')
summary(TSLS_deaths_under_14_2)

##################### X as Percent of Number of Corporations ###################

TSLS_gov_expend_3 <- lm.cluster(clean_panel, y_gov_expend ~ x_p_num_for_corps + c_land + c_resource_endowment + c_population + 
                                  c_rGDP + factor(year) + factor(country_code), 'country_code')
summary(TSLS_gov_expend_3)


TSLS_HDI_3 <- lm.cluster(clean_panel, y_HDI ~ x_p_num_for_corps + c_land + c_resource_endowment + c_population + 
                           c_rGDP + factor(year) + factor(country_code), 'country_code')
summary(TSLS_HDI_3)

TSLS_life_expectancy_3 <- lm.cluster(clean_panel, y_life_expectancy ~ x_p_num_for_corps + c_land + c_resource_endowment + c_population + 
                                       c_rGDP + factor(year) + factor(country_code), 'country_code')
summary(TSLS_life_expectancy_3) 

TSLS_homicides_3 <- lm.cluster(clean_panel, y_homicides ~ x_p_num_for_corps + c_land + c_resource_endowment + c_population + 
                                 c_rGDP + factor(year) + factor(country_code), 'country_code')
summary(TSLS_homicides_3)

TSLS_deaths_under_14_3 <- lm.cluster(clean_panel, y_deaths_under_14 ~ x_p_num_for_corps + c_land + c_resource_endowment + c_population + 
                                       c_rGDP + factor(year) + factor(country_code), 'country_code')
summary(TSLS_deaths_under_14_3)


