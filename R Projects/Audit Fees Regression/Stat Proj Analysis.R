#--- Libraries Used ---#
library(tidyverse)
library(leaps)
library(forecast)

#--- DATA EXPLORATION ---#
#Read data into base + summary data
base = read_csv("final_project_data.csv")
options(scipen = 999, digits = 4)
summary(base)

#Count Number of NA Values Per Column
base_col = colSums(is.na(base))
base_col

#Filter variables with NA Values
na_variable_count = base_col[base_col != 0]
na_variable_count

#Drop variables not in use
base = base %>% 
  select(-c(f_score, guide, permno, comnam))

#Density Plot for all Numeric + Continuous variables in the data set
base %>% 
  select(-c("fca", "fyear", "is_in_nasdaq_composite", 
            "recent_guide", "restatement","treatment")) %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

#--- DATA CLEANING ---#
#DropNA for variables dltt, mve_crsp, ffi12_desc, ib, prcc, earnings_vol
base_1 = base %>% 
  drop_na(dltt, mve_crsp,ffi12_desc, ib, prcc, earnings_vol, ret_std)

#Replace NA Values in numest with 0
base_2 = base_1 %>%
  mutate(numest=ifelse(is.na(numest),0,numest))

#Replace NA values in lev, roa, mtb, ceq, actual using calculations
base_3 = base_2%>%
  mutate(lev=ifelse(is.na(lev), (dlc+dltt)/ceq,lev)) %>% #Debt to equity ratio
  mutate(roa=ifelse(is.na(roa), ib/at, roa)) %>% #Income/Assets
  mutate(mtb=ifelse(is.na(mtb), prcc/(ceq*1000/shrout), mtb)) %>% #Closing Price/(Equity/Shares)
  mutate(ceq = ifelse(is.na(ceq), at-dlc-dltt, ceq)) %>% #Assets - Liabilities
  mutate(actual=ifelse(is.na(actual), ib*1000/shrout, actual)) #Profit/Shares

# Function to Replace extreme observations using 95% and 5% percentiles
winsorize_x = function(x, cut = 0.05){
  cut_point_top <- quantile(x, 1 - cut, na.rm = T)
  cut_point_bottom <- quantile(x, cut, na.rm = T)
  i = which(x >= cut_point_top) 
  x[i] = cut_point_top
  j = which(x <= cut_point_bottom) 
  x[j] = cut_point_bottom
  return(x)
}
#Winsorise Variables with outliers using above function
outlier_variables = c("actual", "at", "audit_fees", "ceq", "dlc", "dltt",
                      "earnings_vol", "ib", "lev","mtb","mve_crsp",
                      "non_audit_fees", "prcc", "roa", "shrout", "xrd")
base_clean = base_3 %>% mutate_at(vars(outlier_variables), funs(winsorize_x))

#Select only numeric variables from base_clean
base_clean_numeric = base_clean %>% 
  select_if(is.numeric)

#--- DATA EXPLORATION AFTER DATA CLEANING ---#
#Density Plot for all Numeric variables in the dataset
base_clean_numeric %>%
  select(-c("fca", "fyear", "is_in_nasdaq_composite", 
            "recent_guide", "restatement","treatment")) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

#Find correlation for each numeric variable to audit fees
auditfee_corr = data.frame(cor(base_clean_numeric[-1], 
                               base_clean_numeric$audit_fees))

#Rename long variable name to correlation + Arrange by Descending Correlation
auditfee_corr = auditfee_corr %>% 
  rename("correlation" = names(auditfee_corr)) %>%
  arrange(desc(correlation))

#Set Theme to increase axis title + label font size + Plot Title Size for all plots
my_theme = theme(axis.text=element_text(size=24),
                 axis.title=element_text(size=24,face="bold"),
                 legend.title = element_text(size=24, face="bold"), 
                 legend.text = element_text(size=24),
                 plot.title = element_text(size=22))

#Focused correlation graph on all variables 
auditfee_corr %>% 
  ggplot(aes(x = rownames(auditfee_corr), y = correlation)) +
  geom_bar(stat = "identity", aes(fill=correlation>0)) +
  ylab("Correlation with audit_fees") + #Label Y-Axis
  xlab("Variable") + #Label X-Axis
  scale_fill_manual(values = c('Red', '#2AAA8A') ) + #Change color to red & green
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + # Rotate x-axis labels
  my_theme #Set Common Theme

#Filter for Treatment + 5 Hypothesis Variables
auditfee_corr_focused = auditfee_corr %>% 
  filter(row.names(auditfee_corr) %in% c("treatment", "non_audit_fees", "at",
                                         "mve_crsp", "ib", "dltt"))
#Focused correlation graph on hypothesis variables
auditfee_corr_focused %>% 
  ggplot(aes(x = rownames(auditfee_corr_focused), y = correlation)) +
  geom_bar(stat = "identity", aes(fill=correlation>0)) +
  ylab("Correlation with audit_fees") + #Label Y-Axis
  xlab("Variable") + #Label X-Axis
  scale_fill_manual(values = c('Red', '#2AAA8A') ) + #Change color to red & green
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + # Rotate x-axis labels
  my_theme #Set Common Theme

#Segregate Training & Testing Data
base_train = sample_frac(base_clean_numeric, 0.6)
base_test = anti_join(base_clean_numeric, base_train)

#Linear Regression for Audit Fees ~ treatment + 5 Relevant Variables [Train]
base_reg_all = lm(formula = audit_fees ~ treatment + non_audit_fees+ at + 
                    mve_crsp + ib + dltt, data=base_train)
summary(base_reg_all) #Display Regression Results
base_reg_all_pred = predict(base_reg_all, base_test)
accuracy(base_reg_all_pred, base_test$audit_fees) #Display Accuracy of Regression

#Visualise Treatment's relation to audit_fees
base_clean_numeric %>% 
  #Temp Column to group data by exposure to FCA before 2011
  mutate(expose = ifelse(treatment==1, "not_exposed", "exposed")) %>% 
  ggplot(aes(x = audit_fees, 
             fill = expose)) +
  geom_density(alpha = 0.4) +
  labs(title = "Audit Fee Distribution by FCA Exposure") + #Title of Graph
  scale_fill_manual(values = c('#FF5F1F', 'Blue') ) + #Change color to orange & Blue
  my_theme #Set Common Theme

#--- Regression on Big 4 vs Non-Big 4 Auditors---#
big4_names = c("KPMG LLP", "PricewaterhouseCoopers LLP", 
                "Ernst & Young LLP", "Deloitte & Touche LLP")
  
#Separate Big 4 Vs Other Auditors
base_big4 = base_clean %>% filter(auditor_name%in%big4_names)
base_non_big4 = base_clean %>% filter(!auditor_name%in%big4_names)

#Regression on Big 4 Auditors
base_reg_big4 = lm(formula = audit_fees ~ treatment + non_audit_fees+ at + 
                     mve_crsp + ib + dltt, data=base_big4)
summary(base_reg_big4) #Display Regression Results

#Regression on Non-Big 4 Auditors
base_reg_non_big4 = lm(formula = audit_fees ~ treatment + non_audit_fees+ at + 
                         mve_crsp + ib + dltt, data=base_non_big4)
summary(base_reg_non_big4) #Display Regression Results

#Visualization of Audit Fee Distribution between Big 4 & Non-Big 4
base_clean %>% 
  #Temporary Column to group data by Big4/Non-Big 4
  mutate(firm_class = ifelse(auditor_name%in%big4_names, "big4", "non_big4")) %>% 
  ggplot(aes(x = audit_fees, 
             fill = firm_class)) +
  geom_density(alpha = 0.4) +
  labs(title = "Audit Fee Distribution by Big4/Non_Big4") + #Title of Plot
  scale_fill_manual(values = c('Red', 'Black') ) + #Change Color to Red & Black
  my_theme #Set Common Theme

#--- Regression on Financial Industry ---#
base_financial = base_clean %>% filter(ffi12_desc=="Money")
base_reg_financial = lm(formula = audit_fees ~ treatment + non_audit_fees+ at + 
                          mve_crsp + ib + dltt, data=base_financial)
summary(base_reg_financial) 

#Visualization of Audit Fee Distribution between Finance & Other Industries
base_clean %>% 
  #Temp Column to group data by Finance/Non-Finance
  mutate(industry = ifelse(ffi12_desc=="Money", "Finance", "Non-Finance")) %>% 
  ggplot(aes(x = audit_fees, 
             fill = industry)) +
  geom_density(alpha = 0.4) +
  labs(title = "Audit Fee Distribution by Finance/Non-Finance") + #Title
  scale_fill_manual(values = c('Yellow', 'Blue') ) + #Change color to yellow + blue
  my_theme #Set Common Theme


base_clean %>% 
  ggplot(aes(x = audit_fees, 
             fill = ffi12_desc)) +
  geom_density(alpha = 0.4) +
  labs(title = "Audit Fee Distribution by Industry") + #Title
  my_theme #Set Common Theme
