###### START OF LIFE GLOBAL FILES


library(tidyverse)
library(lubridate)
library(DBI)
library(RPostgreSQL)




policy <- readRDS("policy_sample.RDS")
life_data_sum_table <- readRDS("policy_summary.rds")

# policy[policy$company_name == "Cologne ",]$company_name <- "Cologne"
# life_data_sum_table[life_data_sum_table$company_name == "Cologne ",]$company_name <- "Cologne"
# 
# saveRDS(policy, "policy_sample.RDS")
# saveRDS(life_data_sum_table, "policy_summary.rds")

set.seed(123)
x <- 1001:1100
dim(policy)
pr <- seq(0.01,1, 0.01)
id <- sample(x, size =nrow(policy), replace = T,prob = pr )
policy$agentid <- id

#Converting into Date Format of R
policy <-  policy %>% 
    mutate(
        dateofbirth = as.Date(dateofbirth, format = "%m/%d/%Y"),
        policystartdate = as.Date(policystartdate, format = "%m/%d/%Y"),
        policyenddate = as.Date(policyenddate, format = "%m/%d/%Y"),
        riskstartdate = as.Date(riskstartdate, format = "%m/%d/%Y"),
        year = year(policystartdate),
        month = month(policystartdate, label =T)) %>% 
    filter(year <=2022)

linechart_policy <-  policy %>%
    group_by(policystartdate, company_name) %>%
    summarise(total = n()) %>%
    ungroup()
# saveRDS(linechart_policy, "linechart_policy.RDS")

monthlychart_data <- policy %>% 
    group_by(company_name, year, month) %>% 
    summarise(total = n()) %>%
    ungroup()


life_compare_policy_data <- policy %>% 
    group_by(policystartdate, month, year, company_name) %>%
    summarise(total = n())%>%
    ungroup()





#Policyholder's age at policy starting computation For Histogram

policy <- policy %>%
    mutate( ages = floor (time_length(difftime(policystartdate, dateofbirth), "years")),
            ages = ifelse(ages>=0|ages<=100,ages, NA),
            age_group = cut(ages, breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), right = FALSE))


age_group_plotly <- data.frame(table(policy$company_name, policy$age_group))
age_group_plotly <- rename(age_group_plotly, 
                           company_name = Var1,
                           Age_Group = Var2,
                           Frequecy = Freq)

# saveRDS(age_group_plotly, "age_group_plotly.RDS")


#DataTable for Company Version
agent_policy_dt <-  policy %>% filter(!is.na(agentid), agentid != "N/A") %>%
    group_by(company_name, agentid) %>%
    summarise(Total_Policy_Issued  = n()) %>%
    ungroup()

# saveRDS(agent_policy_dt, "agent_policy_dt.RDS")

policy_status <- policy %>% 
    group_by(company_name) %>% summarise(
        
        death =  sum(str_detect(status, regex("death", ignore_case = T)), na.rm = T),
        surrender = sum(str_detect(status, regex("^s", ignore_case = T)), na.rm = T),
        matured = sum((((tolower(status) != "death" & tolower(status) != "surrender" ) | is.na(status)) &
                           policyenddate <= today()), na.rm = T))  %>%
    ungroup()



policy_status <- left_join(policy_status, life_data_sum_table, by = c("company_name" = "company_name"))


#sms_length = nchar(trunc(policy$netpremium))


#Recoding 'premiummode'
policy <- policy %>% 
    mutate(premiummode = ifelse(grepl("^M", premiummode , ignore.case = TRUE),
                                "Mly",
                                ifelse(grepl("03|3|04|4|Q|QLY|QUARTERLY", premiummode, ignore.case = TRUE),
                                       "Qly",
                                       ifelse(grepl("02|2|HALF|HLY|HMLY|A HALF-YEARLY", premiummode, ignore.case = TRUE),
                                              "Hly",
                                              ifelse(grepl("01|1|1YL|YLY|YEARLY", premiummode, ignore.case = TRUE),
                                                     "Yly",
                                                     ifelse(grepl("^S", premiummode, ignore.case = TRUE),
                                                            "Sin",
                                                            NA))))))



#table(policy$gender)
#table(policy$gender)
policy <- policy %>% 
    mutate (gender = ifelse (grepl("1|01|^M|^MALE", gender, ignore.case = TRUE),
                             "Male",
                             ifelse(grepl("02|0|2|^F|FEMALE|FE-MALE|FEMAL|FE-MALE", gender, ignore.case = TRUE),
                                    "Female",
                                    ifelse(grepl("O|OTHER|OTHERS", gender, ignore.case = TRUE),
                                           "Other",
                                           NA))))

##  data cleaning for map which will run all the codes written in map.R
#source("map.R")   #If we want the heatmap on the BI then we have to add district and address on the dataset while extracting from the database

###Since we don't need the 'address' column anymore, we can drop that from the dataset. It saves significant space
#policy <- subset(policy, select= -address)


life_company_picker <<- str_sort(unique(policy$company_name))

life_select_options <<- c("All", str_sort(unique(policy$company_name)))

life_policy_year_choices <<- sort(unique(policy$year))

policy_by_term_sum <- policy %>% group_by(company_name, term) %>% count()  %>%
    ungroup()


genderdata_sum <- policy %>%
    group_by(policystartdate, company_name, gender) %>% summarize(counts = n())  %>%
    ungroup()


nomatchpolicy_sum <- policy %>% mutate (no_match = policystartdate != riskstartdate) %>%
    filter (no_match) %>% group_by(company_name) %>% summarize(counts = n())  %>%
    ungroup()   #, percentage = n()/nrow(policy)



or <- readRDS("or_sample.RDS")

##agent id
set.seed(123)
x <- 1001:1100
dim(or)
pr <- seq(0.01,1, 0.01)
y <- sample(x, size =nrow(or), replace = T,prob = pr )
or$agentid <- y

#Selecting between 2019-2022
or <- or %>% 
    mutate(ordate = as.Date(ordate, format = "%Y-%m-%d"),
           year_r = year(ordate),
           month_r = month(ordate, label =T)) %>% 
    filter(year_r >= 2019 & year_r <= 2022)



#recoding 'ortype'
or <- or %>% 
    mutate (ortype = ifelse (grepl("^F|1|01", ortype, ignore.case = TRUE),
                             "F",
                             ifelse(grepl("^D", ortype, ignore.case = TRUE),
                                    "D",
                                    ifelse(grepl("^R", ortype, ignore.case = TRUE),
                                           "R",
                                           NA))))


#Premium Summary
linechart_p_or <-  or %>%
    group_by(ordate, company_name) %>%
    summarise(total = round(sum(totalpayableamount, na.rm = T), 0))  %>%
    ungroup()

# saveRDS(linechart_p_or, "linechart_p_or.RDS")


monthlychart_p_or <- or %>% 
    group_by(company_name, year_r, month_r) %>% 
    summarise(total = round(sum(totalpayableamount, na.rm = T), 0))  %>%
    ungroup()

# saveRDS(monthlychart_p_or, "monthlychart_p_or.RDS")

#E-Receipt
linechart_r_data <-  or %>%
    group_by(ordate, company_name) %>%
    summarise(total = n())  %>%
    ungroup()
# saveRDS(linechart_r_data, "linechart_r_data.RDS")

monthlychart_r <- or %>% 
    group_by(company_name, year_r, month_r) %>% 
    summarise(total = n())  %>%
    ungroup()
# saveRDS(monthlychart_r, "monthlychart_r.RDS")


life_compare_or_data <- or %>% 
    group_by(ordate, month_r, year_r, company_name) %>% 
    summarise(total = round(sum(totalpayableamount, na.rm = T), 0))  %>%
    ungroup()



## total premium collection by OR Type

premium_by_ortype_sum <- or %>% group_by(ordate, company_name, ortype) %>% 
    summarise(total = round(sum(totalpayableamount, na.rm = T), 0))  %>%
    ungroup()

premium_by_ortype_count <- or %>% 
    group_by(ordate, company_name, ortype) %>% summarise(total = n()) %>%
    ungroup()



#DataTable

branch_dt <- or %>%
    group_by(company_name, officebranchcode) %>%
    summarise(Total_E_Receipt  = n(), Collection = round(sum(totalpayableamount, na.rm = T), 0)) %>%
    ungroup()
# saveRDS(branch_dt, "branch_dt.RDS")

agent_dt <-  or %>% filter(!is.na(agentid), agentid != "N/A") %>%
    group_by(company_name, agentid) %>%
    summarise(Total_E_Receipt  = n(), Collection = round(sum(totalpayableamount, na.rm = T), 0)) %>%
    ungroup()
# saveRDS(agent_dt, "agent_dt.RDS")

life_or_year_choices <- sort(unique(or$year_r), T)


save(agent_dt, branch_dt,genderdata_sum,nomatchpolicy_sum,life_compare_policy_data,life_compare_or_data, policy_by_term_sum, premium_by_ortype_sum, premium_by_ortype_count, monthlychart_r, linechart_r_data, 
     monthlychart_p_or, linechart_p_or, policy_status, agent_policy_dt, age_group_plotly, 
     monthlychart_data, linechart_policy, life_data_sum_table,life_select_options,life_policy_year_choices,life_or_year_choices,  life_company_picker,
     file = "data/life_summarised_data.RData")

# rm(list = ls())  # clear the environment
#ENF OF LIFE GLOBAL




########START OF NONLIFE GLOBAL CODES######


# coverpolicy <- readRDS("coverpolicy.RDS")
# 
# ss <- round(nrow(coverpolicy)*0.4)
# coverpolicy_sample <- sample_n(coverpolicy, size = ss) 
# saveRDS(coverpolicy_sample, "coverpolicy_sample.RDS")

coverpolicy <- readRDS("coverpolicy_sample.RDS")

# set.seed(123)
# x <- 1001:1100
# dim(coverpolicy)
# pr <- seq(0.01,1, 0.01)
# id <- sample(x, size =nrow(coverpolicy), replace = T,prob = pr )
# coverpolicy$agentid <- id

# coverpolicy[coverpolicy$company_name == "Cologne ",]$company_name <- "Cologne"

# saveRDS(coverpolicy, "coverpolicy_sample.RDS")

# unique(coverpolicy$covernotenumber) %>% length()
# 
# coverpolicy[!is.na(coverpolicy$covernotenumber),] %>% nrow()


# for (i in 1:nrow(coverpolicy)) {
#     if(!is.na(coverpolicy$covernotenumber[i])){
#         coverpolicy$covernotenumber[i] <- paste0(paste0(sample(LETTERS, 4), collapse = ""),"-", coverpolicy$covernoteissuedate[i], collapse = "")
#     }
# }

# saveRDS(coverpolicy, "coverpolicy_sample.RDS")

# length(unique(coverpolicy$covernotenumber))

# for (i in 1:nrow(coverpolicy)) {
#     if(!is.na(coverpolicy$policynumber[i])){
#         coverpolicy$policynumber[i] <- paste0(paste0(sample(LETTERS, 4), collapse = ""),"-", coverpolicy$policyissuedate[i], collapse = "")
#     }
# }
# 
# coverpolicy %>% select(-agentid)  -> coverpolicy
# 
# for (i in 1:nrow(coverpolicy)) {
#     if(!is.na(coverpolicy$email[i])){
#         coverpolicy$email[i] <- paste0(paste0(sample(letters, 7), collapse = ""),"@", "email.com", collapse = "")
#     }
# }
# 
# for (i in 1:nrow(coverpolicy)) {
#     if(!is.na(coverpolicy$mobilenumber[i])){
#         coverpolicy$mobilenumber[i] <- paste0("0",paste0(sample(1:9, 9, replace = T), collapse = ""), collapse = "")
#     }
# }
# 

# saveRDS(coverpolicy, "coverpolicy_sample.RDS")




coverpolicy$policyissuedate <- as.Date(coverpolicy$policyissuedate, format = "%Y-%m-%d")
coverpolicy$covernoteissuedate <- as.Date(coverpolicy$covernoteissuedate, format = "%Y-%m-%d")
coverpolicy$coverageenddate <- as.Date(coverpolicy$coverageenddate, format = "%Y-%m-%d")

coverpolicy$year_p<- year(coverpolicy$policyissuedate) 
coverpolicy$month_p<- month(coverpolicy$policyissuedate, label = T)

coverpolicy$year_c<- year(coverpolicy$covernoteissuedate) 
coverpolicy$month_c<- month(coverpolicy$covernoteissuedate, label = T)

coverpolicy <- coverpolicy %>%   ## Need to make change in that. This variable is now well organised in database
    mutate (insurancetype = ifelse (grepl("motor", insurancetype, ignore.case = TRUE),
                                    "Motor",
                                    ifelse(grepl("fire", insurancetype, ignore.case = TRUE),
                                           "Fire",
                                           ifelse(grepl("hull", insurancetype, ignore.case = TRUE),
                                                  "Marine Hull",
                                                  ifelse(grepl("cargo", insurancetype, ignore.case = TRUE),
                                                         "Marine Cargo",
                                                         ifelse(grepl("misc", insurancetype, ignore.case = TRUE),
                                                                "Miscellanious",
                                                                ifelse(grepl("engin", insurancetype, ignore.case = TRUE),
                                                                       "Engineering",
                                                                       ifelse(grepl("Avi", insurancetype, ignore.case = TRUE),
                                                                              "Aviation",
                                                                              NA))))))))



mr = readRDS("mr_sample.RDS")
# ss <- round(nrow(mr)*0.4)
# mr_sample <- sample_n(mr, size = ss) 
# 
# saveRDS(mr_sample, "mr_sample.RDS")

mr$mrdate <- as.Date(mr$mrdate, format = "%Y-%m-%d")

mr$year_r <- year(mr$mrdate) 
mr$month_r <- month(mr$mrdate, label = T)


#table(mr$iscoinsurance)

mr <- mr %>% 
    mutate (iscoinsurance = ifelse (grepl("0|N|NO", iscoinsurance, ignore.case = TRUE),
                                    "No",
                                    ifelse(grepl("1|Y|YES", iscoinsurance, ignore.case = TRUE),
                                           "Yes",
                                           NA)))



# ps <- mr[!is.na(mr$policynumber),]$policynumber %>% length()
# 
# # coverpolicy$policynumber %>% unique() %>% length()
# # sample(unique(coverpolicy$policynumber), ps) %>% length()
# mr[!is.na(mr$policynumber),]$policynumber <- sample(unique(coverpolicy$policynumber), ps)
# 
# cs <- mr[!is.na(mr$covernotenumber),]$covernotenumber %>% length()
# 
# # coverpolicy$policynumber %>% unique() %>% length()
# # sample(unique(coverpolicy$policynumber), ps) %>% length()
# mr[!is.na(mr$covernotenumber),]$covernotenumber <- sample(unique(coverpolicy$covernotenumber), cs)
# 


# mr %>% select(-c(officebranchcode, officebranchname)) -> mr
# saveRDS(mr, "mr_sample.RDS")


# ns <- mr %>% nrow()
# 
# mr$coverageenddate <- sample(coverpolicy$coverageenddate, ns)
# 

#getting non life summarised table data from database dircetly (Active, Total policy)


non_life_data_sum_table <- data.frame(companyname = character(), activepolicy = numeric(), totalpolicy = numeric())

current_date <- today() - 1

non_life_data_sum_table <- mr %>% group_by(company_name) %>% 
    summarise(
        activepolicy = sum(coverageenddate > current_date),
        totalpolicy = n()
    )


# non_life_data_sum_table
saveRDS(non_life_data_sum_table, "data/non_life_data_sum_table.RDS")

# 
# 
# 
# RPostgreSQL::postgresqlCloseConnection(con)   #To disconnect from the Database


#################### summarising ######################

coverpolicy_note <<- coverpolicy %>% 
    filter(year_c >= 2019 & year_c <= 2022)

coverpolicy_policy <<- coverpolicy %>% 
    filter(year_p >= 2019 & year_p <= 2022)

mr <<- mr %>% 
    filter(year_r >= 2019 & year_r <= 2022)  #Selecting data only between 2019 and 2022

companies <- unique(coverpolicy$company_name)


################# calculations with covernote, covernoteissuedate

# daywise , monthwise, yearwise covernote issue linechart

# daywisetotal_c_count <- coverpolicy_note %>%
#     group_by(covernoteissuedate, company_name) %>% 
#     summarise(total = n()) %>%
#     ungroup()
# 
# monthwisetotal_c_count <- coverpolicy_note %>%
#     group_by(month_c, year_c, company_name) %>%
#     summarise(total = n()) %>%
#     ungroup()
# 
# agentwisedt_note <- coverpolicy_note %>% 
#     group_by(company_name, agentid) %>%               #should also group by covernoteissuedate
#     filter(!is.na(agentid), agentid != "N/A") %>% 
#     summarise(Total_Covernote = n()) %>%
#     ungroup()
# 
# 
# totalcancelled_cover_sum <- coverpolicy_note %>% 
#     filter (!is.na(coverpolicy_note$cancelledcover)) %>%
#     group_by(covernoteissuedate, company_name) %>%
#     summarise(total = n()) %>%
#     ungroup()



##########################




######### calculation with policy, policyissuedate

daywisetotal_p_count <- coverpolicy_policy %>%
    group_by(policyissuedate, company_name) %>% 
    summarise(total = n()) %>%
    ungroup()

monthwisetotal_p_count <- coverpolicy_policy %>%
    group_by(month_p, year_p, company_name) %>% 
    summarise(total = n()) %>%
    ungroup()


insurancetype_sum <- coverpolicy %>% 
    filter(insurancetype !="") %>%
    group_by(covernoteissuedate, company_name, insurancetype) %>% 
    summarize(counts = n()) %>%
    ungroup()

insurancetype_sum_bar <- coverpolicy_policy %>% 
    filter(insurancetype !="") %>%
    group_by(company_name, year_p, insurancetype) %>% 
    summarize(counts = n()) %>%
    ungroup()


total_policy_count_all_sum <- coverpolicy_policy %>%
    group_by(company_name) %>%
    summarise(total = n()) %>%
    ungroup()


totalexpired_sum <- coverpolicy_policy %>% 
    filter (coverageenddate < today()) %>% 
    group_by(policyissuedate, company_name) %>%
    summarise(total = n()) %>%
    ungroup()



totalcancelled_sum <- coverpolicy_policy %>% 
    filter (!is.na(coverpolicy_policy$cancelledcover) | !is.na(coverpolicy_policy$cancelledpolicy)) %>%
    group_by(policyissuedate, company_name) %>%
    summarise(total = n()) %>%
    ungroup()




agentwisedt_policy <- coverpolicy_policy %>%
    group_by(company_name, agentid) %>%               ##should also group by policyissuedate
    filter(!is.na(agentid), agentid != "N/A") %>%
    summarise(Total_Policy = n()) %>%
    ungroup()
############################



##premium collection

daywisetotalprem <- mr %>%
    group_by(mrdate, company_name) %>% 
    summarise(Total_Transaction= n(), total = round(sum(netpremium, na.rm = T), 0)) %>%
    ungroup()

monthwisetotalprem <- mr %>%
    group_by(company_name,month_r, year_r) %>%
    summarise(Total_Transaction= n(), total = round(sum(netpremium, na.rm = T), 0)) %>%
    ungroup()

# branchwisetotalprem <- mr %>%
#     group_by(mrdate, company_name, officebranchcode) %>%
#     summarise(Total_Transaction= n(), total = round(sum(netpremium, na.rm = T), 0)) %>%
#     ungroup()


# agentwisetotalprem <- mr %>%
#   group_by(mrdate, company_name, agentid) %>%
#   summarise(Total_E_Receipt= n(), total = sum(netpremium, na.rm = T))

totalbankdep_sum <- mr %>% 
    mutate (modeofpayment_updated = ifelse (grepl("^P|ORDER|^ON|^I|BANK|^D|^TRANS|DRAFT|^CH|CHQ|CEQ|CHA|CREDIT|D.D", modeofpayment, ignore.case = TRUE),
                                            "Bank Deposited",
                                            "Others")) %>%
    group_by(company_name, mrdate, modeofpayment_updated)%>%
    summarise(total = n()) %>%
    ungroup()





smsdata_sum <- mr %>% filter(smsstatus !="") %>% 
    group_by(mrdate, company_name, smsstatus) %>% summarize(counts = n()) %>%
    ungroup()

coinsurancedata_sum <- mr %>% filter(iscoinsurance !="") %>% 
    group_by(mrdate, company_name, iscoinsurance) %>% summarize(counts = n()) %>%
    ungroup()




##total valid mobile
mobile <- coverpolicy  %>% pull(mobilenumber)
valid_mobile <- mobile
# valid_mobile <- str_match(mobile, regex("01[3-9][0-9]{2}[-]?[0-9]{6}"))
totalvalidmobile_sum_all <- length(valid_mobile[!is.na(valid_mobile)])
validmobile_percent_sum_all <- paste0(round(totalvalidmobile_sum_all / length(mobile) * 100 , 2),"%")


##total valid mobile by company
totalvalidmobile_sum <- data.frame(company_name = character(), totalvalidmobile = numeric(), validmobile_percent = character())

for (i in companies){
    mobile <- coverpolicy %>% filter(company_name == i) %>% pull(mobilenumber)
    valid_mobile <- mobile
    # valid_mobile <- str_match(mobile, regex("01[3-9][0-9]{8}"))
    totalvalidmobile <- length(valid_mobile[!is.na(valid_mobile)])
    validmobile_percent <- paste0(round(totalvalidmobile / length(mobile) * 100 , 2),"%")
    totalvalidmobile_sum[nrow(totalvalidmobile_sum) + 1, ] <- list(i, totalvalidmobile, validmobile_percent)
}

##total valid email
email <- coverpolicy %>% pull(email)
valid_email <- str_match(email, regex("\\S+@\\S+\\.\\S+"))
totalvalidemail_sum_all <- length(valid_email[!is.na(valid_email)])
validemail_percent_sum_all <- paste0(round(totalvalidemail_sum_all / length(email) * 100 , 2),"%")

valid_email_address_sum <- data.frame(company_name = character(), totalvalidemail = numeric(), validemail_percent = character())

for (i in companies){
    email <- coverpolicy %>% filter(company_name == i) %>% pull(email)
    valid_email <- str_match(email, regex("\\S+@\\S+\\.\\S+"))
    totalvalidemail <- length(valid_email[!is.na(valid_email)])
    validemail_percent <- paste0(round(totalvalidemail / length(email) * 100 , 2),"%")
    valid_email_address_sum[nrow(valid_email_address_sum) + 1, ] <- list(i, totalvalidemail, validemail_percent)
}



nonlife_select_options <<- c("All", str_sort(unique(coverpolicy$company_name)))

nonlife_company_picker <<- unique(mr$company_name)

nrow_mr <- nrow(mr)
year_choices <- unique(coverpolicy_policy$year_p)


e_receipt_daily <-  mr %>%
    group_by(mrdate, company_name) %>%
    summarise(total = n()) %>%
    ungroup()

e_receipt_monthly <-  mr %>%
    group_by(company_name, year_r, month_r) %>%
    summarise(total = n()) %>%
    ungroup()


# branch_dt <- mr %>%
#   group_by(company_name, officebranchcode) %>%
#   summarise(Total_E_Receipt  = n(), Collection = sum(netpremium, na.rm = T))



#no policyissudate present but policynumber present

policynumber_vs_issuedate <- coverpolicy %>% filter(is.na(policyissuedate)) %>% filter(!is.na(policynumber)) %>%
    group_by(company_name) %>% summarise(total = n()) %>%
    ungroup()


## covernoteissudate present but policy issuedate absent

covernote_present_policy_absent <- coverpolicy %>% filter(!is.na(covernoteissuedate)) %>% filter(is.na(policyissuedate)) %>%
    group_by(company_name) %>% summarise(total = n()) %>%
    ungroup()


## covernoteissuedate and policyissuedate both present

covernote_policy_both_present <- coverpolicy %>% filter(!is.na(covernoteissuedate)) %>% filter(!is.na(policyissuedate)) %>%
    group_by(company_name) %>% summarise(total = n()) %>%
    ungroup()



save(covernote_present_policy_absent,covernote_policy_both_present, policynumber_vs_issuedate, e_receipt_daily, e_receipt_monthly, totalvalidemail_sum_all, validemail_percent_sum_all, valid_email_address_sum, 
     validmobile_percent_sum_all, totalvalidmobile_sum_all, totalvalidmobile_sum, nonlife_company_picker, nonlife_select_options,nrow_mr, year_choices,
     totalcancelled_sum,totalexpired_sum, total_policy_count_all_sum,coinsurancedata_sum,
     smsdata_sum,insurancetype_sum, insurancetype_sum_bar, totalbankdep_sum, monthwisetotalprem, daywisetotalprem, daywisetotal_p_count, 
     monthwisetotal_p_count,agentwisedt_policy, file = "data/nonlife_summarised_data.RData")






#######################################################
#######################################################
#######################################################
########### LIFE FORECAST   ###########################
#######################################################
#######################################################

# #library(tidyverse)
# library(tidymodels)
# library(modeltime)
# # remotes::install_github("business-science/timetk")
# library(timetk)
# # install.packages("modeltime.ensemble")
# library(modeltime.ensemble)
# #library(lubridate)
# 
# 
# monthly_7_model_ensemble_function <- function(df) {
#     
#     
#     recipe1 <- recipe(cnt~., df) %>%
#         step_timeseries_signature(date) %>%
#         step_rm(matches("(.iso$)|(.xts$)|(day)|(hour)|(minute)|(second)|(am.pm)"))%>%
#         step_normalize(date_index.num, date_year) %>%
#         step_mutate(date_week = factor(date_week, ordered = T)) %>%
#         step_dummy(all_nominal(), one_hot = T)
#     
#     # recipe1 %>% prep() %>% juice() %>% glimpse()
#     
#     
#     recipe2 <- recipe1 %>%
#         update_role(date, new_role = "ID")
#     
#     
#     # recipe2 %>% prep() %>% summary()
#     
#     fit_prophet <- workflow() %>%
#         add_model(
#             prophet_reg() %>% set_engine("prophet")
#         ) %>%
#         add_recipe(recipe1) %>%
#         fit(df)
#     
#     fit_xgboost <- workflow() %>%
#         add_model(
#             boost_tree() %>% set_engine("xgboost")
#         ) %>%
#         add_recipe(recipe2) %>%
#         fit(df)
#     
#     
#     
#     
#     
#     fit_RF <- workflow() %>%
#         add_model(
#             rand_forest() %>% set_engine("ranger")
#         ) %>%
#         add_recipe(recipe2) %>%
#         fit(df)
#     
#     
#     fit_svm <- workflow() %>%
#         add_model(
#             svm_rbf() %>% set_engine("kernlab")
#         ) %>%
#         add_recipe(recipe2) %>%
#         fit(df)
#     
#     
#     fit_prophet_boost <- workflow() %>%
#         add_model(
#             prophet_boost(
#                 seasonality_daily = F,seasonality_weekly = F, seasonality_yearly = F,
#             ) %>% set_engine("prophet_xgboost")
#         ) %>%
#         add_recipe(recipe1) %>%
#         fit(df)
#     
#     
#     
#     fit_ets <- workflow() %>%
#         add_model(
#             exp_smoothing() %>% set_engine(engine = "ets")
#         ) %>%
#         add_recipe(recipe1) %>%
#         fit(df)
#     
#     
#     fit_mars <- workflow() %>%
#         add_model(
#             mars(mode = "regression") %>% set_engine("earth")
#         ) %>%
#         add_recipe(recipe2) %>%
#         fit(df)
#     
#     # fit_lm <- workflow() %>%
#     #     add_model(
#     #         linear_reg() %>% set_engine("lm")
#     #     ) %>%
#     #     add_recipe(recipe1) %>%
#     #     fit(df)
#     # 
#     # fit_arima_no_boost <- arima_reg() %>%
#     #     set_engine(engine = "auto_arima") %>%
#     #     fit(cnt ~ date, data = df)
#     
#     
#     fit_table <- modeltime_table(
#         fit_prophet,
#         fit_xgboost,
#         fit_RF,
#         fit_svm,
#         fit_prophet_boost,
#         fit_ets,
#         fit_mars
#     )
#     
#     # fit_calibrated <- fit_table %>% 
#     #   modeltime_calibrate(testing(splits))
#     
#     # fit_calibrated %>% modeltime_accuracy()
#     
#     
#     # fit_calibrated %>%
#     #     modeltime_forecast(
#     #         new_data = testing(splits), actual_data = df, keep_data = T
#     #     ) %>%
#     #     plot_modeltime_forecast()
#     
#     
#     
#     ensemble_fit <- fit_table %>%
#         ensemble_average(type = "median")
#     
#     ensemble_fit_table <- ensemble_fit %>%
#         modeltime_table()
#     
#     
#     # ensemble_fit_table %>%
#     # combine_modeltime_tables(fit_table) %>%
#     #     modeltime_accuracy(testing(splits))
#     
#     
#     
#     ensemble_refit_for_future <- ensemble_fit_table %>%
#         modeltime_refit(df)
#     
#     
#     
# }
# 
# 
# companies <- unique(linechart_policy$company_name)
# 
# 
# life_policy_forecastObjects <-list()
# life_policy_forecast_data <- list()
# 
# 
# ## monthly policy data per company and forecast objects
# for (i in companies ){
#     print(i)
#     
#     monthly_policy <- linechart_policy %>%
#         filter(company_name == i, policystartdate <= today(), policystartdate > "2005-01-01") %>%
#         # group_by(policystartdate) %>% 
#         # summarise(n = sum(total, na.rm = T)) %>%
#         mutate(month = month(policystartdate, T),
#                year= year(policystartdate)) %>%
#         group_by(year, month) %>%
#         mutate(n = sum(total),
#                date = max(policystartdate)) %>%
#         ungroup() %>%
#         select(date, cnt = n) %>%
#         distinct() %>% 
#         ungroup()
#     
#     life_policy_forecast_data[[i]] <- monthly_policy
#     
#     forecast_object <- monthly_7_model_ensemble_function(monthly_policy)
#     
#     life_policy_forecastObjects[[i]] <- forecast_object
#     
# }
# 
# 
# 
# 
# ### life admin forecast 
# 
# life_admin_policy_forecast_data <- linechart_policy %>%
#     filter(policystartdate <= today(), policystartdate > "2005-01-01") %>%
#     # group_by(policystartdate) %>% 
#     # summarise(n = sum(total, na.rm = T)) %>%
#     mutate(month = month(policystartdate, T),
#            year= year(policystartdate)) %>%
#     group_by(year, month) %>%
#     mutate(n = sum(total),
#            date = max(policystartdate)) %>%
#     ungroup() %>%
#     select(date, cnt = n) %>%
#     distinct() %>% 
#     ungroup()
# 
# 
# life_admin_policy_forecastObjects <- monthly_7_model_ensemble_function(life_admin_policy_forecast_data)
# 
# 
# 
# 
# 
# 
# 
# save(life_policy_forecastObjects, life_policy_forecast_data,life_admin_policy_forecastObjects,life_admin_policy_forecast_data, file = "../data/life_policy_forecast.RData")
# 
# 
# ## weekly premium collection per company and forecast objects
# 
# life_prem_forecast_object <-list()
# life_prem_forecast_data <- list()
# 
# for (i in companies ){
#     print(i)
#     
#     weekly_premium <- linechart_p_or %>%
#         filter(company_name == i, ordate <= today()) %>%
#         # group_by(ordate) %>% 
#         # summarise(n = sum(total, na.rm = T)) %>%
#         mutate(month = month(ordate, T),
#                year= year(ordate),
#                week = week(ordate))%>%
#         group_by(year, month, week) %>%
#         mutate(n = sum(total),
#                date = max(ordate)) %>%
#         ungroup() %>%
#         select(date, cnt = n) %>%
#         distinct() %>% 
#         ungroup()
#     
#     life_prem_forecast_data[[i]] <- weekly_premium
#     
#     forecast_object <- monthly_7_model_ensemble_function(weekly_premium)
#     
#     life_prem_forecast_object[[i]] <- forecast_object
#     
# }
# 
# 
# 
# 
# life_admin_prem_forecast_data <- linechart_p_or %>%
#     filter(ordate <= today()) %>%
#     # group_by(ordate) %>% 
#     # summarise(n = sum(total, na.rm = T)) %>%
#     mutate(month = month(ordate, T),
#            year= year(ordate),
#            week = week(ordate))%>%
#     group_by(year, month, week) %>%
#     mutate(n = sum(total),
#            date = max(ordate)) %>%
#     ungroup() %>%
#     select(date, cnt = n) %>%
#     distinct() %>% 
#     ungroup()
# 
# 
# 
# life_admin_prem_forecast_object <- monthly_7_model_ensemble_function(life_admin_prem_forecast_data)
# 
# 
# 
# 
# 
# save(life_prem_forecast_object, life_prem_forecast_data, life_admin_prem_forecast_data, life_admin_prem_forecast_object,  file = "../data/life_prem_forecast.RData")
# 
# # 
# # 
# # weekly_premium<- linechart_p_or %>%
# #     filter(company_name == "Metlife") %>%
# #     # group_by(ordate) %>%
# #     # summarise(n = sum(total, na.rm = T)) %>%
# #     mutate(month = month(ordate, T),
# #            year= year(ordate),
# #            week = week(ordate)) %>%
# #     group_by(year, month, week) %>%
# #     mutate(n = sum(total),
# #            date = max(ordate)) %>%
# #     ungroup() %>%
# #     select(date, cnt = n) %>%
# #     distinct() %>%
# #     ungroup()
# 
# 
# 
# ######################################
# ### nonlife monthly premium collection 
# 
# nonlife_companies <- unique(daywisetotalprem$company_name)
# 
# 
# nonlife_prem_forecast_object <-list()
# nonlife_prem_forecast_data <- list()
# 
# 
# 
# 
# for (i in nonlife_companies ){
#     print(i)
#     
#     monthly_premium  <- daywisetotalprem %>%
#         filter(company_name == i, mrdate <= today()) %>%
#         mutate(month = month(mrdate, T),
#                year= year(mrdate))%>%
#         group_by(year, month) %>%
#         mutate(n = sum(total),
#                date = max(mrdate)) %>%
#         ungroup() %>%
#         select(date, cnt = n) %>%
#         distinct() %>% 
#         ungroup()
#     
#     nonlife_prem_forecast_data[[i]] <- monthly_premium
#     
#     forecast_object <- monthly_7_model_ensemble_function(monthly_premium)
#     
#     nonlife_prem_forecast_object[[i]] <- forecast_object
#     
# }
# 
# 
# nonlife_admin_prem_forecast_data  <- daywisetotalprem %>%
#     filter(mrdate <= today()) %>%
#     mutate(month = month(mrdate, T),
#            year= year(mrdate))%>%
#     group_by(year, month) %>%
#     mutate(n = sum(total),
#            date = max(mrdate)) %>%
#     ungroup() %>%
#     select(date, cnt = n) %>%
#     distinct() %>% 
#     ungroup()
# 
# 
# nonlife_admin_prem_forecast_object <- monthly_7_model_ensemble_function(nonlife_admin_prem_forecast_data)
# 
# 
# 
# 
# save(nonlife_prem_forecast_object, nonlife_prem_forecast_data,nonlife_admin_prem_forecast_data,nonlife_admin_prem_forecast_object, file = "../data/nonlife_prem_forecast.RData")
# 
# 
# 
# ## nonlife policy
# 
# 
# nonlife_policy_forecast_object <-list()
# nonlife_policy_forecast_data <- list()
# 
# 
# 
# 
# for (i in nonlife_companies ){
#     print(i)
#     
#     
#     nonlife_monthly_policy <- daywisetotal_p_count %>%
#         filter(company_name == i, policyissuedate <= today()) %>%
#         mutate(month = month(policyissuedate, T),
#                year= year(policyissuedate)) %>%
#         group_by(year, month) %>%
#         mutate(n = sum(total),
#                date = max(policyissuedate)) %>%
#         ungroup() %>%
#         select(date, cnt = n) %>%
#         distinct() %>% 
#         ungroup()
#     
#     
#     nonlife_policy_forecast_data[[i]] <- nonlife_monthly_policy
#     
#     forecast_object <- monthly_7_model_ensemble_function(nonlife_monthly_policy)
#     
#     nonlife_policy_forecast_object[[i]] <- forecast_object
#     
# }
# 
# 
# nonlife_admin_policy_forecast_data <- daywisetotal_p_count %>%
#     filter(policyissuedate <= today()) %>%
#     mutate(month = month(policyissuedate, T),
#            year= year(policyissuedate)) %>%
#     group_by(year, month) %>%
#     mutate(n = sum(total),
#            date = max(policyissuedate)) %>%
#     ungroup() %>%
#     select(date, cnt = n) %>%
#     distinct() %>% 
#     ungroup()
# 
# 
# 
# nonlife_admin_policy_forecast_object <- monthly_7_model_ensemble_function(nonlife_admin_policy_forecast_data)
# 
# 
# 
# 
# save(nonlife_policy_forecast_object, nonlife_policy_forecast_data,nonlife_admin_policy_forecast_data,nonlife_admin_policy_forecast_object,  file = "../data/nonlife_policy_forecast.RData")




# nonlife_monthly_premium <- daywisetotalprem %>%
#     filter(company_name == "Asia") %>%
#     mutate(month = month(mrdate, T),
#            year= year(mrdate)) %>%
#     group_by(year, month) %>%
#     mutate(n = sum(total),
#            date = max(mrdate)) %>%
#     ungroup() %>%
#     select(date, cnt = n) %>%
#     distinct() %>%
#     ungroup()
# 























rm(list = ls())  # clear the environment






















