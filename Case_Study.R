#setwd("~/Documents/IIIT/Case-Study/Gramener")

#install OneR to create bins for continuous data of salaries.
install.packages("OneR")

#Source the libraries
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(OneR)

#Read the data file
loan <- read.csv("loan.csv", stringsAsFactors = FALSE)

#Data Cleaning:
#################

#Remove columns with all NA values
loan <- loan[,colSums(is.na(loan)) != nrow(loan)]

#Convert the date string in date format
str_to_date <-function(column_name){
date_time_req = as.POSIXlt(paste("01-",column_name,sep=""), format = "%d-%b-%y")
format(date_time_req,"%d-%b-%y")
}

loan$issue_d <- str_to_date(loan$issue_d)
loan$last_pymnt_d <- str_to_date(loan$last_pymnt_d)
loan$next_pymnt_d <- str_to_date(loan$next_pymnt_d)
loan$earliest_cr_line <- str_to_date(loan$earliest_cr_line)
loan$last_credit_pull_d <- str_to_date(loan$last_credit_pull_d)

#replace empty string with NA 
loan$last_pymnt_d[loan$last_pymnt_d == ""]  <- "NA"
loan$next_pymnt_d[loan$next_pymnt_d == ""]  <- "NA"

#replace NA in emp_length with 0
loan$emp_length[which((loan$emp_length=="n/a"))] <- "0 year"

#Convert all entries to same case
loan$title <- tolower(loan$title)

#Function to replace string values in a column
replace_str <- function(col_vector,old_str,new_str) {
  str_replace_all(col_vector,old_str,new_str)
} 

#Create bins for Annual Income 
loan$income_bin <- round(loan$annual_inc / 10000)
loan$income_bin[which(loan$income_bin>120)] <- 120

#Create bins for loan_amount
loan$loan_amnt_bin <- round(loan$loan_amnt / 1000)

#Create bins for revolv_bal
loan$revol_bal_bin <- round(loan$revol_bal/1000)

#Create bins for total_paymnt
loan$total_pymnt_bin <- round(loan$total_pymnt/1000)


#Assign value to categorical loan_status
#Fully Paid = 1
#Current = 0
#Charged off = -1
find_status <- function(cl_name, status) {
  which(cl_name==status)
}

loan$loan_status_value[find_status(loan$loan_status,"Fully Paid")] <- 0
loan$loan_status_value[find_status(loan$loan_status,"Current")] <- 0
loan$loan_status_value[find_status(loan$loan_status,"Charged Off")] <- 1

#Function to get percentage of total charged off loans
get_percent <- function(x) {
  prcnt <- round(sum(x)/ length(x) * 100,2)
}

#Univariate analysis

#find percent of loan status
loan_status_group <- group_by(loan,loan_status)
loan_status_group_sum <- summarise(loan_status_group,count=n())
#add percent column
loan_status_group_sum <- mutate(loan_status_group_sum,percent=count/sum(count)*100)
#pie chart for loan status
pie(loan_status_group_sum$percent,labels=paste(loan_status_group_sum$loan_status, sep = "\n",paste(round(loan_status_group_sum$percent,2),sep='','%')))

#aggregate based on term (60 months has max defaults)
loan_term <- aggregate(loan$loan_status_value,by = list(loan$term), FUN=get_percent)
ggplot(loan_term,aes(x=loan_term$Group.1, y = loan_term$x)) + geom_bar(stat = 'identity')
#There are more percentage of loan defaulters in case of 60 months of term.

#aggregate based on grade (Grade G has max defaults)
loan_grade <- aggregate(loan$loan_status_value, by = list(loan$grade), FUN=get_percent)
ggplot(loan_grade,aes(x=loan_grade$Group.1, y = loan_grade$x)) + geom_bar(stat = 'identity')
#Keeping a boundary around 12% default tolerance, only A and B grades can be allowed. Remaining all grades
#exceed the tolerance boundary

#aggregate based on emp_length (Emp length = 0 has max defaults)
loan_emp_len <- aggregate(loan$loan_status_value, by = list(loan$emp_length), FUN=get_percent)
ggplot(loan_emp_len,aes(x=loan_emp_len$Group.1, y = loan_emp_len$x)) + geom_bar(stat = 'identity')
#Keeping a boundary around 12% default tolerance, 0, 10+ and 7 years exceed the tolerance

#aggregate based on home_ownership (Other ownership has max defaults)
loan_ho <- aggregate(loan$loan_status_value,by = list(loan$home_ownership), FUN=get_percent)
ggplot(loan_ho,aes(x=loan_ho$Group.1, y = loan_ho$x)) + geom_bar(stat = 'identity')
#Picking up the Other and Rent as the ownership with more default cases

#aggregate based on verification_status ()
loan_vs <- aggregate(loan$loan_status_value,by = list(loan$verification_status), FUN=get_percent)
ggplot(loan_vs,aes(x=loan_vs$Group.1, y = loan_vs$x)) + geom_bar(stat = 'identity')
#Picking up Verified and Source Verified as cases with most default instances

#aggregate based on purpose (Small business has maximum defaults)
loan_purpose <- aggregate(loan$loan_status_value, by = list(loan$purpose), FUN=get_percent)
ggplot(loan_purpose,aes(x=loan_purpose$Group.1, y = loan_purpose$x)) + geom_bar(stat = 'identity')
#Picking debt_consolidation, education, housing, medical, moving, other, renewable_energy and small_business as
#cases with more default instances.

#aggregate based on addr_state (NE has max defaults)
loan_addr_state <- aggregate(loan$loan_status_value, by = list(loan$addr_state), FUN=get_percent)
ggplot(loan_addr_state,aes(x=loan_addr_state$Group.1, y = loan_addr_state$x)) + geom_bar(stat = 'identity')

#aggregate based on inq_last_6month ()
loan_inq_6mnths <- aggregate(loan$loan_status_value, by = list(loan$inq_last_6mths), FUN=get_percent)
ggplot(loan_inq_6mnths,aes(x=loan_inq_6mnths$Group.1, y = loan_inq_6mnths$x)) + geom_bar(stat = 'identity')

#aggregate based on open_acc ()
loan_open_acc <- aggregate(loan$loan_status_value, by = list(loan$open_acc), FUN=get_percent)
ggplot(loan_open_acc,aes(x=loan_open_acc$Group.1, y = loan_open_acc$x)) + geom_bar(stat = 'identity')

#aggregate based on dti
loan$dti_bin <- round(loan$dti / 2)
View(dti_bin)
loan_dti <- aggregate(loan$loan_status_value, by = list(loan$dti_bin), FUN=get_percent)

#loan_dti <- aggregate(loan$loan_status_value, by = list(loan$dti), FUN=get_percent)
ggplot(loan_dti,aes(x=loan_dti$Group.1, y = loan_dti$x)) + geom_bar(stat = 'identity')

#aggregate based on total_acc
loan_total_acc <- aggregate(loan$loan_status_value, by = list(loan$total_acc), FUN=get_percent)
ggplot(loan_total_acc,aes(x=loan_total_acc$Group.1, y = loan_total_acc$x)) + geom_bar(stat = 'identity')

#aggregate based on annual_income
loan_income <- aggregate(loan$loan_status_value, by = list(loan$income_bin), FUN=get_percent)
loan_income <- setNames(loan_income,c("Income_Bin","Percent_Default"))
ggplot(loan_income,aes(x=Income_Bin, y = Percent_Default)) + geom_bar(stat = 'identity')
#Income bin of 0-10 and 35-onwards have higher chances of defaults

#aggregate based on loan_amount
loan_amnt <- aggregate(loan$loan_status_value, by = list(loan$loan_amnt_bin), FUN=get_percent)
loan_amnt <- setNames(loan_amnt,c("Loan_Amount_Bin","Percent_Default"))
ggplot(loan_amnt,aes(x=Loan_Amount_Bin, y = Percent_Default)) + geom_bar(stat = 'identity')
#loan_amount bin > 15 has higher chances of default

#aggregate based on total_pymnt_bin
loan_pymnt <- aggregate(loan$loan_status_value, by = list(loan$total_pymnt_bin), FUN=get_percent)
loan_pymnt <- setNames(loan_pymnt,c("Payment_Bin","Percent_Default"))
ggplot(loan_pymnt,aes(x=Payment_Bin, y = Percent_Default)) + geom_bar(stat = 'identity')
#loan payment bin <10 has higher chances of default

#aggregate based on revol_bal_bin
loan_revol_bal <- aggregate(loan$loan_status_value, by = list(loan$revol_bal_bin), FUN=get_percent)
ggplot(loan_revol_bal,aes(x=loan_revol_bal$Group.1, y = loan_revol_bal$x)) + geom_bar(stat = 'identity')

#Bivariate Analysis
#####################

#For this purpose, let us take subset of loan data for only charged off instances
# defaulter subset
loan_defaulter <- subset(loan,loan_status=="Charged Off")

##check the structure of data##
str(loan_defaulter)
unique(loan_defaulter$addr_state)

#bivariate analysis between employee length and term
ggplot(loan_defaulter, aes(x=emp_length,fill=term)) + geom_bar(position = "fill")

#heatmap between employee length and purpose
loan_defaulter_group <- group_by(loan_defaulter,emp_length,purpose)#,term,home_ownership,grade,verification_status,addr_state)
loan_defaulter_group_count <- summarise(loan_defaulter_group,count=n())

ggplot(loan_defaulter_group_count, aes(x = emp_length, y = purpose,fill=count)) +
  geom_tile()

#bivariate analysis between employee length and Verification status
ggplot(loan_defaulter, aes(x=emp_length,fill=verification_status)) + geom_bar()

#heatmap between employee length and verification status
loan_defaulter_group1 <- group_by(loan_defaulter,emp_length,verification_status)
loan_defaulter_group1_count <- summarise(loan_defaulter_group1,count=n())

ggplot(loan_defaulter_group1_count, aes(x = emp_length, y = verification_status,fill=count)) +
  geom_tile()

#bivariate analysis between Verification status and term
ggplot(loan_defaulter, aes(x=verification_status,fill=term)) + geom_bar(position = "fill")

#heatmap between verification status and term
loan_defaulter_group2 <- group_by(loan_defaulter,verification_status,term)
loan_defaulter_group2_count <- summarise(loan_defaulter_group2,count=n())

ggplot(loan_defaulter_group2_count, aes(x = verification_status, y = term,fill=count)) +
  geom_tile()

#heatmap between verification status and term
loan_defaulter_group2 <- group_by(loan_defaulter,verification_status,term)
loan_defaulter_group2_count <- summarise(loan_defaulter_group2,count=n())

ggplot(loan_defaulter_group2_count, aes(x = verification_status, y = term,fill=count)) +
  geom_tile()

#heatmap between employee length and purpose
loan_defaulter_group3 <- group_by(loan_defaulter,purpose,term)#,term,home_ownership,grade,verification_status,addr_state)
loan_defaulter_group3_count <- summarise(loan_defaulter_group3,count=n())

ggplot(loan_defaulter_group3_count, aes(x = purpose, y = term,fill=count)) +
  geom_tile()

#bivariate analysis between employee length and loan_amnt
ggplot(loan_defaulter, aes(x=emp_length,y=loan_amnt)) + geom_bar(stat="summary",fun.y="mean") 

#scatter plot between installment & funded_amnt_inv
ggplot(loan_defaulter, aes(x=funded_amnt_inv,y=installment))+ geom_point()+geom_smooth()
ggplot(loan_defaulter, aes(x=installment,y=dti))+geom_line()+geom_smooth()

# frequency plot for dti
a <- ggplot(loan_defaulter, aes(x=dti))
a+geom_freqpoly(binwidth = 1.5)

#density plot for dti
a+geom_density()

##verfication status and dti
mu <- loan_defaulter %>% 
  group_by(verification_status) %>%
  summarise(grp.mean = mean(dti))

a+ geom_density(aes(fill = verification_status)) +
  geom_vline(aes(xintercept = grp.mean, color = verification_status),
             data = mu, linetype = "dashed")

#show boxplot for loan amount and length of employee
boxplot(loan_amnt~emp_length,data=loan_defaulter)

#show boxplot for loan amount and length of employee
boxplot(loan_amnt~term,data=loan_defaulter)

#show boxplot for installment and length of employee
boxplot(installment~emp_length,data=loan_defaulter)

#The approach we are suggesting is to assign score for each variable. Based on univariate analysis,
#taking percentage of defaults for various variables, a score +1 or -1 can be added to the overall score 
#for each customer. Thresholds are specified for each variable as a business decision and overall calculation
#of score is done. Based on the final score, decision about "Loan Approval", "Loan approval with higher interest", or
#"Loan Rejection" can be done.

#Initialize the score
loan$score <- 0

#Ranking algorithm
#Score is provided based on univariate analysis. 
#Term: +1 for 36 months and -1 for 60 months
#Grade: +1 for A,B and -1 for C,D,E,F,G
#Emp_len: -1 for 0,10+ and 7 years. +1 for the remaining
#Home: -1 for Other and Rent
# Verification: -1 for verified and source verified
# Purpose: -1 for debt_consolidation, Education, house,medical, moving, other, renewable_energy and small_business
# income_bin: -1 for 0-10 and 35-onwards.
# loan_amount bin: -1 for >15
# payment_bin: -1 for <10
# DTI bin: -1 for 7 to 12. +1 for remaining

add_score <- function(cl_name,match_str) {
  loan$score[which(str_detect(cl_name,match_str))] = loan$score[which(str_detect(cl_name,match_str))] + 1
}
#1 Term
#Subtracting 1 from all the scores.
loan$score <- loan$score - 1
#Have to add 1 to the relevant score. Since 1 has been subtracted from all, need to add 2 
#in order to have effective addition of 1
loan$score[which(str_detect(loan$term,"36"))] = loan$score[which(str_detect(loan$term,"36"))] + 2

#2 Grade
loan$score <- loan$score - 1
loan$score[which(str_detect(loan$grade,"A"))] = loan$score[which(str_detect(loan$grade,"A"))]  + 2
loan$score[which(str_detect(loan$grade,"B"))] = loan$score[which(str_detect(loan$grade,"B"))]  + 2

#3. Emp_len
loan$score <- loan$score +1
loan$score[which(str_detect(loan$emp_length,"0 year"))] = loan$score[which(str_detect(loan$emp_length,"0 year"))]  - 2
loan$score[which(str_detect(loan$emp_length,"10+"))] = loan$score[which(str_detect(loan$emp_length,"10+"))]  - 2
loan$score[which(str_detect(loan$emp_length,"7"))] = loan$score[which(str_detect(loan$emp_length,"7"))]  - 2

#4. Home
loan$score <- loan$score - 1
loan$score[which(str_detect(loan$home_ownership,"Mortgage"))] = loan$score[which(str_detect(loan$home_ownership,"Mortgage"))]  + 2
loan$score[which(str_detect(loan$home_ownership,"Own"))] = loan$score[which(str_detect(loan$home_ownership,"Own"))]  + 2

#Income verified
loan$score <- loan$score +1
loan$score[which(str_detect(loan$verification_status,"Verified"))] = loan$score[which(str_detect(loan$verification_status,"Verified"))]  - 2
loan$score[which(str_detect(loan$verification_status,"Source Verified"))] = loan$score[which(str_detect(loan$verification_status,"Source Verified"))]  - 2

#Purpose
loan$score <- loan$score +1
loan$score[which(str_detect(loan$purpose,"debt_consolidation"))] = loan$score[which(str_detect(loan$purpose,"debt_consolidation"))]  - 2
loan$score[which(str_detect(loan$purpose,"education"))] = loan$score[which(str_detect(loan$purpose,"education"))]  - 2
loan$score[which(str_detect(loan$purpose,"house"))] = loan$score[which(str_detect(loan$purpose,"house"))]  - 2
loan$score[which(str_detect(loan$purpose,"medical"))] = loan$score[which(str_detect(loan$purpose,"medical"))]  - 2
loan$score[which(str_detect(loan$purpose,"moving"))] = loan$score[which(str_detect(loan$purpose,"moving"))]  - 2
loan$score[which(str_detect(loan$purpose,"other"))] = loan$score[which(str_detect(loan$purpose,"other"))]  - 2
loan$score[which(str_detect(loan$purpose,"renewable_energy"))] = loan$score[which(str_detect(loan$purpose,"renewable_energy"))]  - 2
loan$score[which(str_detect(loan$purpose,"small_business"))] = loan$score[which(str_detect(loan$purpose,"small_business"))]  - 2

#Annual income bin
loan$score <- loan$score +1
loan$score[which(loan$income_bin<=10)] = loan$score[which(loan$income_bin<=10)]  - 2
loan$score[which(loan$income_bin>=35)] = loan$score[which(loan$income_bin>=35)]  - 2

#loan amount
loan$score <- loan$score +1
loan$score[which(loan$loan_amnt_bin>15)] = loan$score[which(loan$loan_amnt_bin>15)]  - 2

#Total payment 
loan$score <- loan$score +1
loan$score[which(loan$total_pymnt_bin<10)] = loan$score[which(loan$total_pymnt_bin<10)]  - 2

#DTI
loan$score <- loan$score +1
loan$score[which(loan$dti_bin>=7 & loan$dti_bin<=12)] = loan$score[which(loan$dti_bin>=7 & loan$dti_bin<=12)]  - 2

# >=0 = Disburse with full 
#-6 to -2 = High interest or Less 
#<-6 = Deny 