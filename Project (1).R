# read the dataset
abandoned = read.csv("data/Abandoned_Data_Seed.csv")
reservation = read.csv("data/Reservation_Data_Seed.csv")


#Summary Statistics Abandoned 
abandoned$Test  <- NA
abandoned$Test[abandoned$Test_Control == "test"] <- 1
abandoned$Test[abandoned$Test_Control == "control"] <- 0

summary(abandoned$Test)

#Summary Statistics Abandoned by States
abandoned$Has_State <- 0
abandoned$Has_State[abandoned$Address != ""] <- 1

summary(abandoned$Test[abandoned$Has_State == 1])


#Data Matching

summary(abandoned)

#Attributes like First Name and Last Name are missing in the datasets 
#Therefor use attributes like Email Address, Contact Phone, (Incoming Phone,Last Name), [First Name, Last Name, Zip]
# There were a lot of empty cells in the dataframe, therefore lets fill the empty values with NA
abandoned[abandoned == ""] <- NA
reservation[reservation == ""] <- NA

#Since the caller Id and session Id wont uniquely identifies the customers, we might have to match 
#customers with data like email, phone, unique combination of first_name, last_name, Zipcode 

#Once we have identified the match in the abandoned customers with reserved customers, we might have to 
# find the duplicates and remove them

#Matching with Email from abandoned with reservation dataset
Email_Matches_Abandoned <- ifelse(!is.na(abandoned$Email),abandoned$Email %in% reservation$Email,FALSE)

#Contact Phone Matches from abandoned with reservation dataset
ContactPhone_Matches_Abandoned <- ifelse(!is.na(abandoned$Contact_Phone),abandoned$Contact_Phone %in% reservation$Contact_Phone,FALSE)

#Last Name, Incoming Phone Matches from abandoned with reservation dataset
LastNamePhones_Matches_Abandoned <- ifelse(!is.na(abandoned$Last_Name) & !is.na(abandoned$Incoming_Phone),paste0(abandoned$Last_Name,abandoned$Incoming_Phone) %in% paste0(reservation$Last_Name,reservation$Incoming_Phone),FALSE)

#First Name, Last Name, Zip Matches
NameZip_Matches_Abandoned <- ifelse((!is.na(abandoned$First_Name) & !is.na(abandoned$Last_Name)) & !is.na(abandoned$Zipcode) ,paste0(abandoned$First_Name,abandoned$Last_Name,abandoned$Zipcode) %in% paste0(reservation$First_Name,reservation$Last_Name,reservation$Zipcode),FALSE)


# Combine all Matches
All_Matches_Abandoned = Email_Matches_Abandoned | ContactPhone_Matches_Abandoned | LastNamePhones_Matches_Abandoned | NameZip_Matches_Abandoned
abandoned_matches <- abandoned[All_Matches_Abandoned,]

#Remove Duplicates
abandoned_matches <- abandoned_matches[!duplicated(abandoned_matches[,c("Email")],incomparables = NA),]
abandoned_matches <- abandoned_matches[!duplicated(abandoned_matches[,c("Contact_Phone")],incomparables = NA),]


#We tried different combinations just to make sure there weren't any duplicates
incoming_dup <- duplicated(abandoned_matches[,c("Incoming_Phone")],incomparables = NA)
lastname_dup <- duplicated(abandoned_matches[,c("Last_Name")],incomparables = NA)
firstname_dup <- duplicated(abandoned_matches[,c("First_Name")],incomparables = NA)
zipcode_dup <- duplicated(abandoned_matches[,c("Zipcode")],incomparables = NA)
abandoned_data_matches <- abandoned_matches[!(incoming_dup & lastname_dup),]
abandoned_data_matches <- abandoned_matches[!(firstname_dup & lastname_dup & zipcode_dup),]

# Store Outcome in original dataset
abandoned$Outcome <- 0
abandoned$Outcome[as.numeric(row.names(abandoned_matches))] <- 1


#We found around 223 matches from abandoned dataset with reservation dataset, though the no's seem less
#we should see whether the retargeting is substantially significant, if the profit from retargeting is good
#then we might say that retargeting is a good idea, lets explore that further.

table(abandoned$Test,abandoned$Outcome)

#library(caret)
#confusionMatrix(factor(abandoned$Test),factor(abandoned$Outcome))

abandoned_NY<-filter(abandoned, Address == "NY")
table(abandoned_NY$Test,abandoned_NY$Outcome)

abandoned_AZ<-filter(abandoned, Address == "AZ")
table(abandoned_AZ$Test,abandoned_AZ$Outcome)

abandoned_OH<-filter(abandoned, Address == "OH")
table(abandoned_OH$Test,abandoned_OH$Outcome)

abandoned_CA<-filter(abandoned, Address == "CA")
table(abandoned_CA$Test,abandoned_CA$Outcome)

abandoned_IL<-filter(abandoned, Address == "IL")
table(abandoned_IL$Test,abandoned_IL$Outcome)

# Where Test Variable indicates, again, the treatment or the control group,
# Outcome is a binary variable indicating whether a vacation package was ultimately bought, 



res_email_match <- match(abandoned_matches$Email, reservation$Email, nomatch = 0, incomparables = NA)
res_phone_match <- match(abandoned_matches$Contact_Phone, reservation$Contact_Phone, nomatch = 0, incomparables = NA)

res_name_phone_match <- ifelse(!is.na(abandoned_matches$Last_Name) & !is.na(abandoned_matches$Incoming_Phone),match(paste0(abandoned_matches$Last_Name,abandoned_matches$Incoming_Phone), paste0(reservation$Last_Name,reservation$Incoming_Phone), nomatch = 0, incomparables = NA),0)
res_name_zip_match <- ifelse(!is.na(abandoned_matches$First_Name) & !is.na(abandoned_matches$Last_Name) & !is.na(abandoned_matches$Zipcode),match(paste0(abandoned_matches$First_Name,abandoned_matches$Last_Name,abandoned_matches$Zipcode), paste0(reservation$First_Name,reservation$Last_Name,reservation$Zipcode), nomatch = 0, incomparables = NA),0)


reservation_matches <- res_email_match
reservation_matches <- ifelse(reservation_matches == 0,reservation_matches+res_phone_match,reservation_matches)
reservation_matches <- ifelse(reservation_matches == 0,reservation_matches+res_name_phone_match,reservation_matches)
reservation_matches <- ifelse(reservation_matches == 0,reservation_matches+res_name_zip_match,reservation_matches)


abandoned_all_matches <- as.numeric(row.names(abandoned_matches))

# Days in between is the (largest) difference between the dates in the ABD and RS dataset (Columns B). 
# If no purchase, set "Days_in_between" as "200".

abandoned$Days_in_between <- 200
abandoned$Days_in_between[abandoned$Outcome == 1] <- as.numeric(as.Date(reservation$Session[reservation_matches],"%Y.%m.%d %H:%M:%S") - as.Date(abandoned$Session[abandoned_all_matches],"%Y.%m.%d %H:%M:%S")) 


abandoned_data_clean <- data.frame(c(1:nrow(abandoned)),abandoned$Test,abandoned$Outcome,abandoned$Days_in_between,abandoned$Address)
colnames(abandoned_data_clean) <- c("Customer_ID","Test_Variable","Outcome","Days_in_Between","State")

write.csv(abandoned_data_clean,file = "Cleaned_Abandoned_Data.csv")


Test <- abandoned$Test_Control
Test[Test=="test"] <- 1
Test[Test=="control"] <- 0

mean(as.numeric(Test))
median(as.numeric(Test))
sd(as.numeric(Test))
quantile(as.numeric(Test),c(.05,.95))
hist(as.numeric(Test), main="Histogram of Test_Control Variable",labels = c("Control","","","","","","","","","Test"))



Test <- abandoned$Test_Control[abandoned$Address!=""]
Test[Test=="test"] <- 1
Test[Test=="control"] <- 0
Test <- as.numeric(Test)
mean(as.numeric(Test))
median(as.numeric(Test))
sd(as.numeric(Test))
quantile(as.numeric(Test),c(.05,.95))
hist(as.numeric(Test), main="Histogram of Test_Control Variable (State-only Level)",labels = c("Control","","","","","","","","","Test"))


library(ggplot2)                           
# Setting up the vectors                           
Outcome <- c("Buy","No Buy")
Group <- c("Control","Trearment")
# Creating data frame
df <- expand.grid(Outcome, Group)
df$value <- c(30,3614,139,3541)    
#Plotting Data
g <- ggplot(df, aes(Var1, Var2)) + geom_point(aes(size = value), colour = "red") + theme_bw() + xlab("Outcome") + ylab("Group")
g + scale_size_continuous(range=c(1,30)) + geom_text(aes(label = value))


#  Run a Linear regression model
library(data.table);library(stargazer);library(plm); library(ggplot2); library("readxl"); library(dplyr)

lmodel1 <- lm(abandoned_data_clean$Outcome ~ abandoned_data_clean$Test_Variable)
stargazer(lmodel1,title="Retargeting effect on package booking",type="text")

abandoned$Has_Email <- 0
abandoned$Has_Email[!is.na(abandoned$Email)] <- 1
lmodel2 <- lm(abandoned_data_clean$Outcome ~ abandoned_data_clean$Test_Variable + abandoned$Has_Email + abandoned$Has_State)
stargazer(lmodel2,title="Retargeting effect on package booking with dummies",type="text")

#Hence, from the managerial perspective, customers who have a recorded email address and state on file from the treatment group grouping per state have more chances of converting to a reservation category.
lmodel3 <- lm(abandoned_data_clean$Outcome ~ abandoned_data_clean$Test_Variable*abandoned$Has_Email + abandoned$Has_State)
stargazer(lmodel3,title="Retargeting effect on package booking with interaction effect",type="text")

lmodel4 <- lm(abandoned_data_clean$Outcome ~ abandoned_data_clean$Days_in_Between*abandoned_data_clean$Test_Variable)
stargazer(lmodel4,title="Retargeting effect on package booking with Days in between",type="text")

#lmodel5 <- lm(abandoned_data_clean$Outcome ~ abandoned_data_clean$Days_in_Between:abandoned_data_clean$Test_Variable+abandoned_data_clean$Test_Variable:abandoned$Has_Email + abandoned$Has_State)
#stargazer(lmodel5,title="Retargeting effect on package booking with all the interaction effects available",type="text")

lmodel5 <- lm(abandoned_data_clean$Outcome ~ abandoned_data_clean$Days_in_Between:abandoned_data_clean$Test_Variable+abandoned_data_clean$Test_Variable:abandoned$Has_Email + abandoned_data_clean$Test_Variable:abandoned$Has_State)
stargazer(lmodel5,title="Retargeting effect on package booking with all the interaction effects available",type="text")




