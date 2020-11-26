#*** Causal Analytics and A/B Testing ***#
#*** Graduate Course at UTD ***#
#*** Developed by Amit Mehra, Aug2020 ***#
#*** Code for Lecture 2 ***#

rm(list=ls());gc()
library(data.table);library(stargazer);library(plm); library(ggplot2)

#*** Load Dataset ***#

MyData = fread(input='/Users/amehra/Dropbox/PROFESSIONAL/TEACHING/UTD/MSIS/CausalAnalytics2020/Session 2/StudentDataset.csv', verbose = T)

#*** Basic descriptive statistics ***#
length(unique(MyData$student_id))
unique(MyData$semester)
hist(MyData$sem_grd_pts)
hist(MyData$wifi_hours)

#*** Plots for one student as example ***#
qplot(semester,wifi_hours,data=MyData[student_id==858]) + theme_set(theme_grey(base_size = 24))
qplot(semester,sem_grd_pts,data=MyData[student_id==858]) + theme_set(theme_grey(base_size = 24))
qplot(wifi_hours,sem_grd_pts,data=MyData[student_id==858]) + theme_set(theme_grey(base_size = 24))

#***Simple regression without using the panel nature of data  ***#
ols <-lm(sem_grd_pts ~ log(wifi_hours), data = MyData)
stargazer(ols,title="OLS gpa on wi-fi hours",type="text")

#*** SIMPLE PLM with CLUSTERED ROBUST STANDARD ERRORS ***#
plm <-plm(sem_grd_pts ~ log(wifi_hours), data = MyData, index=c("student_id","semester"),model="pooling")
stargazer(plm,plm,se=list(NULL,sqrt(diag(vcovHC(plm, method="arellano", type="HC1")))),title="Panel OLS without and with clustered robust standard errors",type="text",column.labels=c("without CRSE","with CRSE"),omit = c("factor[(]cohort[)]","factor[(]major_id[)]"),omit.yes.no = c("Yes","No"))

#*** PLM with CLUSTERED ROBUST STANDARD ERRORS and other observed covariates ***#
plm <-plm(sem_grd_pts ~ log(wifi_hours) + app_avg_grade + factor(cohort) + factor(major_id), data = MyData, index=c("student_id","semester"),model="pooling")
stargazer(plm,plm,se=list(NULL,sqrt(diag(vcovHC(plm, method="arellano", type="HC1")))),title="Panel OLS without and with clustered robust standard errors",type="text",column.labels=c("without CRSE","with CRSE"),omit = c("factor[(]cohort[)]","factor[(]major_id[)]"),omit.yes.no = c("Yes","No"))

#*** ADDING TIME DUMMIES ***#
plmtd <-plm(sem_grd_pts ~ log(wifi_hours) + app_avg_grade + factor(cohort) + factor(major_id) + factor(semester), data = MyData, index=c("student_id","semester"),model="pooling")
stargazer(plm,plmtd,se=list(sqrt(diag(vcovHC(plm, method="arellano", type="HC1"))),sqrt(diag(vcovHC(plm, method="arellano", type="HC1")))),title="Panel without and with time dummies",type="text",column.labels=c("no td","add td"),omit = c("factor[(]cohort[)]","factor[(]major_id[)]","factor[(]semester[)]"),omit.yes.no = c("Yes","No"))

#*** FIXED EFFECTS ***#
plmtdfe <-plm(sem_grd_pts ~ log(wifi_hours) + app_avg_grade + factor(cohort) + factor(major_id) + factor(semester), data = MyData, index=c("student_id","semester"),model="within")
stargazer(plmtd,plmtdfe,se=list(sqrt(diag(vcovHC(plmtd, method="arellano", type="HC1"))),sqrt(diag(vcovHC(plmtdfe, method="arellano", type="HC1")))),title="Panel results with td, adding fixed effects",type="text",column.labels=c("with td","with td and fixed effects"),omit = c("factor[(]cohort[)]","factor[(]major_id[)]","factor[(]semester[)]"),omit.yes.no = c("Yes","No"))




