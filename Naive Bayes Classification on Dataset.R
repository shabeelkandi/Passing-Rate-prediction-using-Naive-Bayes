library(readxl)
library("ggpubr")

options(scipen=999)
# Replace the number below by your LSE ID
ID = 201763332
# Then copy XYZprofile.r into your R working directory
source("XYZprofile.r")
# Now run the function XYZprofile with argument ID
XYZprofile(ID)


#cleaning the test data data
year=2014:2007
data=read_excel("dvsa1203.xlsx",sheet=1,col_names = T,col_types ="numeric" ,skip = 6)
data2 <- read_excel("dvsa1203.xlsx",sheet=1,col_names = T,skip = 6)
data$year <- rep(year[1],nrow(data))

for (i in 2:8){
  y1=read_excel("dvsa1203.xlsx",sheet=i,col_names = T,col_types ="numeric" ,skip = 6)
  y2=read_excel("dvsa1203.xlsx",sheet=i,col_names = T ,skip = 6)
  y1$year <- rep(year[i],nrow(y1))
  data=rbind(data,y1)
  data2=rbind(data2,y2)
}

data$Location <- data2$Location

#clearing empty rows
data<-data[complete.cases(data[ , 2:9]),]

#creating subsets
rochdale=data[data$Location =="Rochdale",2:12]
wood_green=data[data$Location =="Wood Green",2:12]

#analysing wrt year
b.rc=aggregate(.~year,rochdale , FUN=sum)
b.rc$`Total_Pass rate`=b.rc$Total_Passes/b.rc$Total_Conducted
b.rc$Male_Pass_rate =b.rc$Male_Passes/b.rc$Male_Conducted
b.wg=aggregate(.~year,wood_green , FUN=sum)
b.wg$`Total_Pass rate`=b.wg$Total_Passes/b.wg$Total_Conducted
b.wg$Male_Pass_rate =b.wg$Male_Passes/b.wg$Male_Conducted

#finding mean of pass rate for xyz at hometown
mean(rochdale$Male_Pass_rate[rochdale$Age == 22])

#finding mean of pass rate for xyz at LSE
mean(wood_green$Male_Pass_rate[wood_green$Age == 22])

#taking cum of columns for both cities
sum.rc=colSums(rochdale[2:9])
sum.wg=colSums(wood_green[2:9])               

#creating likelihood tables by age 
#for rochdale
age.rc=aggregate(.~Age,rochdale , FUN=sum)
age.rc$`Total_Pass rate` = age.rc$Total_Passes/age.rc$Total_Conducted
age.rc$Female_Pass_rate = age.rc$Female_Passes/age.rc$Female_Conducted
age.rc$Male_Pass_rate = age.rc$Male_Passes/age.rc$Male_Conducted
#for wood green
age.wg=aggregate(.~Age,wood_green , FUN=sum)
age.wg$`Total_Pass rate` = age.wg$Total_Passes/age.wg$Total_Conducted
age.wg$Female_Pass_rate = age.wg$Female_Passes/age.wg$Female_Conducted
age.wg$Male_Pass_rate = age.wg$Male_Passes/age.wg$Male_Conducted

#likelihood table for gender
#for rochdale
sum.rc["Total_Pass rate"] = sum.rc["Total_Passes"]/sum.rc["Total_Conducted"]
sum.rc["Female_Pass_rate"] = sum.rc["Female_Passes"]/sum.rc["Female_Conducted"]
sum.rc["Male_Pass_rate"] = sum.rc["Male_Passes"]/sum.rc["Male_Conducted"]
#for wood green
sum.wg["Total_Pass rate"] = sum.wg["Total_Passes"]/sum.wg["Total_Conducted"]
sum.wg["Female_Pass_rate"] = sum.wg["Female_Passes"]/sum.wg["Female_Conducted"]
sum.wg["Male_Pass_rate"] = sum.wg["Male_Passes"]/sum.wg["Male_Conducted"]


#probabilities required for bayes formula
#probability of being age 22 for both cities
pr.age.rc=age.rc$Total_Conducted[age.rc$Age == 22]/sum.rc[["Total_Conducted"]]
pr.age.wg=age.wg$Total_Conducted[age.wg$Age == 22]/sum.wg[["Total_Conducted"]]
#probability of being male
pr.male.rc=sum.rc[["Male_Conducted"]]/sum.rc[["Total_Conducted"]]
pr.male.wg=sum.wg[["Male_Conducted"]]/sum.wg[["Total_Conducted"]]

#probability of being age 22 given passed for both cities
pr.pass.age.rc=age.rc$Total_Passes[age.rc$Age == 22]/sum.rc[["Total_Passes"]]
pr.pass.age.wg=age.wg$Total_Passes[age.wg$Age == 22]/sum.wg[["Total_Passes"]]
#probability of being male given passed
pr.pass.male.rc=sum.rc[["Male_Passes"]]/sum.rc[["Total_Passes"]]
pr.pass.male.wg=sum.wg[["Male_Passes"]]/sum.wg[["Total_Passes"]]

#finding the bayesian probability of XYZ passing in hometown, given Age and Gender at both locations
#for rochdale
prob.pass.rc=(pr.pass.age.rc * pr.pass.male.rc * sum.rc[["Total_Pass rate"]])/(pr.age.rc*pr.male.rc)
#for wood green
prob.pass.wg=(pr.pass.age.wg * pr.pass.male.wg * sum.wg[["Total_Pass rate"]])/(pr.age.wg*pr.male.wg)

