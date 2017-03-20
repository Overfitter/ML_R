#looking at the data
str(train)
#Univariate analysis
summary(train)
##check and Change variables types as appropriate
soltest$Loan_Amount_Term=as.factor(soltest$Loan_Amount_Term)
soltest$Credit_History=as.factor(soltest$Credit_History)
#Looking at number of unique values in categorical variables before indiidual variable analysis
apply(train,2,function(x){length(unique(x))})
#Multi-variate analysis of two categorical variables
library(gmodels)
CrossTable(train$Dependents,train$Loan_Status)
library("ggplot2")
ggplot(train,aes(x=Dependents,fill=Loan_Status))+geom_bar()+labs(title="Stacked bar chart",x="dependents",y="loanstatus")+theme_bw()
#Categorical to continues
ggplot(train,aes(Loan_Status,ApplicantIncome))+geom_boxplot()
#Checking for Missing values and treating them(you may use summary function)
## character missing values as blanks can be imputed manually this way
summary(train)
colSums(is.na(train))
train$Gender[train$Gender==""]="Male"
##Replacing missing values of numeric variable manually with mean or median
train$LoanAmount[is.na(train$LoanAmount)]=136
#
..
..##Imputing by missForest package(impute missing values by creating random forest models)

library(missForest)
train[train ==""]=NA   ##Converting blank missing data in categorical variables to NA for imputation
forest_impu=missForest(train[,-1]) ##-1 for excluding ID variable for multiple categories
id_var=subset(soltest,select=(Loan_ID))##Subsetting ID Variable
imputed_data=forest_impu$ximp ##to call upon  results from list(forest_impu)

forest_impu$OOBerror ##For error estimation(NRMSE=Continues variable error,PFC=Categorical var error)
##we can tune the model by using ntree and mtry parameters

...##add more
#recoding character variable to new numaric variable
library(plyr)
loantest$mar_num=revalue(loantest$Married,c("No"=2,"Yes"=1))

#Outlier treatment
## we can detect outlier by simply creating scatter plots for continues variables and may also box plots
ggplot(train,aes(Loan_ID,ApplicantIncome))+geom_point()
## You should also check for impossible values like ApplicantIncome cannot be 0
.
..
...
##Binning Continues variables
imputed_train$Income_buc[imputed_train$ApplicantIncome<=2878]=1
imputed_train$Income_buc[imputed_train$ApplicantIncome>2878 & imputed_train$ApplicantIncome<=3813]=2
imputed_train$Income_buc[imputed_train$ApplicantIncome>3813 & imputed_train$ApplicantIncome<=35795]=3
imputed_train$Income_buc[imputed_train$ApplicantIncome>5795 & imputed_train$ApplicantIncome<=39460]=4
imputed_train$Income_buc[imputed_train$ApplicantIncome>9460 & imputed_train$ApplicantIncome<=381000]=5
imputed_train$Income_buc=as.factor(imputed_train$Income_buc)

imputed_train$LoanAmount[imputed_train$LoanAmount<100]=1
imputed_train$LoanAmount[imputed_train$LoanAmount<=100]=1
imputed_train$LoanAmount[imputed_train$LoanAmount>100 & imputed_train$LoanAmount<=166]=2
imputed_train$LoanAmount[imputed_train$LoanAmount>166 & imputed_train$LoanAmount<=233]=3
imputed_train$LoanAmount[imputed_train$LoanAmount>233 & imputed_train$LoanAmount<=300]=4
imputed_train$LoanAmount[imputed_train$LoanAmount>300 & imputed_train$LoanAmount<=700]=5
imputed_train$LoanAmount=as.factor(imputed_train$LoanAmount)
summary(imputed_train)
