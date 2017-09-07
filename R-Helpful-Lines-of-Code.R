citation()
R.Version()

#Make contrasts
# Check levels
levels(d$group)

# Make contrasts and set ama as baseline (base = 1)
contrasts(d$group) = contr.treatment(3, base = 1)

NonVsAma = c(-1, 1, 0)
AmaVsPro = c(-1, 0, 1)
contrasts(d$group) = cbind(NonVsAma, AmaVsPro)
melodymodel = lm(melody ~ group, data = d)
rhythmmodel = lm(rhythm ~ group, data = d)
summary.lm(melodymodel)
summary.lm(rhythmmodel)

#MANOVA
dep = cbind(无d$outvar1, 无d$outvar2)
fit = manova(dep ~ 无invar, 无d)

summary(fit, test="Pillai")

#outlier-detector - plot
aq.plot(无dataframe)

#mshapiro - only owrks on matrices - see transpose
mshaprio.test(无matrix)

#Transpose dataframe to matrix
无newmatrix = t(无dataframe[无rows,无columns])

#Make mixed effects model - two ways
model1 = lmer(rt ~ cond_emo*cond_blue*freq + (1|ID), ds, REML=F)
model11 = lme(rt ~ cond_emo*cond_blue*freq, random = ~1|ID, data=ds, method = "ML")

#make subset of data based on responses
ds = d[which(d$correct_resp == "correct"), ]

#Boxplot 
ggplot(data, aes(xvar, yvar, colour=splitvar)) + 
  geom_boxplot() + 
  facet_wrap(~freq, labeller = label_both) + 
  scale_fill_manual(values=c("yellow","blue","yellow","blue","yellow","blue","yellow","blue"))


summaries = gsummary(dataframe)   #generate summary stat for all data - defaukt mean - per participant
view(summaries)

model = lme (outcar ~ invar + 1|randomvar, data = dataframe)  # use method = "ML" if you want to do model comparison
summary(model)

modelsplit = by(dataframe, splitbyvar, function(x) lm(outvar ~invar, data = x))
summary (model$splitbyvar1)
summary (model$splitbyvar2)

anova (model, type = "marginal")
anova (model, type = "sequential")
  
t.test ( outvar ~ invar, data = dataframe, var.equal = T)

#Rename
Data_new = rename(data, c(var1 = "name1", var2 = "name2" ... varn = "namen"))

#extract test statistics
F_list=c(summary(m2)$fstatistic[1],
         summary(m3)$fstatistic[1],
         summary(m4)$fstatistic[1])
#Name models
m_list=c("Base","Cultural","Practice")
#Add into dataframe
statf=as.data.frame(list(m_list,F_list))
#Rename columns in dataframe
colnames(statf) <- c("Model", "F_value")
#Plot F values by models
ggplot(statf, aes(Model, F_value, fill=Model))+
  geom_bar(stat="summary",fun.y=mean)+
  labs(x='Model',y='F value', title='F values for different models')+
  theme(legend.position="none")

#Do a planned contrasts lm regression.
K=matrix(data=0,nrow=3,ncol=4) #make a matrix for the contrast values
K[1,]=matrix(c(0,0,0))   #write in the contrasts
K[2,]=matrix(c(0,0,0))
K[3,]=matrix(c(0,0,0))

t=glht(无model,linfct=K)   #make the contrasted model
summary(t,test=adjusted("none"))   #get test statistics

image(K,col=gray(1:10/10))   #make an image of the contrasts

PostHocs = glht(model,linfct=dose())  #unfinished. Look in slides for week 2


m2=matrix(data=0,nrow=30,ncol=7)
m2[,c(1) = as.matrix(data$outcome)]   #also unfinished. look in slides. NTROL


#Check for missing values
which(is.na(data$var))
which(!complete.cases(politeness))

#make a planned t-test
#First decide on the weightings / contrasts
contrasts(data$Condition)=cbind(), cbind(c(),c(),c())  #put weightings in the lists, same order as in the data
model = aov(outcome~Condition,data)
summary.lm(model)
#alternative
contrasts(data$Condition)=contr.poly(5)  #put weightings in the lists, same order as in the data
model = aov(outcome~Condition,data)
summary.lm(model)

#pairwise post-hoc tests
mcppb(outcome~predictor,data)#tr=0,2, nboot=599, crit=NA

#Trimmed means
t1waybt(outcome~predictor,data) #tr=0,2, nboot=599 - how much to trim and how many times to repeat

#Levenetest
leveneTest(outcome~predictor,data)
#if significant, use: Welch's F
oneway.test(outcome~predictor,data)

# ANOVA:
#Is data normally distributed?
#Is there homogenity of variance? (LeveneTest)
model=lm(outcome~predictor,data)   
summary(model)
model2=aov(outcome~predictor,data)  #Good for planned testing 
summary(model2)
#post-hoc
pairwise.t.test(outcome,data$predictor, p.adjust.method="bonferroni")
pairwise.t.test(outcome,data$predictor, p.adjust.method="BH")   #more conservative

#More advanced way
library(multcomp)
postHocs = glht(model,linfct=mcp(Condition = "Tukey"))
Summary(postHocs)

#See direction
by(outcome,data$predictor,mean)   #means
boxplot(outcome~data$predictor)   #boxplot
#there is also Tukey 

# INDEPENDENT T TEST
# Long
ind.t.test<-t.test(无outvar ~ 无Predvar, 无data)
ind.t.test
# Wide
ind.t.test<-t.test(无var1, 无var2)
ind.t.test
   #If dependat: add paried=TRUE

# Calculate effect size
t<-ind.t.test$statistic[[1]]
df<-ind.t.test$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)

#For logistic regression
predictions=predict(无model2,type="response") #getting predictions out in percent
predicted_diagnosis=ifelse(predictions<.5,"无optionTRUE","无optionFALSE")   #Turning that into judgement/guess
caret::confusionMatrix(predicted_diagnosis,无data$Diagnosis, positive="无optionFALSE")

#Overall fit
anova(无model,test="Chisq")
#Or compare
anova(无model1,无model2,test="Chisq")

#improvement in model fit and effect size - binary logistic
modelChi=model$null.deviance-model$deviance   #get chi squared
chidf=model$df.null-model$df.residual   #get degrees of freedom
pchisq(modelChi,chidf)   #get p-value
R2.hl=modelChi/model$null.deviance   #effect size
   #IF null is replaced by the deviance and degrees of freedom from the base model, it is compared to base model

#Generalized linear model - binary logistic
model=glm(无varout~无varpred1+无varpred2+无varpred3...,无data,family = binomial)
summary(model)

#Combine two datasets - add below other
dplyr::bind_rows(无data1,无data2)

#Combine data by matching rows
dplyr::left_join(无data,无extrapredictors)

#coding dummy variables
contrasts(无data$var)=contr.treatment(无numberofgroups,base=无baselinegroup) 
#Alternative method with name control
无var1_v_无basevar=c(1,0,0,0)
无var2_v_无basevar=c(0,1,0,0)
无var3_v_无basevar=c(0,0,1,0)
contrasts(无data$var)=cbind(无var1_v_无basevar,无var2_v_无basevar,无var3_v_无basevar)
#make normal regression on this
round(tapply(无data$outvar,无data$predvar,mean,na.rm=TRUE),3) #average changes in different groups


#Bootstrapping regression
bootReg=function=(formula,data,indices)
{
  d=data[i,]
  fit=lm(formula,data=d)
  return(coef(fit))
}
bootResults=boot(statistic=bootReg,formula=无outvar~无predvar1+无predvar2+无predvar3,data=无data,R=2000)
boot.ci(bootResults,type="bca",index=1)   #intercept 
boot.ci(bootResults,type="bca",index=2) 
boot.ci(bootResults,type="bca",index=3) 
boot.ci(bootResults,type="bca",index=4) 

#Write datafile
write.table(无data,"无filename.csv",sep=",",row.names=FALSE)   #or true

#add predicted values to dataframa
无data$fitted=无regmodel$fitted.values

#independence test
dwt(无regmodel)

#multicollinearity test
vif(无regmodel)
mean(vif(无regmodel))

#Outliers:   #Good idea to add to dataframe as a variable
无data$resid=resid(无regmodel)   # find residuals
无data$stz.r=rstandard(无regmodel)   #standardized residuals
无data$stu.r=rstudent(无regmodel)   #studentized residuals
无data$large.resid=无data$stz.r>2|无data$stz.r<-2   #residuals larger than 2


#Influential cases   #Good idea to add to dataframe as a variable
无data$cooks=cooks.distance(无regmodel)   #Cook's distance
无data$DFBeta=dfbeta(无regmodel)   #DFBeta
无data$DFFit=dffits(无regmodel)   #DFFit
无data$leverage=hatvalues(无regmodel)   #hat values / leverage (av. leverage= (k+1)/n)
无data$covariance.ratios=covratio(无regmodel)   #covariance ratio   (CVR limits: 1+-3(k+1)/n)

#Simple regression
model=lm(无varout~无varpred,无data)   #optional: na.action=na.exclude
summary(model)
summary.lm(model)


#Multiple regression
model=lm(无varout~无varpred1+无varpred2+无varpred3...,无data)
summary(model)
lm.beta(model)   #for standardized b-values
confint(model)   #for confidence intervals
#add predictors:
newmodel=update(model,.~.+无newvar1,无newvar2)

#Predict one value
predict(lm(无varout~无varpred1+无varpred2+无varpred3...,无data),newdata=data[999,])

#Compare hierarchical models
anova(无model1, 无model2, 无model3)

#Do partial correlation
par_cor=pcor(c("无var1","无var2","无control1","无control2"),var(无data))
par_cor
par_cor^2
pcor.test(par_cor,无numberofcontrol,无n)

#Compare ratio of frequencies
prop.table(table(无data))

#Find biserial correlation
polyserial(无var1,无var2)

#Make a correlation test:
cor.test(无var1,无var2,alternative="",method="correlation type",conf.level=0.95)   #must be matrix
#or for mulitple variables
cor(无var1,无var2,use=无"strings",method=无"correlation type")   #instead of variables can be dataframe
rcorr(as.matrix(无data,type=无"correlation type"))
use: "everything","all.obs","complete.obs" #remove listwise,"pairwise.complete.obs"
methods: "pearson","spearman","kendall"
Alternatives: "less","greater",or not specify
#Calculate R^2
(cor(无var1,无var2,use=无"strings",method=无"correlation type"))^2

#Tidy data
tidyr::gather(无dataset,1999,2000,"year","cases")  #Make into wide format
tidyr::spread(columns_to_spread)  #Make into long format

#Save image
imageFile=file.path(imageDirectory,"无filename.png")
ggsave(imageFile)

#Calculate covarians and correlation coefficient
covarians = sum(无data1-mean(无data1))*sum(无data2-mean(无data2))/(N-1)
r = covarians / (sd(无data1)*sd(无data2))
r^2
cor.test(data1,data2)

#Make a scatterplot with linear regression
ggplot(无data,aes(无var1,无var))+
  geom_point()+
  geom_smooth(method="lm")

#Count all missing data
rowSums(cbind(is.na(无data$condition1,
                     无data$condition2,
                     无data$condition3)))

#Remove all observations with a set number of missing values
ifelse(rowSums(cbind(is.na(无data$condition1,
                            无data$condition2,
                            无data$condition3)))   #Count how many non-missing values, 1 per condition per observation
       <2,   #if non-missed values less than 2
       NA,   #then remove
       cbind(无data$condition1,   #if not, then use original data, but with missed values removed
              无data$condition2,
              无data$condition3),
       na.rm=TRUE)


#
ifelse(无condition,无ifTRUEthenwhat,无ifFALSEthenwhat)


#Make a Levene Test
leveneTest(无data$无outcomevar,无data$无groupvar)   #below .05 -> significance

#Make a grouped scatterplot
ggplot(无data,aes(无var1,无var2,colour=无splitbyvariable))+
  geom_point()+
  geom_smooth(method="lm",aes(fill=无splitbyvariable),alpha=0.1)+
  labs(x="无var1",y="无var2",colour="无splitbyvariable")

#Plot a barplot with errorbars
ggplot(无dataset,aes(无var1)+geom_bar(stat="summary",fun.y=mean)+geom_errorbar(stat="summary",fun.data=mean_se)

#Plot lines for more than two variables
ggplot(data,aes(x,y,colour=xyz))+geom_line()
        
#Change numerically annotated variable names into proper names
无data$无var=factor(无data$无var,levels=c(0:1),label=c("无level1","无level2"))

#Make Bar chart for independant variables with errorbars
bar=ggplot(无dataset,aes(无var1,无var2,fill=无splitbyvariable))
bar+stat_summary(fun.y=mean,geom="bar",position="dodge") + stat_summary(fun.data=mean_cl_normal,geom="errorbar",position=position_dodge(width=无number),width=无number),labs(x="无label1",y="无label2",fill="无splitbyvariable")
#Different method
bar=ggplot(无dataset,aes(无var1,无var2))
bar+stat_summary(fun.y=mean,geom="bar")+stat_summary(fun.data=mean_cl_normal,geom="errorbar",width=无number)+facet_wrap(~无splitbyvariable)+labs(x="var1",y="var2")

#Make histogram with frequency distribution
ggplot(无data,aes(无var))+
  geom_histogram(aes(y=..density..),colour="Black",fill="White")+
  stat_function(fun=dnorm,args=list(mean=mean(无data$var,na.rm=TRUE),sd=sd(无data$var,na.rm=TRUE)),colour="Blue",size=1)+
  labs(x="无var",y="Density")+
  theme(legend.position="none")
  

#Make qqplot
qqplot=qplot(sample=无data$var,stat="qq")

# Order dataset after variable
无dataset[order(无variable)]

#Make barplot with errorbars for different varaíabels
ggplot(无data,aes(无var1,无var2,fill=无var1))+
  geom_bar(stat="summary", fun.y=mean)+   #add barplot
  geom_errorbar(stat="summary",fun.data=mean_se,width=0.5)+   #add errorbars

#Remove simultaneous duplicates
cleandata=data[!duplicated(data[c("无column1","无column2")]),]

#Make an advanced summary
stat.desc(无data$无column)
#Make an advanced summary without basic
stat.desc(无data$无column,basic=F)
#Make an advanced summary without basic and with normerical data
stat.desc(无data$无column,basic=F,norm=T)

#Split one column into groups for the values of another column, and do an advanced summary
by(无data$无column1,无data$无column2,stat.desc,basic=F,norm=T)

#Plot a histogram
ggplot(无data,aes(x=无column))+geom_histogram(binwidth=几)