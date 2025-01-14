library(lme4)    #载入需要的程序包lme4
mydata<-read.csv(file.choose())     #选择导入处理好的数据文件（一般为csv格式）,并且将数据命名为mydata
head(mydata)    #查看导入数据
str(mydata)    #查看数据格式，在建模之前需要考虑的因素应（编号，重复，环境及年份等）转化为因子（Factor），要分析的性状数据要转化为数值（num）
for(i in 1:3) mydata[,i] = as.factor(mydata[,i])    #将前n个因素转化为因子（Facter），n在具体操作室应换成具体数值
for(i in 4:12) mydata[,i] = as.numeric(mydata[,i]) 
str(mydata)    #再次查看数据格式，观察是否转化完全
library(lmerTest)    #载入需要的程序包lmerTest
mod1 = lmer(PH ~ (1|lines) + (1|rep) + (1|env) + 
              (1|lines:rep) + (1|lines:env) , data=mydata)
summary(mod1)    #显示建模结果,H=Vg/(Vg+Vge/L+Ve/RL),Vg:Variance_lines;Vge:Varance_lines:env;Ve:Varance_Residual;L:num of env;R:num of rep
mod2 = lmer(LMS ~ (1|lines) + (1|rep) + (1|env) + 
              (1|lines:rep) + (1|lines:env) , data=mydata)
summary(mod2)
mod3 = lmer(NLS ~ (1|lines) + (1|rep) + (1|env) + 
              (1|lines:rep) + (1|lines:env) , data=mydata)
summary(mod3)
mod4 = lmer(SLS ~ (1|lines) + (1|rep) + (1|env) + 
              (1|lines:rep) + (1|lines:env) , data=mydata)
summary(mod4)
mod5 = lmer(SIL ~ (1|lines) + (1|rep) + (1|env) + 
              (1|lines:rep) + (1|lines:env) , data=mydata)
summary(mod5)
mod6 = lmer(TSL ~ (1|lines) + (1|rep) + (1|env) + 
              (1|lines:rep) + (1|lines:env) , data=mydata)
summary(mod6)
mod7 = lmer(FIL ~ (1|lines) + (1|rep) + (1|env) + 
              (1|lines:rep) + (1|lines:env) , data=mydata)
summary(mod7)
mod8 = lmer(NT ~ (1|lines) + (1|rep) + (1|env) + 
              (1|lines:rep) + (1|lines:env) , data=mydata)
summary(mod8)
mod9 = lmer(NNMS ~ (1|lines) + (1|rep) + (1|env) + 
              (1|lines:rep) + (1|lines:env) , data=mydata)
summary(mod9)