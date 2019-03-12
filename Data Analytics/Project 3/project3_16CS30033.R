data=read.csv("MACHINE.txt")

head(data)


ERP=data$ERP
PRP=data$PRP

mean_ERP=mean(ERP)
mean_ERP
mean_PRP=mean(PRP)

x=ERP-mean_ERP
y=PRP-mean_PRP


b=sum(x*y)/sum(x*x)
a=mean_PRP-b*mean_ERP

a
b

#Calculating R2

SSE=sum((PRP-(b*ERP+a))*(PRP-(b*ERP+a)))   # y = a+ bx  

SST=sum(y*y)

R2=1-(SSE/SST)

R2                                     # R2=0.9340675 close to 1 so very good fit

# Checking for non linear regression model

# for degree 1 ie a+bx already computer above
#Degree 1 computed again with the method lm 
x=ERP
y=PRP

plot(ERP,PRP,main="Plot of PRP vs ERP")
fit.model<-lm(y~x)

abline(fit)
summary(fit)
summary(fit)$r.squared


# For degree 2 y = a + b*x + c*x*x

#Let us take x2=x*x as another variable 
# Then it becomes multiple linear regression with y= a + b x + c x2


x2=x*x
fit2.model<-lm(y~x+x2)
summary(fit2)$r.squared    # R2 value calculated for non linear regression with degree 2 : 0.9355 

#Let us take x3=x*x*x as another variable 
# Then it becomes multiple linear regression with y= a + b x + c x2 + d x3

x3=ERP*ERP*ERP

plot(ERP,PRP,main="Plot of PRP vs ERP")

fit3.model<-lm(y~x+x2+x3)

pred<-predict(fit3.model)
lines(sort(x),y=pred[order(x)],col="red")

summary(fit3)

fit3.model$coefficients

summary(fit3)$r.squared

# R2 value calculated for non linear regression with degree 3 : 0.9355



