dl.tm<-read.csv("delivery_time (1).csv")
dt.st<-dl.tm
View(dt.st)
#21 observations of two variables

#scatter diagram (plotx,y)
plot(dt.st$Sorting.Time,dt.st$Delivery.Time)
 
#Exploratory data analysis and plots
boxplot(dt.st)

hist(dt.st$Sorting.Time)
hist(dt.st$Delivery.Time)
summary(dt.st)
#correlation coefficient value for delivery time and sorting time
dt<-dt.st$Delivery.Time
st<-dt.st$Sorting.Time
cor(st,dt)
# if r is greater than 0.8 then the co-relation is strong
#simple model without using any transformation
reg<-lm(dt~st)
summary(reg)
# the probability value should be less than 0.5,r squared value is 0.6655 which is less than 0.8
confint(reg,level=0.95)   #confidence interval
# the above code will give you two equations (one to calculate lower range and one to calculate upper)
#function to predict the above model
predict(reg,interval="predict")
#we may have to do transformations for better R-squared value, applying transformations
# logarithmic transformations
reg_log<-lm(dt~log(st)) # regression using logarithmic transformatins
summary(reg_log)
confint(reg_log,level=0.95)
predict(reg,interval="predict")
reg_exp<-lm(log(dt)~st) #regression using Exponential model
summary(reg_exp)
confint(reg_exp,level=0.95)
exp(predict(reg_exp,interval="predict"))
# Higher the R squared value- better the chances of getting good model
# for delivery time and sorting time
# Quadratic  model
dt.st[,"st_sq"]=st*st
quad.mod<-lm(dt~st+I(st^2),data=dt.st)
summary(quad.mod)
confint(quad.mod,level=0.95)
predict(quad.mod,interval='predict')
# adjusted R-squared=0.6594,multiple R-squared=0.6934
#Cubic model
poly_mod<-lm(dt~st+I(st^2)+I(st^3),data=dt.st)
summary(poly_mod)
confint(poly_mod,level=0.95)
predict(poly_mod,interval "predict")
# Adjusted R suared value=0.6511, multiple R squared value=0.7034
model_R_squared_values<-list(model=NULL,R_squared=NULL)
model_R_squared_values[["model"]]<-c("reg","reg_log","reg_exp","quad_mod","poly_mod")
model_R_squared_values[["R_squared"]]<-c(0.6655,0.6794,0.6957,0.6594,0.6511)
Final<-(model_R_squared_values)
View(model_R_squared_values)
View(Final)

#Exponential model gives the best Adjusted R-squared value
predicted_value<-exp(predict(reg_exp))
predicted_value

Final<-cbind(Sorting_Time=dt.st$Sorting.Time,Delivery_Time=dt.st$Delivery.time,Predicted_Delivery_time=predicted_value)
View(Final)
rmse<-sqrt(mean(predicted_value-dt)^2)
rmse
plot(reg_exp)



hist(residuals(reg_exp))   #close to Normal distribution












