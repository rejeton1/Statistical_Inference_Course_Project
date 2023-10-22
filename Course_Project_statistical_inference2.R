#load needed package
library(dplyr)

data <- ToothGrowth
head(data)
dim(data)
str(data)
unique(data$supp)

##Perform some basic exploratory data analysis table/plot

# len : Tooth length
# supp : Supplement type (VC or OJ)
# dose : Dose in milligrams

with(data, table(supp, dose))

group <- data %>% group_by(supp, dose) %>% summarize(mean=mean(len))

group


##plotting it

boxplot(len~supp, data, xlab='Supplement Type', ylab='Tooth length')
boxplot(len~dose, data, xlab='Dose(milligrams/day)', ylab='Tooth length')


##Test it
##First, test the difference by type
##we will perform independent t test with common variace
##(should check assuming!(iid normal, common variance))
halfOJ <- with(data, data[supp=='OJ'&dose==0.5,1])
halfVC <- with(data, data[supp=='VC'&dose==0.5,1])
t.test(halfOJ, halfVC, paired = FALSE, var.equal = TRUE)
##rejected

oneOJ <- with(data, data[supp=='OJ'&dose==1,1])
oneVC <- with(data, data[supp=='VC'&dose==1,1])
t.test(oneOJ, oneVC, paired = FALSE, var.equal = TRUE)
##rejected


twoOJ <- with(data, data[supp=='OJ'&dose==2,1])
twoVC <- with(data, data[supp=='VC'&dose==2,1])
t.test(twoOJ, twoVC, paired = FALSE, var.equal = TRUE)
##fail to rejected

##test by the amount of dose
t.test(halfOJ, oneOJ, paired = FALSE, var.equal = TRUE)
t.test(oneOJ, twoOJ, paired = FALSE, var.equal = TRUE)

t.test(halfVC, oneVC, paired = FALSE, var.equal = TRUE)
t.test(oneVC, twoVC, paired = FALSE, var.equal = TRUE)
##all rejected

