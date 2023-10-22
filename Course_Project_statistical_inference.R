##load needed package
library(ggplot2)

##plotting exponential distribution with rate 0.2
ggplot(data.frame(x=c(0,100)), aes(x=x)) +
  stat_function(fun=dexp, args=list(rate=0.2), colour="brown", linewidth =1.5) +
  ggtitle("Exponential Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

##theoretical average of sample mean of size 40
##By CLT, the distribution of sample means approximates a normal distribution (i.e., a “bell curve”) 
##as the sample size becomes larger, regardless of the population's actual distribution shape.
##the mean of population is