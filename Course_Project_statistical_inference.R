##load needed package
library(ggplot2)
library(gridExtra)

##plotting exponential distribution with rate 0.2
ggplot(data.frame(x=c(0,100)), aes(x=x)) +
  stat_function(fun=dexp, args=list(rate=0.2), colour="brown", linewidth =1.5) +
  ggtitle("Exponential Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

##theoretical average of sample mean of size 40
##By CLT, the distribution of sample means approximates a normal distribution (i.e., a “bell curve”) 
##as the sample size becomes larger, regardless of the population's actual distribution shape.
##the mean of population is 5

##make a thousand of random exponential sample of size 40

n=40
B=1000

set.seed(123)
randomexp <- rexp(n*B, rate = 0.2)
samples40 <- matrix(randomexp, nrow=B, ncol = n)

samplemeans <- apply(sample, MARGIN = 1, mean)

aver_sam_means <- mean(samplemeans)

aver_sam_means

##What is theoretical variance of the distribution of sample mean?

theo_sam_var <- 5^2/n
theo_sam_var

##What is the variance of a thousand of sample means?

var_sam_means <- sd(samplemeans)
var_sam_means

## the distribution of a large collection of random exponentials VS 
## the distribution of a large collection of averages of 40 exponentials.

samplemeans <- data.frame(x=samplemeans)

samplepoints <- data.frame(x=rexp(1000, rate=0.2))

p <- ggplot(samplemeans, aes(x=x)) + 
  geom_histogram(aes(y=..density..), binwidth=0.1, color='black') +
  geom_density(alpha=0.3, fill='red') +
  geom_vline(aes(xintercept=mean(x)), color='blue', linetype='dashed', size=1)

q <- ggplot(samplepoints, aes(x=x)) +
  geom_histogram(aes(y=..density..), binwidth=0.4, color='black') +
  geom_density(alpha=0.3, fill='red') +
  geom_vline(aes(xintercept=mean(x)), color='blue', linetype='dashed', size=1)

grid.arrange(q, p, ncol=2)


##the distribution of averages of 40 exponentials is approximating a normal
##distribution in regardless of the distribution of 
##a large collection of random exponentials that is not 
##approximating normal distribution.
