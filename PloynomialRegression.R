#------------------Polynomial Regression------------------

install.packages("ggplot2")
library("ggplot2")
a=c(21,19,39,50,60,55,78,69,90,110,111,120,130,141)

b = c(32,46,38,47,40,48,67,50,40,52,65,74,85,79)

f = data.frame(a,b)

ggplot(f, aes(y=b, x=a)) +
  geom_point(alpha = .9) +
  stat_smooth(method = "lm", formula = y ~ I(x^2))
