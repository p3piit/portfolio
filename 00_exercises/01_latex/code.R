#load package lattice
library(lattice)
library(xtable) # generate the LaTeX code for tables
#fix the random generator seed
set.seed(123)
#create data
data <- rnorm(1000)
#plot histogram
histogram(data)
#plot density 
densityplot(data^12 / data^10, xlab = expression(data^12/data^10))
#plot stripplot
stripplot(data^2, xlab = expression(data^2))
#plot boxplot
bwplot(exp(data))
#matrix with all data used
data.all <- cbind(data = data, 
                  squared1 = data^12 / data^10,
                  squared2 = data^2,
                  exponent = exp(data))
write.csv(data.all, "/home/paoloc/Documenti/Utrecht/uni/Mlarpis/ex_16/data.csv")
