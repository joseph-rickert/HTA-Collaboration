
library(devtools)
install_github("rmhorton/TurtleROC")

library(TurtleROC)
help(package="TurtleROC")
data <- data.frame(score=(1:10)/10, actual=rep(c(1, 0), 5))
TurtlePathWidget(data, width = 600, height = 600)
