schedprobb
plot(schedprobb, xlab="Time", ylab="Cost")
as.data.frame((schedprobb[[3]])$value)
as.data.frame((schedprobb[[3]])$pareto.optimal)
daf<-cbind(as.data.frame((schedprobb[[3]])$value),as.data.frame((schedprobb[[3]])$pareto.optimal))
colnames(daf)<-c("Time", "Cost", "Pareto")


#create a list
daff<-list()
#append the cost and time matrices to the list
for (k in 1:(length(schedprobb))) {
  output = (schedprobb[[k]])$value
  daff[[length(daff) + 1]] = output
}
#rbind the entire list
daffy<-do.call("rbind", daff)
daffy

#create a vector
veccy<-c()
#append the pareto optimality to the vector
for (k in 1:(length(schedprobb))) {
  veccy<-append(veccy,(schedprobb[[k]])$pareto.optimal)          
}
veccy

numgen<-4

#create a vector
vexx<-c()
#append the generation position to the vector
for (k in 1:(length(schedprobb))) {
  vexx<-append(vexx,c(rep(k,numgen)))          
}
vexx


