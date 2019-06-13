# Submitted by: Laura Neff and Zachary Hong
# Math 117

#### define sorting functions
insertion.sort = function(x) {
  for (j in 2:length(x)) {
    key = x[j] 
    i = j - 1 
    while (i > 0 && x[i] > key) {
      x[(i + 1)] = x[i]
      i = i - 1 
    }
    x[(i + 1)] = key
  }
  return(x)
} 

selection.sort = function(x) {
  lenx <- length(x)
  for(i in seq_along(x))
  {
    mini = (i - 1) + which.min(x[i:lenx])
    start_ = seq_len(i-1)
    x <- c(x[start_], x[mini], x[-c(start_, mini)])
  }
  return(x)
}

quick.sort = function(x) {
  mid = sample(x, 1)
  left = c()
  right = c()
  
  lapply(x[x != mid], function(d) {
    if (d < mid) {left <<- c(left, d) } 
    else { right <<- c(right, d) }
  })
  
  if (length(left) > 1) { left = quick.sort(left) }
  if (length(right) > 1) { right = quick.sort(right) }
  return(c(left, mid, right))
}
#### define timing function
sort.time = function(size,times.each,sorting.method,replicates = 3){
  sum_times = 0
  for(r in 1:replicates){
    vector = sample(1:size)
    ## timing sort method here
    start_time = Sys.time()
    for(time in 1:times.each){
      sorting.method(vector)
    }
    end_time = Sys.time()
    sum_times = sum_times + end_time-start_time
  }
  return(sum_times/replicates)
}

#### check if sorting algorithms take the same time or not
  # using: one-way ANOVA test
  # assumptions: 
  #   1. data are independent and random - true by design
  #   2. data are normally distributed
  #   3. normal populations have a common variance

## generate input data
insertion.sample = replicate(10,expr = sort.time(10,100,insertion.sort))
selection.sample = replicate(10,expr = sort.time(10,100,selection.sort))
quick.sample = replicate(10,expr = sort.time(10,100,quick.sort))
combined_data = data.frame(c(insertion.sample,selection.sample,quick.sample))
colnames(combined_data) = "time"
combined_data[,"method"] = c(rep("insertion",10),
                             rep("selection",10),
                             rep("quick",10))

## assumption #2:: test each group is roughly normally distibuted
ks.test(insertion.sample,"pnorm",mean(insertion.sample),sd(insertion.sample)) #p-value = 0.3676
ks.test(selection.sample,"pnorm",mean(selection.sample),sd(selection.sample)) #p-value = 0.9603
ks.test(quick.sample,"pnorm",mean(quick.sample),sd(quick.sample)) #p-value = 0.5217
qqnorm(insertion.sample,main ="QQ for insertion sort") #normal
qqline(insertion.sample,
       col = "red")
qqnorm(selection.sample,main ="QQ for selection sort") #normal
qqline(selection.sample,
       col = "red")
qqnorm(quick.sample,main ="QQ for quick sort") #normal
qqline(quick.sample,
       col = "red")

## assumption #3:: test all groups share common variance - from http://www.sthda.com/english/wiki/compare-multiple-sample-variances-in-r
car::leveneTest(time ~ method, combined_data) #variance not significantly different
  # Levene's Test for Homogeneity of Variance (center = median)
  #        Df  F value  Pr(>F)
  # group  2   0.5356   0.5914
  #        27  

## assumptions met:: perform ANOVA test
result = aov(time ~ method,data=combined_data)
summary(result) # yes, there is a significant difference in the runtimes!
  # a very significant difference (<2e-16)
  #            Df    Sum Sq   Mean Sq F value Pr(>F)    
  #  method       2 2.767e-04 1.384e-04     560 <2e-16 ***
  #  Residuals   27 6.670e-06 2.500e-07                   
  #  ---
  #  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# which runtime is different?
boxplot(time~method,data=combined_data, main="Sorting methods", 
        xlab="Sorting method", ylab="Time for 100 iterations")
# results:
# insertion sort is fastest (?!?)
# selection sort is second fastest
# quick sort is slowest (?!?!?!?)

#is insertion sort faster than quick sort? ANSWER: YES (for this test)
t.test(insertion.sample,quick.sample,alternative = "less")$p.value # = 5.963089e-07
#is insertion sort faster than selection sort? ANSWER: YES (for this test)
t.test(insertion.sample,selection.sample,alternative = "less")$p.value # = 1.237726e-18
# is selection sort faster than quick sort? ANSWER: YES (for this test)
t.test(selection.sample,quick.sample,alternative = "less")$p.value # = 3.346549e-06

#### check if runtime increases linearly with array size for insertion and selection sort
library(car) #includes an outlier test for linear models

insertion.proportional =sapply(seq(10,800,10),function(x){sort.time(x,10,insertion.sort)})
selection.proportional =sapply(seq(10,800,10),function(x){sort.time(x,10,selection.sort)})
insertion_df = data.frame(length=seq(10,800,10),time=insertion.proportional)
selection_df = data.frame(length=seq(10,800,10),time=selection.proportional)
insertion_lm = lm(time ~ length,insertion_df)
selection_lm = lm(time ~ length,selection_df)

#plot insertion sort, check fit, remove outliers, replot
plot(insertion_df,main="Insertion sort with outliers")
abline(insertion_lm,col="red")
insertion_dist = cooks.distance(insertion_lm) #from http://r-statistics.co/Outlier-Treatment-With-R.html
ins_outlier = as.numeric(names(insertion_dist)[(insertion_dist > 4*mean(insertion_dist, na.rm=T))])
insertion_df = insertion_df[-ins_outlier,]
insertion_lm = lm(time ~ length,insertion_df)
plot(insertion_df,main="Insertion sort without outliers")
abline(insertion_lm,col="red")

#plot selection sort, check fit, remove outliers, replot
plot(seq(10,800,10),selection.proportional,main="Selection sort with outliers")
abline(selection_lm,col="red")
selection_dist = cooks.distance(selection_lm) #from http://r-statistics.co/Outlier-Treatment-With-R.html
sel_outlier = as.numeric(names(selection_dist)[(selection_dist > 4*mean(selection_dist, na.rm=T))])
selection_df = selection_df[-sel_outlier,]
selection_lm = lm(time ~ length,selection_df)
plot(selection_df,main="Selection sort without outliers")
abline(selection_lm,col="red")

# just from visual inspection, and what we know, it seems the runtime increases by n^2
# let's try to fix that
insertion_df["time"] = sqrt(insertion_df$time) #take the sqrt() of it since O(n2)
selection_df["time"] = sqrt(selection_df$time) #take the sqrt() of it since O(n2)

insertion_lm = lm(time ~ length,insertion_df)
plot(insertion_df,main="Insertion sort sqrt() time",ylab="sqrt(time)") 
abline(insertion_lm,col="red") #this correlation is practically perfect

selection_lm = lm(time ~ length,selection_df)
plot(selection_df,main="Selection sort sqrt() time",ylab="sqrt(time)")
abline(insertion_lm,col="red") #better, but still could use some work. its as close as it's gonna get

summary(insertion_lm) #yes, there is a significant correlation
# Call:
# lm(formula = time ~ length, data = insertion_df)
# 
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -0.0106008 -0.0031308 -0.0009799  0.0016209  0.0289465 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 2.518e-03  1.319e-03   1.909     0.06 .  
# length      5.901e-04  2.829e-06 208.568   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.005843 on 78 degrees of freedom
# Multiple R-squared:  0.9982,	Adjusted R-squared:  0.9982 
# F-statistic: 4.35e+04 on 1 and 78 DF,  p-value: < 2.2e-16

summary(selection_lm) #yes, there is a significant correlation
# Call:
# lm(formula = time ~ length, data = selection_df)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.032935 -0.011203 -0.001969  0.003915  0.067668 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 3.836e-02  4.390e-03   8.737 3.49e-13 ***
#   length      4.854e-04  9.417e-06  51.542  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.01945 on 78 degrees of freedom
# Multiple R-squared:  0.9715,	Adjusted R-squared:  0.9711 
# F-statistic:  2657 on 1 and 78 DF,  p-value: < 2.2e-16

#### predict runtime for insertion sort @ n=1000
new <- data.frame(length = seq(1000,1000,1))
insertion_prediction = predict(insertion_lm,new,se.fit=T)

pred_val = round(insertion_prediction$fit^2,5)
conf_interval = round(1.96*insertion_prediction$se.fit,5)
print(paste0("predicted insertion sort for n=1000: ", pred_val))
print(paste0("confidence interval: (",pred_val-conf_interval,",",pred_val+conf_interval,")"))

real_time = sort.time(1000,10,insertion.sort)
print(paste0("real time for n=1000: ", real_time)) #0.3668"

inacc = round((real_time-pred_val)/pred_val*100,5)

print(paste0("prediction off by: ",inacc,"%")) #8.06446%"

