# To print just print statements to the console, select all the code and press
# ctrl + shift + S
# https://stackoverflow.com/questions/35293735/prevent-rstudio-console-from-showing-script-commands
# https://stackoverflow.com/questions/31205554/output-p-value-from-a-t-test-in-r
# how to comment out code: Ctrl + Shift + C.
# how to indent code: Ctrl + Shift + i.

library(BSDA)
library(TeachingDemos)

x = SLI$DIQ
x

y = YND$DIQ
y


#_______________ INPUT BY THE USER ______________________________________________

null_hypothesis = "There is NO difference in the price of business textbooks between the two stores."
alternative_hypothesis = "There is a difference in the price of business textbooks between the two stores."

alpha = 0.01
mu = 0 
alternative = "two.sided" # two.sided, less, or greater
paired = FALSE # input by the user (TRUE or FALSE)

#________________________________________________________________________________
pVal = NULL

CONCLUSION = "Conclusion: "
FAIL_TO_REJECT = "Fail to reject the null hypothesis."
REJECT = "Reject the null hypothesis."

print(paste("null hypothesis: ", null_hypothesis))
print(paste("alternative hypothesis: ", alternative_hypothesis))
print("---------------------------------------------")

xPshapVal <- shapiro.test(x)$p.value
yPshapVal <- shapiro.test(y)$p.value

shapiro.test(x)
shapiro.test(y)

print(shapiro.test(x))
print(shapiro.test(y))


print(paste("p-value of shapiro.test(x)", xPshapVal))
print(paste("p-value of shapiro.test(y)", yPshapVal))

if (xPshapVal > 0.05 && yPshapVal > 0.05) {
  print(paste(xPshapVal, "> 0.05 AND ", yPshapVal, " > 0.05", sep = " "))
  print("Both of the data sets are normal.")
  print("---------------------------------------------")
  if (paired == TRUE){
    print(t.test(x,y,alternative = alternative, mu = mu, paired = TRUE))
    pVal <- t.test(x,y,alternative = alternative, mu = mu, paired = TRUE)$p.value
    print(paste("p-value of paired t.test(x,y): ", pVal))
    
  } 
  else{ # if both normal and not paired
    print(var.test(x,y))
    print(paste("p-value of 2-Sample var.test(x,y): ", var.test(x,y)$p.value))
    if(var.test(x,y)$p.value > 0.05){
      print(paste(var.test(x,y)$p.value, " > 0.05"))
      print(t.test(x,y,alternative = alternative, mu = mu, var.equal = TRUE))
      pVal <- t.test(x,y,alternative = alternative, mu = mu, var.equal = TRUE)$p.value
      print(paste("p-value of pooled t.test(x,y): ", pVal))
      
    } 
    else{
      print(paste(var.test(x,y)$p.value, " <= 0.05"))
      print(t.test(x,y,alternative = alternative, mu = mu, paired = FALSE))
      pVal <- t.test(x,y,alternative = alternative, mu = mu, paired = FALSE)$p.value
      print(paste("p-value of non-pooled t.test(x,y): ", pVal))
      print(paste(pVal, " <= 0.05"))
    } 
    print("---------------------------------------------")
  } 
} else { # if one or both data sets are NOT normal 
  if(xPshapVal <= 0.05 && yPshapVal > 0.05){
    print(paste(xPshapVal, " <= 0.05 AND ", yPshapVal, " > 0.05", sep = " "))
  }
  if(xPshapVal > 0.05 && yPshapVal <= 0.05){
    print(paste(xPshapVal, " > 0.05 AND ", yPshapVal, " <= 0.05", sep = " "))
  }
  if(xPshapVal >= 0.05 && yPshapVal < 0.05){
    print(paste(xPshapVal, " >= 0.05 AND ", yPshapVal, " < 0.05", sep = " "))
  }
  if(xPshapVal < 0.05 && yPshapVal >= 0.05){
    print(paste(xPshapVal, " < 0.05 AND ", yPshapVal, " >= 0.05", sep = " "))
  }
  if(xPshapVal <= 0.05 && yPshapVal <= 0.05){
    print(paste(xPshapVal, " <= 0.05 AND ", yPshapVal, " <= 0.05", sep = " "))
  }
  print("One or both of the data sets are NOT normal.")
  print("---------------------------------")
  if(paired == TRUE){ 
    print(wilcox.test(x,y,alternative = alternative,mu = mu, paired = TRUE))
    pVal <- wilcox.test(x,y,alternative = alternative,mu = mu, paired = TRUE)$p.value
    print(paste("p-value of paired wilcox.test(x,y): ", pVal))
  } 
  else{
    print(wilcox.test(x,y,alternative = alternative,mu = mu))
    pVal <- wilcox.test(x,y,alternative = alternative,mu = mu)$p.value
    print(paste("p-value of 2-Sample (non-paired) wilcox.test(x,y): ", pVal))
  } 
} 
print(paste("alpha: ", alpha))




if(pVal > alpha){
  print(paste(pVal, " > ", alpha))
  print(FAIL_TO_REJECT)
  print(paste(CONCLUSION, null_hypothesis))
  
} else {
  print(paste(pVal," <= ", alpha))
  print(REJECT)
  print(paste(CONCLUSION, alternative_hypothesis))
}
