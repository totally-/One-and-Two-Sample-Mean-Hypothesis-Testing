library(BSDA)
library(TeachingDemos)

x = BONES$Bones
x


#_______________ INPUT BY THE USER ______________________________________________

null_hypothesis = "the population mean ratio of all bones of this particular species is 8.5"
alternative_hypothesis = "the population mean ratio of all bones of this particular species differs from 8.5"

popMean = 8.5 # BONES
n = nrow(BONES) # BONES
alpha = 0.01 # BONES
alternative = "two.sided" # two.sided, less, or greater. 


#________________________________________________________________________________
pVal = NULL

CONCLUSION = "Conclusion: "
FAIL_TO_REJECT = "Fail to reject the null hypothesis."
REJECT = "Reject the null hypothesis."

print(paste("null hypothesis: ", null_hypothesis))
print(paste("alternative hypothesis: ", alternative_hypothesis))
print("---------------------------------------------")

shapiro.test(x)

print(shapiro.test(x))

shapPvalue = shapiro.test(x)$p.value

if (shapPvalue > 0.05) { # if data set is normal
  print(paste(shapPvalue, " > 0.05 so the dataset is normal"))
  print(paste("n = ",n))
  if (n >= 30){
    print(paste(n," >= 30 Use the z.test"))
    pVal = z.test(x, mu = popMean, stdev = sd(x), alternative = alternative)$p.value
    print(z.test(x, mu = popMean, stdev = sd(x), alternative = alternative))
    print(paste("p-value of z.test = ", pVal))
  } else { # if n < 30
    print(paste(n," < 30 Use the t.test"))
    pVal = t.test(x, mu = popMean, alternative = alternative)$p.value
    print(t.test(x, mu = popMean, alternative = alternative))
    print(paste("p-value of t.test = ", pVal))
  }
} else { # if data set is not normal
  print(paste(shapPvalue, " <= 0.05 so the dataset is NOT normal"))
  print(paste("Use the SIGN.test"))
  pVal = SIGN.test(x, md = popMean, alternative = alternative)$p.value
  print(SIGN.test(x, md = popMean, alternative = alternative))
  print(paste("p-value of SIGN.test = ", pVal))
  
}


print(paste("alpha = ", alpha))
if(pVal > alpha){
  print(paste(pVal, " > ", alpha))
  print(FAIL_TO_REJECT)
  print(paste(CONCLUSION, null_hypothesis))
  
} else {
  print(paste(pVal," <= ", alpha))
  print(REJECT)
  print(paste(CONCLUSION, alternative_hypothesis))
}
