library(foreign)
tablets = read.spss("tablets.sav", to.data.frame=TRUE)
# Get rid of NAs
tablets <- na.omit(tablets)

# Save copy/paste
showstats <- function(data){
  # Anova
  fit <- aov(TIME~COMPRESS, data=data)
  a <- summary(fit)
  # Do pairwise T tests
  b <- pairwise.t.test(data$TIME,data$COMPRESS,p.adj="none",pool.sd=FALSE)
  # Do Kruskal-Wallis
  c <- kruskal.test(TIME~COMPRESS,data=data)
  print("anova")
  print(a)
  print("pairwise")
  print(b)
  print("kruskal")
  print(c)
}

# Do boxplot
ggplot(data=tablets) + aes(reorder(MS,TIME),TIME) + geom_boxplot() + facet_wrap(~COMPRESS) 
ggplot(data=tablets) + aes(reorder(COMPRESS,TIME),TIME) + geom_boxplot() + facet_wrap(~MS) 
ggplot(data=tablets) + aes(reorder(COMPRESS,TIME),TIME) + geom_boxplot() 

# Exercise 2. For the experiment data in tablets.sav (Exercise 2 in Week 1), perform
# ANOVA, pairwise comparisons and Kruskal-Wallis test to investigate how the disintegration
# times depend on the compression force in the following three cases:
# 1. the concentration of magnesium stearate is 0.5%;
showstats(tablets[tablets$MS==0.5,])
# 2. the concentration of magnesium stearate is 1%;
showstats(tablets[tablets$MS==1.0,])
# 3. the concentration of magnesium stearate is ignored (effectively, included into the
showstats(tablets)
# Assess whether the ANOVA assumptions are satisfied in each of the three cases.
# Anova is satisfied in all cases as each test shows p<0.05

