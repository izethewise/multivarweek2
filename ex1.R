library(foreign)
amino = read.spss("amino.sav", to.data.frame=TRUE)
# Exercise 1. The file amino.sav on Studentcentral contains the results of an experiment
# comparing the effects of three amino acids (carnosine, histodine and imidazole) on the
# concentration of malondialdehyde (MDA). For these data, perform ANOVA, pairwise
# comparisons and Kruskal-Wallis test and assess whether the ANOVA assumptions are
# satisfied.
library(ggplot2)
# Boxplot for visual check of data
ggplot(data=amino) +  aes(reorder(AMINO,`MDA`),`MDA`) + geom_boxplot() 
# Use aov to show whether there are differences for each factor
fit <- aov(MDA~AMINO, data=amino)
summary(fit)
# Pr(>F) 0.000363

# Do pairwise T tests
pairwise.t.test(amino$MDA,amino$AMINO,p.adj="none",pool.sd=FALSE)

# Do Kruskal-Wallis
kruskal.test(MDA~AMINO,data=amino)
#p-value = 0.0087
