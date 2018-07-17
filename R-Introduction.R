# Quick intro to R by Alex Fine.  Presented on 3.3.15 at the Hebrew University.
# in R, you can comment out code like I'm doing right now by using the "#" sign.
# R as a calculator

2 + 2
2 * 5
2^10
log(2)

# variables in R.  Numbers (and other things besides) can be assigned to variables just like in other programming languages.  Variable names can be any character or string of characters, but it's good practice to avoid using any character (string) that names a function in R, like c(), t(), matrix(), etc.
a <- 2 + 2 
b <- a + 2
# also the arrow can go both ways.
2 * 5 -> ProductOfTwoAndFive
# but don't do stuff like this, even though it will work
2^4 -> c


# but the vast majority of everything you do in R will involve FUNCTIONS.  Functions in R look just like functions in math:  there's a name, followed by parentheses:

# plot()
# c()
# log()
# setwd()
# ...

# Functions in R--just like in math--take some ARGUMENT as input, and give you a specific type of output.  For instance:

# c() takes numbers, variables, character strings, etc. as arguments, and combines them into a vector.  
ages <- c(65, 66, 39, 43, 30)
# mean() does what you think it does:  it takes a vector of numbers and gives you the mean.  
mean(ages)

# functions are also recursive:

mean(c(65, 66, 39, 43, 30))

# and if you give it the wrong kind of argument, like a vector of characters, it won't work
letters <- c("a", "b", "c")
mean(letters) # see?

# In some sense, everything you will ever do in R will involve (1) figuring out which function you need to solve your specific problem(s), and (2) figuring out what kinds of arguments that function takes.  To figure out how a function works, you can use the documentation stored in R by typing a question mark before the function name, like this:

?setwd
?mean
?seq

# To figure out which function you need to start a given problem--sorry to state the obvious--you can simply google it.  Google searches can solve 99.99% of R-related problems.


# Where do these functions come from?  Many are part of what is called Base R--these are the core libraries and packages that come with R.  Others are stored in the Comprehensive R Archive Network (CRAN)--a collection of servers around the world that store identical, up-to-date versions of R and R packages.  When someone creates a package of functions and is ready to share it, they post it to CRAN.  Then you can get it with the install.packages() function (see below)

# to set the CRAN mirror, go to R -> Preferences.  Choose the one that is closest to you.

# also, you can write your own function!

myFunction <- function(x){
	c(mean(x), sd(x), min(x), max(x))
}

myFunction(ages) -> descriptiveStats

# or something even fancier:


myFunction2 <- function(x){
	matrix(myFunction(x), nrow = 2, ncol = 2)
}

familyMatrix <- myFunction2(ages)


# or something truly fantastic

myFunction3 <- function(x){
	write.table(myFunction2(x), file = "SummaryOfAges")
}

myFunction3(ages)

# this will save a table called "SummaryOfAges" to wherever your working directory is set to.  to find out where R thinks your working directory is, type getwd().  See below for information about how to CHANGE your working directory.  


# R stores essentially everything (deep down) as a vector.  That's why if you, say, type in the number 5, when it prints out the number there will be a little [1] next to it:  the way R really treats that 5 is as a vector of length 1 whose sole element is the number 5.  The concept of a vector will come up (in often subtle ways) again and again.  So let's start with learning about vectors in R.

# vectors
# here's a vector of numbers
c(13, 4)

# here's a vector of previously specified variables (see above)
c(a, b)
# here's a vector of characters.
c("a", "b")
# assign a vector to a variable v
v <- c(13, 4)
# do math on that vector.
2 * v
# use the colon to create a vector from 1 to 10.
c(1:10)
# use the seq() function to make vectors
seq(from = 1, to = 20, by = 2)
seq(1, 20, 2)

seq(1, 10, 2)
seq(0, 10, by = 2)
s <- seq(1, 10, 2)

# use rep() to make a vector of repeated numbers
rep(1, 2)
rep(x = 1, times = 2)
rep(x = 100, times = 10)
rep(x = "ariel", times = 10)
rep(s, 3)

sequence <- seq(from = 1, to = 20, by = 1)
sequence[2:5]
seq(from = 1, to = 20, by = 1)[2:5]

# Matrices:  often we want to represent information in 2 dimensions.  here's how you create a matrix in R.
# the function matrix() takes as arguments a vector, information about how many rows and columns you want, and how you want the vector to be "distributed" across the matrix (this is what byrow specifies)
matrix(c(1:12), ncol = 2, nrow = 6, byrow = T)



matrix(c(1:12), ncol = 2, nrow = 6)
matrix(c(1:12), ncol = 2, nrow = 6, byrow = T)
M <- matrix(c(1:12), ncol = 2, nrow = 6)

# Or, assign a vector a variable name, then turn it into a matrix

names <- c("alex", "ariel", "nate", "andres", "erika", "marcel")
M <- matrix(names, ncol = 2, nrow = 3)

# inspecting vectors and matrices
newVector <- M[,2]
newVector[2]
M[1, 2]
v[1]
M[1:2, 1]
M[1:2, 1:2]
# transpose a matrix
t(M)


sample(c(1:100), size = 20, replace = TRUE)

sample(c(1:100), size = 20, replace = TRUE)
RandomInt <- matrix(sample(c(1:100), size = 20, replace = TRUE), nrow = 4, ncol = 5)
RandomInt
t(RandomInt)

# loading external data
# install a package called "xlsx", which contains a bunch of functions that allow you to do stuff with excel sheets
install.packages("xlsx")
library(xlsx)
setwd("/Users/alexfine/Dropbox/Hebrew-Stats/Lecture1-3.3.15/")
# read in an excel file directly
d <-read.xlsx("Example_Data_Baayen.xlsx", sheetIndex = 1, header = TRUE)
# (sometimes this takes forever.  if it gets on your nerves just do the next step, below)

# you can also go to excel, and save the file as a tab-delimited text file and import it this way:
d <- read.table("Example_Data_Baayen.txt", header = T)


# other common functions for loading data (built into base R):
# read.table()
# read.csv()
# read.delim()

summary(d)
head(d)
# you can treat d just like (or almost just like) a matrix
d[1, 1]
d[, 1]
d[1, ]
d[1:4,]
d[, 1:4]

# notice that different columns in d contain different types of information
summary(d)
# numeric variables are given descriptive stats.  other variables are either characters or factors.  this will be an important distinction when we start doing analyses, because functions that do things like regression and anova will require factor variables

# how do we call a specific variable in a dataframe?  R makes our lives easier by giving dataframes special privileges over matrices: R knows that columns in dataframes are VARIABLES, and we can use the dollar sign "$" to specify  variables in a dataframe.

d$RTlexdec # this will just print out the column "RTlexdec" as a vector
# these vectors serve as arguments to lots of functions in R
summary(d$RTlexdec) # this will give descriptive stats for numeric variables
summary(d$WordCategory) # for factors, this will tell you how many instances there are of each level of the factor 

is.numeric(d$RTlexdec) # this will return a TRUE or FALSE, telling you whether RTlexdec is a numeric variable
is.factor(d$Familiarity)
is.character(d$Familiarity)
is.logical(d$Familiarity)
is.numeric(d$Familiarity)
is.data.frame(d)
# summarize the means etc. of a variable, conditioned on other variables


## summarize subsets of data.  say you want to know the mean lexical decision RT...

summary(d$RTlexdec)
as.data.frame(M) -> Mdata
summary(Mdata)

## ...when the word is a noun

summary(subset(d, WordCategory == "N")$RTlexdec) 
## or:

summary(d[d$WordCategory == "N",]$RTlexdec)


## what if you want to see means for nouns and verbs at once?  use by()  
by(d$RTlexdec, d$WordCategory, mean)


## what if you want to know the mean and standard deviation of lexical decision and word naming times for all combinations of word category and voicing?  use ddply()
install.packages("plyr")
library(plyr)
ddply(d, .(WordCategory, Voice), function(x) c(mean(x$RTlexdec), mean(x$RTnaming)))

### is this a data frame or a matrix?  

is.data.frame(ddply(d, .(WordCategory, Voice), function(x) c(mean(x$RTlexdec), mean(x$RTnaming))))

### so let's save it as a dataframe

Means <- ddply(d, .(WordCategory, Voice), function(x) c(mean(x$RTlexdec), mean(x$RTnaming)))
summary(Means)
### these variable names are totally opaque!  how do we fix that?  Hmm, a function that can change column names in a data frame... 
colnames(Means) <- c("WordCategory", "Voice", "RTlexdec", "RTnaming") 

## what if this is my experimental data, and I just want to do some sanity checks and see how many observations I have for each level of WordCategory?  create cross-tabulations of different variables

xtabs(~ d$WordCategory)

## broken down by word category and voice...

xtabs(~ d$WordCategory + d$Voice)



## compute basic correlations in R

cor(d$WrittenFrequency, d$RTlexdec)

## but this doesn't give a significance level, like you get with a pearson correlation coefficient.  How do we get that?  Different function.

cor.test(d$WrittenFrequency, d$RTlexdec)

## what if we want to know how this relationship changes depending on whether the word is a noun or a verb??  Well, I don't want to get into that too much, but that is basically a multiple regression question.  Just to give you a flavor of one basic way of asking that question using regression:

model <- lm(RTlexdec ~ WrittenFrequency * WordCategory, data = d)

summary(model)

# look at the third coefficient:  that tells you there is a significant interaction between frequency and word category.  You just plotted that interaction in the last ggplot graph above.  Tee-hee!

# what if we want to know if mean RTs on the lexical decision task are different for nouns and verbs?  Well, that's a t-test, man!

t.test(RTlexdec ~ WordCategory, data = d) # that's it!  If your data require a different type of t-test (paired samples, whatever) just google it!  The syntax will be _extremely_ similar.


# yeah but t-tests are boring.  I got more than one two-level variable!  Well, that's an ANOVA.  R can do ANOVA.

myANOVA <- aov(RTlexdec ~ WordCategory * Voice, data = d)
summary(myANOVA)

# looks like there are main effects of category and voicing but no interaction.  Guess which effects you plotted using boxplot() above?


# this seems like a bunch of disjointed commands, not really a "program".  I thought R was a programming language.  Well, it is...

# enter Cmd + N to create a new script and create a script there that:

# 1.  reads in the dataframe above
# 2.  plots and saves boxplot of lexical decision RTs, broken down by wordcategory
# 3.  creates a subsetted dataframe with only the nouns

# after you've created that script, and saved it in your working directory as "SampleScript.R", run it from here.

source("SampleScript.R")






