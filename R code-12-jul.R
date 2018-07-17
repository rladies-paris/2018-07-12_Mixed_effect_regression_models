####################################################################
###################### NAOMI PRESENTATION CODE #####################
####################################################################

##At the begining of every session you upload the packages you are going to use
library(languageR)
library(lme4)
library(psych) 
library (lmerTest)
library (doBy)
library (ggplot2)
library(dplyr)
library(tidyr)
library(boot)
library(lattice)
devtools::install_github("dmbates/RePsychLing")
library(RePsychLing)
library(afex)

##first set directory to the one containing the file
setwd("C:/Users/naomi/Google Drive/נעמי/post doc/MEM workshop")

#now read in the file to an object called "d"
d <- read.csv("lexicalpriming.csv", header=T, na.strings = "NULL", comment.char="") 

##what does the data look like?
summary(d)


##some variables are coded wrong, let's fix that!
d$wordness<-as.factor(d$wordness)

## faster to do all at once:
d[,c("prime","subject", "item", "luminance")] <- lapply(d[,c("prime","subject", "item", "luminance")] , factor)

##random intercepts

MEM1 <- lmer(RT ~ wordness + (1|subject)+ (1|item), data = d, control=lmerControl(optimizer="bobyqa"))
summary (MEM1)
ranef(MEM1)
qqmath(ranef(MEM1))
################### -> back to slides

##random slopes
MEM1<- lmer(RT ~ wordness + (1+wordness|subject)+ (1|item), data = d, control=lmerControl(optimizer="bobyqa"))
summary (MEM1)
ranef(MEM1)

##model comparison
MEM1<- lmer(RT ~ wordness + (1+wordness|subject)+ (1|item), data = d, control=lmerControl(optimizer="bobyqa"))
MEM2<- lmer(RT ~ wordness + (1|subject)+ (1|item), data = d, control=lmerControl(optimizer="bobyqa"))


anova(MEM2,MEM1)

### but we usually use it to test our fixed effects:
MEM1<- lmer(RT ~ wordness + (1+wordness|subject)+ (1|item), data = d, control=lmerControl(optimizer="bobyqa"))
MEM2<- lmer(RT ~  (1+wordness|subject)+ (1|item), data = d, control=lmerControl(optimizer="bobyqa"))

anova(MEM2,MEM1)

###### -> back to presentation!

##model does not converge
MEM1<- lmer(RT ~ wordness + (1+wordness|subject)+ (1+prime+luminance|item), data = d, control=lmerControl(optimizer="bobyqa"))
summary (MEM1)

MEM1<- lmer(RT ~ wordness + (1+wordness|subject)+ (1+prime|item), data = d, control=lmerControl(optimizer="bobyqa"))
summary (MEM1)


############################
# Parsimonious mixed models - code from Alexandre Cremers, run it to create the function "parsimonious"
############################

# Implements a basic version of Bates et al (2015) parsimonious models
# Expects models with maximal RE structure (in the sense of Barr et al (2013)) and does the following:
# 	1) runs PCA on each RE structure,
# 	2) eliminates components with variance below 'tol', relative to the main component,
# 	3) checks that no remaining component is under 'tol' (the updated model does not have correlations between RE components), otherwise eliminates them.

# The function seems to work well even when the input model had not converged.

# The function should work with glmer as well (only tested with family=binomial(link="logit") though)

# By default, the output is not comparable with the original model because they are not fitted on the same data (i.e., anova(model, parsimonious(model)) will return an error). Use option "comparable=TRUE" to force the new model to be comparable with the original model. This will define the factors used in the new RE structures locally. Note that by doing so, the new model will not be comparable with models obtained by further update. If you wish to compare the new model to further updated models but set the option comparable to TRUE, you must refit the new model on its own model.frame first (i.e., run "update(new_model,data=model.frame(new_model))").

parsimonious<-function(mod,tol=1e-3,comparable=F){
  RE<-names(ranef(mod))
  dat.name<-getCall(mod)[[3]]
  newdat<-model.frame(mod)
  if (comparable & length(dat.name)>1) {
    warning("The data of the model is given by a complex expression. This is not compatible with option 'comparable=T'. Reverting to default behavior 'comparable=F'")
    comparable<-F
  }
  # If the RE structure is already minimal, do nothing:
  if (length(RE)==length(getME(mod,"Ztlist"))) {return(mod)}
  else{
    # Get the formula without the RE:
    trms<-attributes(terms(formula(mod)))$term.labels
    sf<-paste(deparse(formula(mod)),collapse="")
    sf<-gsub("[[:space:]]+"," ",sf)
    for(j in grep("\\|+",trms)){
      sf<-gsub(trms[j],"",x=sf,fixed=T)
    }
    sf<-gsub("\\+ \\(([[:space:]]|\\(|\\)|\\+)*\\)","",sf)
    ff<-as.formula(sf)
    for(i in 1:length(RE)){			# Usually, 1 for subjects, 2 for items
      rot<-rePCA(mod)[[RE[i]]]$rotation
      # Check if this RE is not already minimal:
      if (length(rot)>1){
        mat<-do.call(cbind,getME(mod,"mmList")[grep(paste("| ",RE[i],sep=""),names(getME(mod,"mmList")),fixed=T)])
        rmat<-data.frame(mat%*%rot)
        names(rmat)<-paste(RE[i],names(rmat),sep='_')
        if (comparable & length(dat.name)==1){
          assign(as.character(dat.name),cbind(eval(dat.name),rmat))
        } else {
          newdat<-cbind(newdat,rmat)
        }
        
        nsdevs<-rePCA(mod)[[i]]$sdev/rePCA(mod)[[i]]$sdev[1]
        # Decide how many components we should keep:
        keep<-length(nsdevs[nsdevs>tol])
        # Add the new RE to the formula:
        ff<-update(ff,as.formula(paste(".~.+(0+",paste(names(rmat)[1:keep],collapse="+ "),ifelse(keep==1,"|","||"),RE[i],")")))
      }
      else{
        # If RE is minimal, reproduce the original with compatible names
        assign(paste(RE[i],"_X1",sep=""), 1)
        if (comparable & length(dat.name)==1){
          eval(parse(text=paste(as.character(dat.name),"<-cbind(",as.character(dat.name),",",RE[i],"_X1)",sep="")))
        } else {
          eval(parse(text=paste("newdat <-cbind(newdat,",RE[i],"_X1)",sep="")))
        }
        ff<-update(ff,as.formula(paste(".~.+(0+", paste(RE[i],"_X1",sep=""),"|",RE[i],")")))	
      }			
    }
    if (comparable& length(dat.name)==1) {
      pmod<-update(mod,ff)
    } else {
      pmod<-update(mod,ff,data=newdat)
    }
    # Clean up remaining components (some may end up under tol after removing correlations)
    ff2<-formula(pmod)
    changes<-F
    for(i in 1:length(RE)){
      k<-which(rePCA(pmod)[[RE[i]]]$sdev<tol*rePCA(pmod)[[RE[i]]]$sdev[1])
      ff2<-update(ff2,as.formula(paste(c("~.",paste(RE[i],"_X",k,"|",RE[i],")",sep="")),collapse="-(0+")))
      changes<-ifelse(length(k)>0,T,changes)
    }
    if(ff2==ff|!changes){
      return(pmod)
    }
    else{
      return(update(pmod,ff2))
    }
  }
}

##example for use:
MEM1<- lmer(RT ~ wordness + (1+wordness|subject)+ (1+prime+luminance|item), data = d, control=lmerControl(optimizer="bobyqa"))
summary (MEM1)
summary(parsimonious(MEM1))

## this does not give us our beloved p values!
#solution:
model<-mixed(parsimonious(MEM1), data=model.frame(parsimonious(MEM1)), method='LRT')
model

###### -> back to presentation!

##coding categorical data
#1. dummy
levels(d$wordness)
levels(d$wordness) <- c("nonword", "word")
contrasts(d$wordness) <- c(0, 1)

MEM1<- lmer(RT ~ wordness + (1+wordness|subject)+ (1|item), data = d, control=lmerControl(optimizer="bobyqa"))
summary (MEM1)

#2. sum
levels(d$wordness)
levels(d$wordness) <- c("nonword", "word")
contrasts(d$wordness) <- c(-1, 1)

                                                                                                      
MEM1<- lmer(RT ~ wordness + (1+wordness|subject)+ (1|item), data = d, control=lmerControl(optimizer="bobyqa"))
summary (MEM1)
ranef(MEM1)
###### -> back to presentation!

##logistic regression
list1<- rbinom(4920, 1, 0.8)
list2<- rbinom(4920, 1, 0.2)

d$decision<-ifelse(d$wordness=="word",list1,list2)
summary(d)

MEM1<- glmer(decision ~ wordness+ (1+wordness|subject) + (1|item), data = d, family = binomial, control=glmerControl(optimizer="bobyqa"))
summary (MEM1)


###### -> back to presentation!
d$luminance<-as.numeric(d$luminance)
##interactions
MEM1<- lmer(RT ~ luminance*wordness + (1+wordness|subject)+ (1|item), data = d, control=lmerControl(optimizer="bobyqa"))
summary (MEM1)

#simple slopes:
MEM1<- lmer(RT ~ luminance*wordness-luminance + (1+wordness|subject)+ (1|item), data = d, control=lmerControl(optimizer="bobyqa"))
summary (MEM1)

#we could also plot this to see how it looks
ggplot(data = subset(d), aes(y=RT, x=luminance, colour = wordness))+
  stat_smooth(aes(colour=wordness ), method = "lm", formula = y ~ x, size=1.4)+
  theme_bw()+
  theme(text = element_text(size = 20))

## model comparisons with interactions
MEM1<- lmer(RT ~ prime * wordness  + (1+wordness|subject)+ (1|item), data = d, control=lmerControl(optimizer="bobyqa"))
summary (MEM1)
MEM2<- lmer(RT ~ prime + wordness  + (1+wordness|subject)+ (1|item), data = d, control=lmerControl(optimizer="bobyqa"))
summary (MEM2)

anova(MEM2,MEM1)


###### BONUS
levels(d$prime)
levels(d$prime) <- c("identical", "different", "neutral", "control")
treat.codes <- matrix(c(0, 0, 1, 0,1, 0,1, 0, 0,0,0,0), 
                      nrow = 4, 
                      byrow = T,
                      dimnames = list(c("identical", "different", "neutral", "control"), c("identical", "different", "neutral")))
contrasts(d$prime) <- treat.codes


####################################################################
###################### NURA PRESENTATION CODE ######################
####################################################################


####### Create orthogonal contrasts for the predictors, or centred predictors ------
# # NB. Especially useful when you have more predictors, as you could later just set them to 0, i.e. their average
d <- d %>% 
  mutate(lum_c = scale(luminance, center=T, scale=F),
         word_c = ifelse(wordness == "word", .5, -.5) )

# New model
MEM2 <- lmer(RT ~ lum_c*word_c + (1+word_c|subject) + (1|item), data = d, control=lmerControl(optimizer="bobyqa"))
summary (MEM2)



#### General Linear Hypothesis testing  ---------------------------------------

# Do t-tests on point estimates (or the point estimates of a contrast)
# Snijders, T. A. B., & Bosker, R. J. (1999). Multilevel Analysis: An Introduction to Basic and Advanced Multilevel Modeling. London: SAGE.

## dfs should be based on sample size -1, or some other reasonable number
DF = length(unique(d$subject)) - 1 

## Remember the fixed effects in the model, as the contrasts are defined based on these, with 1 value (column) per effect
fixef(MEM2)
# i.e. Intercept, luminance, wordness, luminance:wordness - so, always need 4 columns


####### Any effect of Luminance for Wordness == 1, i.e. 0.5 ?

# Define the two conditions we want to contrast, or could input the difference contrast directly
# i.e. holding wordness = .5, compare luminance at -1 vs. 1
word1.lumLo <- matrix( c(0, -1, .5, -1*.5), nrow=1, byrow=T)
word1.lumHi <- matrix( c(0,  1, .5,  1*.5), nrow=1, byrow=T)
word1 <- word1.lumLo - word1.lumHi
word1

# Get beta value for the difference
b.word1 <- word1 %*% fixef(MEM2)
b.word1

# Get variance
v.word1 <- word1 %*% vcov(MEM2) %*% t(word1)
v.word1

# Calculate standard error
se.word1 <- sqrt(v.word1)
se.word1

# Calculate t-test
t.word1 <- as.numeric(b.word1 / se.word1)
t.word1

# Compute p-value from the t-test and p distribution
p.word1 <- 2 * pt(t.word1, df = DF, lower.tail = T)  # lower.tail needs to be False if t > 0
p.word1 # Yes!



####### Any effect of Luminance for Wordness == 0, i.e. -0.5 ?

word0 <- matrix( c(0, -1, -.5, -1*-.5), nrow=1, byrow=T) - 
  matrix( c(0,  1, -.5,  1*-.5), nrow=1, byrow=T)

b.word0 <- word0 %*% fixef(MEM2)                # beta
v.word0 <- word0 %*% vcov(MEM2) %*% t(word0)    # variance
t.word0 <- as.numeric(b.word0 / sqrt(v.word0) ) # t-test
t.word0
p.word0 <- 2 * pt(t.word0, df = DF, lower.tail = T)  # lower.tail needs to be False if t > 0
p.word0 # No




####### Or, Is there a significant effect of Wordness at Low Luminance ?

# i.e. holding luminance = -1, compare wordness at -.5 vs. .5
lumLo <- matrix( c(0, -1, -.5, -1*-.5), nrow=1, byrow=T) -
  matrix( c(0, -1,  .5, -1* .5), nrow=1, byrow=T)

b.lumLo <- lumLo %*% fixef(MEM2)                # beta
v.lumLo <- lumLo %*% vcov(MEM2) %*% t(lumLo)    # variance
t.lumLo <- as.numeric(b.lumLo / sqrt(v.lumLo))  # t-test
t.lumLo
p.lumLo <- 2 * pt(t.lumLo, df = DF, lower.tail = F)  # lower.tail needs to be False if t > 0
p.lumLo # Yes!



####### Any effect of Wordness at High Luminance ?

lumHi <- matrix( c(0, 1, -.5, 1*-.5), nrow=1, byrow=T) -
  matrix( c(0, 1,  .5, 1* .5), nrow=1, byrow=T)

b.lumHi <- lumHi %*% fixef(MEM2)                # beta
v.lumHi <- lumHi %*% vcov(MEM2) %*% t(lumHi)    # variance
t.lumHi <- as.numeric(b.lumHi / sqrt(v.lumHi))  # t-test
t.lumHi
p.lumHi <- 2 * pt(t.lumHi, df = DF, lower.tail = F)  # lower.tail needs to be False if t > 0
p.lumHi # Yes!








#### Generate model predictions at the average level (i.e. fixed effects)  ---------------------------------------
# Based on the arm package: Gelman, A., & Su, Y.-S. (2015). arm: Data Analysis Using Regression and Multilevel/Hierarchical Models.
library(arm)


# More info and alternative strategies can be found in this helpful tutorial: https://vuorre.netlify.com/post/2016/2016-03-06-multilevel-predictions/

# Create a matrix with our predictors and the levels we want to compare, to later combine with the model predictions
newdat <- expand.grid(
  lum_c = c(-1, 0, 1),
  word_c = c(-.5, .5)  )


# 1 - Simulating plausible parameter values
sims <- arm::sim(MEM2, n.sims = 1000)

# 2 - Saving the simulated samples (a faux posterior distribution) in a data frame - for the fixed effects (fixef)
sims.fix <- fixef(sims)

# 3 - Creating a predictor matrix
Xmat <- model.matrix( ~ lum_c*word_c, data = newdat)

# 4 - Creating a matrix for the fitted values
fitmat <- matrix(ncol=nrow(sims.fix), nrow=nrow(newdat))

# 5 - Calculating fitted values for each combination of the predictor values, for each plausible combination of the parameter values
for (i in 1:nrow(sims.fix)) { fitmat[,i] <- Xmat %*% as.matrix(sims.fix)[i,]  }

# 6 - Calculating the desired quantiles of the fitted values, i.e. 95% CI
newdat$lower <- apply(fitmat, 1, quantile, prob=0.05)
newdat$median <- apply(fitmat, 1, quantile, prob=0.5)
newdat$upper <- apply(fitmat, 1, quantile, prob=0.95)




#### Plotting the interaction  ---------------------------------------

# Plot model predictions overlayed on average data
d %>%
  group_by(subject, lum_c, word_c) %>%
  summarise(RT = mean(RT)) %>%
  ggplot(aes(x = lum_c, y = RT, col = factor(word_c), fill = factor(word_c))) +
  
  # Participants' data - dots (with SE across Ss, thanks to previously averaging within Ss)
  stat_summary(fun.data="mean_se", geom="pointrange", size=.8) +
  
  # Model Predictions - lines and shading
  geom_ribbon(data=newdat, aes(ymax=upper, ymin=lower, y=median), alpha=.2, col=NA) +
  geom_line(data=newdat, aes(y=median))





