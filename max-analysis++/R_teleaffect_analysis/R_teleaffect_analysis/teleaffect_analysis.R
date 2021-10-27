library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)
library(tibble)
library(gridExtra)

colTypes = cols(pid=col_integer(),
                arousal=col_double(),
                valence=col_double(),
                timeS=col_double(),
                slideshow=col_character(),
                viz=col_character(),
                session=col_integer(),
                trial=col_integer())

colNames = c("pid", "arousal", "valence", "timeS", "slideshow", "viz", "session", "trial")

conditions <- c("NO_VIZ", "CONGRUENT", "INCONGRUENT")
slideshows <- c("A", "B")

# 
# 
# pid1_interpData <- read_csv("../../processed_data/pid1-interpData.csv")
# pid2_interpData <- read_csv("../../processed_data/pid11-interpData.csv", col_types = colTypes)
# head(pid2_interpData)
# lookAround <- pid1_interpData[(pid1_interpData$viz == "A") &
#                        (pid1_interpData$slideshow == "A"), ]
# 
# unique(lookAround$trial)

rms <- function(x){
  return(sqrt(x**2))
}

makeMasterTibble <-  function(){
  tempTibble <- read_csv("\n", col_names=colNames, col_types = colTypes)
  for (file in list.files("../../processed_data")) {
    tmp <- read_csv(paste("../../processed_data/", file, sep=""), col_types = colTypes)
    tempTibble <- bind_rows(tempTibble, tmp)
  }
  print('complete!')
  return(tempTibble)
}

multiplot <- function(df){
  ggplot(data=df, 
         aes(x=timeS, y=arousal, group=individualize)) +
         geom_line(alpha=0.3, color='salmon', size=1)
}


plotCondition <- function(slideshowCond, vizCond, df){
  # slideshowCond := String
  # vizCond := String
  # df := a (decimated) df, like decimatedEvenPid
  cond <- df[df$slideshow==slideshowCond & df$viz == vizCond,]
  cond$individualize <- cond$trial+(cond$pid * 100)
  multiplot(cond)
}

plotByTrial <- function(df){
  
  ggplot(data=df, 
         aes(x=timeS, y=arousal, group=trial)) +
         geom_line(alpha=0.5, color='blue', size=1)
}

plotSingleRun <- function(df){
  # ASSUME: filter to specify a pid and trial
  ggplot(data=df,
         aes(x=timeS, y=valence))+
          geom_line() +
        ylim(-1,1)
}

# conditionize <- function(df){
#   df$condition <- case_when(df$slidesshow == df$viz ~ "CONGRUENT",
#                             df$slideshow != df$viz ~ "INCONGRUENT",
#                             )
#   return(df)
# }

conditionize <-function(slideshow, viz){
  case_when(viz == "0" ~ "NO_VIZ",
            slideshow == viz ~ "CONGRUENT",
            slideshow != viz ~ "INCONGRUENT"
            )
}

compareConds <- function(df, supply_pid, s){
  df <- mutate(df, condition = conditionize(slideshow, viz))
  df <- filter(df, pid==supply_pid, slideshow==s)
  ggplot(data=df, 
         aes(x=timeS, y=valence, group=trial, color=condition)) + 
         geom_line() +
         scale_color_brewer(palette="Dark2") + 
         ylim(-1,1) +
         ggtitle(paste("participant: ", supply_pid, ", video:", s)) +
         xlab("time (s)")
}

compareConds(decimatedOddPid, 11, "B")
compareConds(decimatedEvenPid, 8, "A")

# setup critical env vars
masterTibble <- makeMasterTibble()

evenPid <- masterTibble[masterTibble$pid%%2 == 0, ]
decimatedEvenPid <- (evenPid %>% filter(row_number() %% 10 == 0))
decimatedEvenPid <- mutate(decimatedEvenPid, condition =  conditionize (slideshow, viz))

oddPid <- masterTibble[masterTibble$pid%%2 == 1, ]
decimatedOddPid <- (oddPid %>% filter(row_number() %% 10 == 0))
decimatedOddPid <- mutate(decimatedOddPid, condition =  conditionize (slideshow, viz))


# ------------------------------------------------------------------
# Make column where we see retest index
# ------------------------------------------------------------------

retestify <- function(trial){
  trials = unique(trial)
  case_when(trial == trials[1] ~ 1,
            trial == trials[2] ~ 2,
            TRUE ~ 0)
}

decimatedEvenPid <- decimatedEvenPid %>% group_by(pid, condition, slideshow) %>% mutate(retest = retestify(trial))
decimatedOddPid <- decimatedOddPid %>% group_by(pid, condition, slideshow) %>% mutate(retest = retestify(trial))

# plotSingleRun(decimatedOddPid %>% filter(pid==9, trial==12))

# ------------------------------------------------------------------
# TESTING TO GET MEAN
# ------------------------------------------------------------------

plotSingleRun(decimatedEvenPid %>% filter(slideshow=="A", pid==12, condition=="CONGRUENT", retest==2))

getAverageBetweenRetests <- function(df, slideshow_in, pid_in, condition_in){
  v1 <- df %>% filter(slideshow==slideshow_in, 
                      pid==pid_in, 
                      condition==condition_in, 
                      retest==1)
  v2 <- df %>% filter(slideshow==slideshow_in, 
                      pid==pid_in, 
                      condition==condition_in, 
                      retest==2)
  
  meanValence <- (v1$valence +  v2$valence)/2 # this is how we gotta get le mean
  meanArousal <- (v1$arousal +  v2$arousal)/2 # this is how we gotta get le mean
  timePoints <- df %>% filter(slideshow==slideshow_in, pid==pid_in, condition==condition_in, retest==1) %>% select(timeS)
  
  timePoints <- timePoints %>% add_column(pid=pid_in, slideshow=slideshow_in, condition=condition_in, mean_valence=meanValence, mean_arousal=meanArousal)
  # meanValence <- meanValence %>% mutate(timePoints)
  return(timePoints)
}

getAverageBetweenRetestsGraph <- function(df, slideshow_in, pid_in, condition_in, affect){
  
  meanValues <- getAverageBetweenRetests(df, slideshow_in, pid_in, condition_in)

  ggplot() + geom_line(data=meanValues,
                       aes_string(x="timeS", y=paste("mean_",affect, sep="")), color="steelblue")+
    geom_line(data=decimatedEvenPid %>% 
                filter(slideshow==slideshow_in, 
                       pid==pid_in, 
                       condition==condition_in, 
                       retest==1),
              aes_string(x="timeS", y=affect), color="salmon")+
    geom_line(data=decimatedEvenPid %>% 
                filter(slideshow==slideshow_in, 
                       pid==pid_in, 
                       condition==condition_in, 
                       retest==2),
              aes_string(x="timeS", y=affect), color="salmon") +
    ylim(-1,1)
}

getAllAverage <- function(df){
  df <- ungroup(df)
  aveColTypes = cols(pid=col_integer(),
                     condition=col_character(),
                     slideshow=col_character(),
                     timeS=col_double(),
                     mean_valence=col_double(),
                     mean_arousal=col_double()
                     )
  aveColNames = c("pid", "condition", "slideshow", "timeS", "mean_valence", "mean_arousal")
  
  workingTibble <- read_csv("\n", col_names=aveColNames, col_types = aveColTypes)
  pids = unique(df$pid)
  
  for (pid in pids) {
    print(paste('evaluating:',pid))
    for (cond in conditions){
      for(s in slideshows){
        tmp <- getAverageBetweenRetests(df, s, pid, cond)
        workingTibble <- bind_rows(workingTibble, tmp)
      }
    }
  }
  return(workingTibble)
}

getMeanInterRatingReliability <- function(df){
  interColTypes = cols(pid=col_integer(),
                     condition=col_character(),
                     slideshow=col_character(),
                     inter_valence=col_double(),
                     inter_arousal=col_double()
  )
  interColNames = c("pid", "condition", "slideshow", "inter_valence", "inter_arousal")
  workingTibble <- read_csv("\n", col_names=interColNames, col_types = interColTypes)
  pids = unique(df$pid)
  for (p in pids) {
    print(paste('assessing: ',p))
    for(cond in conditions){
      for (s in slideshows){
        meanInterRatings <- interRating(df, s, p, cond) # c(valence, arousal)
        tmp <- tibble(pid=p, 
                      condition=cond, 
                      slideshow = s, 
                      inter_valence = meanInterRatings[1], 
                      inter_arousal = meanInterRatings[2])
        workingTibble <- bind_rows(workingTibble, tmp)
      }
    }
  }
  return(workingTibble)
  # timePoints <- timePoints %>% add_column(pid=pid_in, slideshow=slideshow_in, condition=condition_in, mean_valence=meanValence, mean_arousal=meanArousal)
  
  # return(timePoints)
}

interRating <- function(df, slideshow_in, pid_in, condition_in){
  v1 <- df %>% filter(slideshow==slideshow_in, 
                      pid==pid_in, 
                      condition==condition_in, 
                      retest==1)
  v2 <- df %>% filter(slideshow==slideshow_in, 
                      pid==pid_in, 
                      condition==condition_in, 
                      retest==2)
  
  valenceDiff <- v1$valence - v2$valence 
  arousalDiff <- v1$arousal - v2$arousal 
  return(c(mean(valenceDiff), mean(arousalDiff)))
}
decimatedMerged <- bind_rows(decimatedOddPid,decimatedEvenPid)
interRatingReliability <- getMeanInterRatingReliability(decimatedMerged)





allAveEven <- getAllAverage(decimatedEvenPid)
allAveOdd <-  getAllAverage(decimatedOddPid)



# take a look at what the means look like for a participant
ggplot(data=allAveEven %>% filter(pid==6, slideshow=="B"), aes(x=timeS, y=mean_arousal, color=condition)) +
  geom_line() +
  ylim(-1,1)

makeDifferenceMtrx <- function(df){
  # REQUIRES: a Tibble of averages
  
  # pid diff_effect    diff
  # 2   nv-congruent   -0.1
  # ...
  
  df <- ungroup(df)
  
  
  aveColTypes = cols(pid=col_integer(),
                     diff_effect=col_character(),
                     diff_arousal=col_double(),
                     diff_valence=col_double()
  )
  aveColNames = c("pid", "diff_effect", "diff_arousal", "diff_valence")
  miniConds = c("CONGRUENT", "INCONGRUENT")  
  
  
  workingTibble <- read_csv("\n", col_names=aveColNames, col_types = aveColTypes)
  pids = unique(df$pid)
  for (p in pids){
    print(paste('processing:',p))
    for(s in slideshows){
        nv <- df %>% filter(pid==p, slideshow==s, condition=="NO_VIZ")
      for(cond in miniConds){
        c <- df %>% filter(pid==p, slideshow==s, condition==cond)
        
        diffArousal <- rms(nv$mean_arousal - c$mean_arousal)
        diffArousal <- mean(diffArousal)
        diffValence <- rms(nv$mean_valence - c$mean_valence)
        diffValence <- mean(diffValence)
        tmp = tibble(pid=p, diff_effect=cond, diff_arousal = diffArousal, diff_valence = diffValence)
        workingTibble <- bind_rows(workingTibble, tmp)
      }
    }
  }
  return(workingTibble)
}

diffMtrx <- bind_rows(makeDifferenceMtrx(allAveEven), makeDifferenceMtrx(allAveOdd))

diffMtrx
p1 <-  	ggplot(diffMtrx, 
               aes(x=diff_effect, y=diff_arousal), color=diff_effect) +
  geom_boxplot() +
  geom_hline(aes(yintercept=GRAND_WITHIN_MEAN_AROUSAL, linetype="mean\ninter-rating\narousal"), color = "blue") +
  ylim(0,1) +
  ylab("arousal") +
  xlab("condition") +
  scale_linetype_manual(name = "legend", values = c(2), 
                        guide = guide_legend(override.aes = list(color = c("blue")))) +
  theme(legend.text=element_text(size=rel(0.8)))

p2 <-  	ggplot(diffMtrx, 
               aes(x=diff_effect, y=diff_valence), color=diff_effect) +
  geom_boxplot() +
  geom_hline(aes(yintercept=GRAND_WITHIN_MEAN_VALENCE, linetype="mean \ninter-rating \nvalence"), color = "blue") +
  ylim(0,1) +
  ylab("valence") +
  xlab("condition") +
  scale_linetype_manual(name = "legend", values = c(2), 
                        guide = guide_legend(override.aes = list(color = c("blue")))) +
  theme(legend.text=element_text(size=rel(0.8)))


grid.arrange(p1,p2, nrow=1)

makeDifferenceMtrxWITHIN <- function(df){
  # REQUIRES: a Tibble (decunatedEvebPid)
  
  # pid diff_effect    diff
  # 2   nv-congruent   -0.1
  # ...
  df <- ungroup(df)
  
  aveColTypes = cols(
                     pid=col_integer(),
                     diff_within=col_character(),
                     diff_arousal=col_double(),
                     diff_valence=col_double()
                    )
  aveColNames = c("pid","diff_within", "diff_arousal", "diff_valence")
  
  workingTibble <- read_csv("\n", col_names=aveColNames, col_types = aveColTypes)
  pids = unique(df$pid)
  for (p in pids){
    print(paste('processing:',p))
    for(s in slideshows){
      for(cond in conditions){
        c1 <- df %>% filter(pid==p, slideshow==s, condition==cond, retest==1) %>% select(valence, arousal)
        c2 <- df %>% filter(pid==p, slideshow==s, condition==cond, retest==2) %>% select(valence, arousal)
        diffArousal <- rms(c1$arousal-c2$arousal)
        diffArousal <- mean(diffArousal)
        diffValence <- rms(c1$valence-c2$valence)
        diffValence <- mean(diffValence)
        tmp = tibble(pid=p, diff_within=cond, diff_arousal = diffArousal, diff_valence = diffValence)
        workingTibble <- bind_rows(workingTibble, tmp)
      }
    }
  }
  return(workingTibble)
}

withinDiffs <- bind_rows(makeDifferenceMtrxWITHIN(decimatedEvenPid),
                         makeDifferenceMtrxWITHIN(decimatedOddPid))


ggplot(withinDiffs, aes(x=diff_within, y=diff_arousal, fill=diff_within))+
  geom_boxplot()+
  scale_fill_brewer(palette="Pastel2") +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.2) +
  ggtitle('Inter-rating differences within participants')

labelizeAffect <- function(x){
  print(x)
}
# withinDiffs %>% gather(affect_type, affect_val, diff_arousal, diff_valence)
ggplot(withinDiffs %>% gather(affect_type, affect_val, diff_arousal, diff_valence), 
       aes(x=factor(diff_within), y=affect_val, fill=diff_within, color=diff_within))+
  geom_boxplot()+
  scale_fill_brewer(palette="Pastel2") +
  scale_color_brewer(palette="Dark2") +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.2) +
  facet_wrap(~affect_type, labeller=labelizeAffect) +
  ggtitle('Inter-rating differences within participants') +
  ylab('affect rating') +
  xlab('')






# Visualizing eeeeeverything .....................................................

# all average everything

allAve <- bind_rows(allAveEven, allAveOdd)

allAve

allAveEven %>% filter(slideshow=="A", pid==12, timeS < 1.01, condition=="NO_VIZ")


ggplot(data=allAveEven %>% filter(slideshow=="A"), 
       aes(x=timeS, y=mean_valence, group=interaction(condition,pid), color=condition)) +
       geom_line(alpha=0.4) +
       scale_color_brewer(palette = "Dark2") +
       ggtitle("Valence ratings over time for slideshow A2") +
       ylab("valence") +
       xlab("time (s)")
    

ggplot(data=allAveEven %>% filter(slideshow=="B"), 
       aes(x=timeS, y=mean_valence, group=interaction(condition,pid), color=condition)) +
  geom_line(alpha=0.4) +
  scale_color_brewer(palette = "Dark2")

ggplot(data=allAveEven %>% filter(slideshow=="B"), 
       aes(x=timeS, y=mean_valence, group=interaction(condition,pid), color=condition)) +
  geom_line(alpha=0.4)

ggplot(data=allAveOdd %>% filter(slideshow=="A"), 
       aes(x=timeS, y=mean_arousal, group=interaction(condition,pid), color=condition)) +
  geom_line(alpha=0.6)

ggplot(data=allAveOdd %>% filter(slideshow=="B"), 
       aes(x=timeS, y=mean_arousal, group=interaction(condition,pid), color=condition)) +
  geom_line(alpha=0.6)




# FINALLY...THE GOD DAMN ANALYSIS ------------------------------------------------


GRAND_WITHIN_MEAN_AROUSAL <- mean(withinDiffs$diff_arousal)
GRAND_WITHIN_MEAN_VALENCE <- mean(withinDiffs$diff_valence)
GRAND_WITHIN_MEAN_AROUSAL

congruentDiff <- diffMtrx %>% filter(diff_effect=="CONGRUENT")
MEAN_CONGRUENT_EFFECT_AROUSAL <- mean(congruentDiff$diff_arousal)
MEAN_CONGRUENT_EFFECT_VALENCE <- mean(congruentDiff$diff_valence)
MEAN_INCONGRUENT_EFFECT_AROUSAL <- mean(diffMtrx[diffMtrx$diff_effect == "INCONGRUENT",]$diff_arousal)
MEAN_INCONGRUENT_EFFECT_VALENCE <- mean(diffMtrx[diffMtrx$diff_effect == "INCONGRUENT",]$diff_valence)


# testing the effect of visualizations .....................................

t.test(diffMtrx %>% filter(diff_effect=="CONGRUENT") %>% select(diff_arousal), mu=GRAND_WITHIN_MEAN_AROUSAL)
t.test(diffMtrx %>% filter(diff_effect=="CONGRUENT") %>% select(diff_valence), mu=GRAND_WITHIN_MEAN_VALENCE)


t.test(diffMtrx %>% filter(diff_effect=="INCONGRUENT") %>% select(diff_arousal), mu=GRAND_WITHIN_MEAN_AROUSAL)
t.test(diffMtrx %>% filter(diff_effect=="INCONGRUENT") %>% select(diff_valence), mu=GRAND_WITHIN_MEAN_VALENCE)

# t.test(   diffMtrx %>% filter(diff_effect=="CONGRUENT") %>% select(diff_arousal), 
#        mu=MEAN_INCONGRUENT_EFFECT_AROUSAL)
# 
# t.test(   diffMtrx %>% filter(diff_effect=="CONGRUENT") %>% select(diff_valence), 
#           mu=MEAN_INCONGRUENT_EFFECT_VALENCE)

t.test(   diffMtrx %>% filter(diff_effect=="CONGRUENT") %>% select(diff_arousal), 
          diffMtrx %>% filter(diff_effect=="INCONGRUENT") %>% select(diff_arousal))

t.test(   diffMtrx %>% filter(diff_effect=="CONGRUENT") %>% select(diff_valence), 
          diffMtrx %>% filter(diff_effect=="INCONGRUENT") %>% select(diff_valence))


# testing if there's sig diff of variances .......................................
congruent_diff <- diffMtrx %>% filter(diff_effect=="CONGRUENT")
incongruent_diff <-diffMtrx %>% filter(diff_effect=="INCONGRUENT") 

var.test(congruent_diff$diff_arousal,
         incongruent_diff$diff_arousal,
         alternative = "two.sided")

var.test(congruent_diff$diff_valence,
         incongruent_diff$diff_valence,
         alternative = "two.sided")


var(incongruent_diff$diff_arousal)
var(congruent_diff$diff_arousal)


congruentAve <- allAve %>% filter(condition=="CONGRUENT")
incongruentAve <- allAve %>% filter(condition=="INCONGRUENT")
nvAve <- allAve %>% filter(condition=="NO_VIZ")
GLOBAL_MEAN_CONGRUENT_AROUSAL <- congruentAve$mean_arousal %>% mean
GLOBAL_MEAN_CONGRUENT_VALENCE <- congruentAve$mean_valence %>% mean

GLOBAL_MEAN_INCONGRUENT_VALENCE <- incongruentAve$mean_valence %>% mean

GLOBAL_MEAN_NV_VALENCE <- nvAve$mean_valence %>% mean

GLOBAL_MEAN_INCONGRUENT_VALENCE
GLOBAL_MEAN_CONGRUENT_VALENCE
GLOBAL_MEAN_NV_VALENCE

participantGlobalMeans <- allAve %>% 
                            group_by(pid, condition, slideshow) %>% 
                            summarise(p_m_arousal = mean(mean_arousal), p_m_valence =  mean(mean_valence))
participantGlobalMeans  

ggplot(participantGlobalMeans, aes(x=condition, y=p_m_valence)) +
  geom_boxplot() +
  ylab('mean valence') +
  ggtitle("valence") +
  ylim(-1,1)

ggplot(participantGlobalMeans, aes(x=condition, y=p_m_arousal)) +
  geom_boxplot() +
  ylab('mean valence') +
  ggtitle("valence") +
  ylim(-1,1)


aov(p_m_valence ~ condition, data=participantGlobalMeans) %>% summary
aov(p_m_arousal ~ condition, data=participantGlobalMeans) %>% summary
# maybe do an ANOVA
# factor1: condition
# factor2: arousal-type

# TODO: an anova can probably show difference between global means...


# INTER-RATING RELIABILITY -----------------------------------------------------
interValenceNV <- interRatingReliability %>% filter(condition=="NO_VIZ") %>% pull(inter_valence) %>% mean
interArousalNV <- interRatingReliability %>% filter(condition=="NO_VIZ") %>% pull(inter_arousal) %>% mean
interArousalNV


ggplot(interRatingReliability, aes(x=condition, y=inter_arousal)) +
  geom_boxplot() +
  ylab('inter-arousal reliability') +
  ggtitle("Arousal inter-rating reliability")

ggplot(interRatingReliability, aes(x=condition, y=inter_valence)) +
  geom_boxplot() +
  ylab('inter-valence reliability') +
  ggtitle("Valence inter-rating reliability")


interRatingReliability %>% filter(condition=="INCONGRUENT") %>% pull(inter_valence) %>% var
interRatingReliability %>% filter(condition=="CONGRUENT") %>% pull(inter_valence) %>% var
interRatingReliability %>% filter(condition=="NO_VIZ") %>% pull(inter_valence) %>% var

var.test(interRatingReliability %>% filter(condition=="INCONGRUENT") %>% pull(inter_valence),
         interRatingReliability %>% filter(condition=="CONGRUENT") %>% pull(inter_valence),
         alternative = "two.sided")

aov(inter_arousal ~ condition, data = interRatingReliability) %>% summary

aov(inter_valence ~ condition, data = interRatingReliability) %>% summary

# 
# testy <- function(df, p){
#   df <- ungroup(df)
#   s = "A"
#   cond = "CONGRUENT"
#   c1 <- df %>% filter(pid==p, slideshow==s, condition==cond, retest==1) %>% select(valence, arousal)
#   c2 <- df %>% filter(pid==p, slideshow==s, condition==cond, retest==2) %>% select(valence, arousal)
#   diffArousal <- c1$arousal - c2$arousal
#   # diffArousal <- mean(diffArousal)
#   diffValence <- c1$valence - c2$valence
#   diffValence <- mean(diffValence)
#   return(diffArousal)
# }

# differenceDF <- decimatedEvenPid %>% filter( 
#   pid==12, 
#   slideshow=="A", 
#   condition=="CONGRUENT", 
#   retest==1
# ) %>% select(timeS) %>%
#   add_column(diff=d4p122)
# 
# differenceDF2 <- decimatedEvenPid %>% filter( 
#   pid==12, 
#   slideshow=="A", 
#   condition=="CONGRUENT", 
#   retest==1
# ) %>% select(timeS) %>%
#   add_column(diff=d4p12)
# 
# 
# 
# ggplot() +
#         geom_line(data= decimatedEvenPid %>% filter( 
#           pid==12, 
#           slideshow=="A", 
#           condition=="CONGRUENT", 
#         ),
#         aes(x=timeS, y=arousal, group=retest)) +
#         geom_line(data=differenceDF, aes(x=timeS, y=diff), color="red")



# -----------------------------------------------------------------------------------------

# no_viz - Congruent
# no_viz - Incongruent


# -----------------------------------------------------------------------------------------

# head(masterTibble[masterTibble$viz=="0" & masterTibble$pid==1, ]$arousal - 
#        masterTibble[masterTibble$viz=="A" & masterTibble$pid==1, ]$arousal)



getEndCrop <- function(M, ct){
  # M := Matrix (or df) for condition A
  # ct := critical trials (vector of key trials)
  return(min(max(M[M$trial==ct[1],]$timeS), max(M[M$trial==ct[2],]$timeS)))
}

getPairs <- function(cond){
  # cond := Matrix corresponding to a condition (e.g. Viz=A, slideshow=A)
  trials = unique(cond$trial)
  return(trials)
}



pidTrialGroup <- decimatedEvenPid %>%
                  group_by(pid, trial) %>%
                  filter(slideshow=="A")
  
pidTrialGroup %>% summarize(timeS = max(timeS)) %>% min(trial) # does not work

decimatedEvenPid$session

maxTimesForVideo <- pidTrialGroup %>% summarize(timeS = max(timeS))
maxTimesForVideo %>% filter(timeS < 40)
maxTimesForVideo[maxTimesForVideo$pid == 12,]
pid12congruent <- filter(decimatedEvenPid, slideshow=="A", viz=="A", pid==12)



pairs <- getPairs(pid12congruent)
endCrop <- getEndCrop(pid12congruent, pairs)
attempt1 <- pid12congruent %>% filter(timeS <= endCrop)
nrow(attempt1 %>% filter(trial==5))
nrow(attempt1 %>% filter(trial==10))
getEndCrop()

decimatedEvenPid$congruent <- decimatedEvenPid$slideshow == decimatedEvenPid$viz

byCongruent <- decimatedEvenPid %>%
                  group_by(congruent, trial)

group_data(byCongruent)
group_keys(byCongruent)
group_size(byCongruent)

tail(masterTibble[masterTibble$slideshow=="A" & masterTibble$viz=="0" & masterTibble$pid==1 & masterTibble$timeS < 33 & mt$trial == 2, ])$timeS
tail(masterTibble[masterTibble$slideshow=="A" & masterTibble$viz=="A" & masterTibble$pid==1 & masterTibble$timeS < 33 & mt$trial == 4, ])$timeS

nrow(masterTibble[masterTibble$slideshow=="A" & masterTibble$viz=="0" & masterTibble$pid==1 & masterTibble$timeS < 33 & mt$trial == 2, ])
nrow(masterTibble[masterTibble$slideshow=="A" & masterTibble$viz=="A" & masterTibble$pid==1 & masterTibble$timeS < 33 & mt$trial == 4, ])


length(masterTibble[masterTibble$viz=="0" & masterTibble$pid==1 & masterTibble$timeS < 44, ]$timeS)
length(masterTibble[masterTibble$viz=="A" & masterTibble$pid==1 & masterTibble$timeS < 44, ]$timeS)

mt <- masterTibble
tail(mt[mt$slideshow=="A" & mt$viz=="0" & mt$pid==1 & mt$timeS < 34.3,])$timeS
tail(mt[mt$slideshow=="A" & mt$viz=="A" & mt$pid==1 & mt$timeS < 34.3,])$timeS

n_occur <-data.frame(table((mt[mt$slideshow=="A" & mt$viz=="0" & mt$pid==1 & mt$timeS < 34.3,]$timeS)))
n_occur[n_occur$Freq > 1, ]
# plotting with long data -----
lomg <- gather(s1, "key", "value", valence, arousal)

ggplot(data=lomg, aes(x=timeS, y=value, color=key)) +
  geom_line()


