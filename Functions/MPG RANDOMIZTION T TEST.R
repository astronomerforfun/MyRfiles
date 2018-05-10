library(MASS)
library(dplyr)
library(ggplot2)


mpg1 <- mpg[mpg$class %in% c("compact", "subcompact"),c("class", "hwy")]

#is average of class compact different then suv

subcompact <- mpg[mpg$class == "subcompact", ]
compact <- mpg[mpg$class == "compact",]
suv <- mpg[mpg$class == "suv",]
minivan <- mpg[mpg$class == "minivan",]


t.test(suv$hwy, minivan$hwy)

test <- mpg1%>%
  rep_sample_n(nrow(mpg1), reps = 10000)%>%
  mutate(hwy_perm = sample(mpg1$cty))%>%
  group_by(replicate, class)%>%
  summarise(hwy_perm = mean(hwy_perm), hwy_orig = mean(cty))%>%
  summarise(diff_perm = diff(hwy_perm),
            diff_original = diff(hwy_orig))

  
  ggplot(test, aes(x = diff_perm)) + 
  geom_histogram(binwidth = .05) +
  geom_vline(aes(xintercept = diff_original), col = "red")

  
  
library(NHANES)
  
  
#dO MEN OR WOMAN sleep more hours per night.  T.test.
  
  
ggplot(NHANES, aes(NHANES$SleepHrsNight, fill = Gender)) + geom_bar(aes(position = "fill"))
men <- NHANES[NHANES$Gender== "male",]
woman <- NHANES[NHANES$Gender == "female",]
t.test(men$SleepHrsNight, woman$SleepHrsNight)

test3 <- NHANES%>%
  rep_sample_n(size = nrow(NHANES), reps = 1000)%>%
  mutate(Sleep_perm = sample(Gender))%>%
  group_by(replicate, Gender)%>%
  summarize(sleep_perm = mean(Sleep_perm == "male")%>%
            sleep_hours_night = mean(Gender == "male"))%>%
  summarize(diff_perm = diff(sleep_hours_night_perm),
            diff_sleep_orig = diff(sleep_hours_night))

df <- NHANES[,c("Gender", "SleepHrsNight")]
df <- na.omit(df)


test3 <- df%>%
  rep_sample_n(size = nrow(df), reps = 1000)%>%
  mutate(sleep_perm = sample(SleepHrsNight))%>%
  group_by(replicate, Gender)
%>%
  summarize(sleep_perm = mean(sleep_perm),
            sleep_orig = mean(SleepHrsNight))%>%
  summarize(diff_perm = diff(sleep_perm),
            orig_sleep = diff(sleep_orig))
        


ggplot(test3, aes(x = diff_perm)) + 
  geom_histogram(binwidth = .01) +
  geom_vline(aes(xintercept = orig_sleep), col = "red")


#class 
