# Input data and creat a new variable
Original_Data <- read.csv(file =
                            "C:\\Users\\lenovo\\Desktop\\camp_teach.csv")

# Cleaning Data for Hypothesis Three
library(dplyr)
New_Data <- Original_Data %>% filter(fdays==0)
New_Data <- New_Data %>% filter(PREFEVPP!="na", 
                                PREFVCPP!="na", 
                                age_rz!="na",
                                ETHNIC!="na",
                                woodstove!="na",
                                hemog!="na",
                                wbc!="na",
                                agehome!="na",
                                dehumid!="na", 
                                GENDER!="na", 
                                parent_smokes!="na",
                                any_smokes!="na",
                                anypet!="na")

New_Data$Change <- New_Data$POSFEVPP-New_Data$PREFEVPP

summary(New_Data$age_rz)
table(New_Data$ETHNIC)
table(New_Data$GENDER)
summary(New_Data$hemog)
summary(New_Data$PREFEVPP)
summary(New_Data$POSFEVPP)
summary(New_Data$wbc)
summary(New_Data$agehome)
table(New_Data$anypet)
table(New_Data$woodstove)
table(New_Data$dehumid)
table(New_Data$parent_smokes)
table(New_Data$any_smokes)

TG1=vector();
for (i in 1:length(New_Data$TG)){
  if (New_Data$TG[i] == 'A') {
    TG1[i] = 1
  }
  if (New_Data$TG[i] == 'B') {
    TG1[i] = 2
  }
  if (New_Data$TG[i] == 'C') {
    TG1[i] = 0
  }
}

GENDER1=vector();
for (i in 1:length(New_Data$GENDER)){
  if (New_Data$GENDER[i] == 'm') {
    GENDER1[i] = 0
  }
  if (New_Data$GENDER[i] == 'f') {
    GENDER1[i] = 1
  }
}

ETHNIC1=vector();
for (i in 1:length(New_Data$ETHNIC)){
  if (New_Data$ETHNIC[i] == 'o') {
    ETHNIC1[i] = 0
  }
  if (New_Data$ETHNIC[i] == 'b') {
    ETHNIC1[i] = 1
  }
  if (New_Data$ETHNIC[i] == 'w') {
    ETHNIC1[i] = 2
  }
  if (New_Data$ETHNIC[i] == 'h') {
    ETHNIC1[i] = 3
  }
}

New_Data$TG1 <- TG1
New_Data$GENDER1 <- GENDER1
New_Data$ETHNIC1 <- ETHNIC1

# Regression Model

fit <- lm(Change ~ anypet + GENDER1 + age_rz + ETHNIC1
           + woodstove + hemog + wbc + agehome + dehumid + parent_smokes
           + any_smokes,
           Data <- New_Data)
summary(fit)


# QQ-Plot
QQplot_of_residuals <- qqnorm(fit$residuals, 
                          ylab="Sample", 
                          xlab="Theoretical")+qqline(fit$residuals)

QQplot_of_Change <- qqnorm(New_Data$Change, 
                          ylab="Sample", 
                          xlab="Theoretical")+qqline(New_Data$Change)

# Histgram
hist(fit$residuals)
hist(New_Data$Change)




# Cleaning Data for Hypothesis Two

New_Data1 <- Original_Data %>% group_by(id) %>% filter(max(fdays)!=min(fdays))
New_Data2 <- New_Data1 %>% group_by(id) %>% filter(fdays==max(fdays))
New_Data3 <- New_Data1 %>% group_by(id) %>% filter(fdays==min(fdays))
New_Data3$POST <- New_Data2$POSFEVPP
New_Data3$PRE <- New_Data2$PREFEVPP
New_Data3 <- New_Data3 %>% filter(PREFEVPP!="na", 
              PREFVCPP!="na", 
              TG!="na",
              age_rz!="na",
              ETHNIC!="na",
              woodstove!="na",
              hemog!="na",
              wbc!="na",
              agehome!="na",
              dehumid!="na", 
              GENDER!="na", 
              parent_smokes!="na",
              any_smokes!="na",
              anypet!="na")

TG1=vector();
for (i in 1:length(New_Data3$TG)){
  if (New_Data3$TG[i] == 'A') {
    TG1[i] = 1
  }
  if (New_Data3$TG[i] == 'B') {
    TG1[i] = 2
  }
  if (New_Data3$TG[i] == 'C') {
    TG1[i] = 0
  }
}

GENDER1=vector();
for (i in 1:length(New_Data3$GENDER)){
  if (New_Data3$GENDER[i] == 'm') {
    GENDER1[i] = 0
  }
  if (New_Data3$GENDER[i] == 'f') {
    GENDER1[i] = 1
  }
}

ETHNIC1=vector();
for (i in 1:length(New_Data3$ETHNIC)){
  if (New_Data3$ETHNIC[i] == 'o') {
    ETHNIC1[i] = 0
  }
  if (New_Data3$ETHNIC[i] == 'b') {
    ETHNIC1[i] = 1
  }
  if (New_Data3$ETHNIC[i] == 'w') {
    ETHNIC1[i] = 2
  }
  if (New_Data3$ETHNIC[i] == 'h') {
    ETHNIC1[i] = 3
  }
}

New_Data3$TG1 <- TG1
New_Data3$GENDER1 <- GENDER1
New_Data3$ETHNIC1 <- ETHNIC1

Change_In_Lung_Function<- New_Data3$POST - New_Data3$PRE - New_Data3$POSFEVPP + New_Data3$PREFEVPP
New_Data3$Change_In_Lung_Function <- Change_In_Lung_Function
fit1 <- lm(Change ~ anypet + TG1 + GENDER1 + age_rz + ETHNIC1
              + woodstove + hemog + wbc + agehome + dehumid + parent_smokes
              + any_smokes, 
              Data <- New_Data3)
summary(fit1)

QQplot_of_residuals <- qqnorm(fit1$residuals, 
                                  ylab="Sample", 
                                  xlab="Theoretical")+qqline(fit1$residuals)
QQplot_of_Change <- qqnorm(Change_In_Lung_Function, 
                              ylab="Sample", 
                              xlab="Theoretical")+qqline(Change_In_Lung_Function)

hist(fit1$residuals)
hist(Change_In_Lung_Function)

#Hypothesis One

fit2 <- lm(Change ~ anypet + TG1 + anypet*TG1 + GENDER1 + age_rz + ETHNIC1
           + woodstove + hemog + wbc + agehome + dehumid + parent_smokes
           + any_smokes, 
           Data <- New_Data3)
summary(fit2)

QQplot_of_residuals <- qqnorm(fit2$residuals, 
                              ylab="Sample", 
                              xlab="Theoretical")+qqline(fit2$residuals)
QQplot_of_Change <- qqnorm(Change_In_Lung_Function, 
                           ylab="Sample", 
                           xlab="Theoretical")+qqline(Change_In_Lung_Function)

hist(fit2$residuals)
hist(Change_In_Lung_Function)
length(New_Data3 %>% filter(TG=="A"))