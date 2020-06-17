#o see column wise missing values
data = read.csv("appstore_games.csv")
head(data)
str(data)
install.plibrary("dplyr")
library("dplyr")
sapply(data, function(x) sum(is.na(x)))
data$Size[is.na(data$Size)]<-mean(data$Size,na.rm=TRUE)
data$Price[is.na(data$Price)]<-0
data <- na.omit(data)
rel_col <- c("Name","Average.User.Rating","User.Rating.Count","Price","In.app.Purchases","Age.Rating","Languages","Size","Original.Release.Date","Current.Version.Release.Date")
df$Original.Release.Date <- as.Date(as.character(df$Original.Release.Date),format ="%d/%m/%Y")
df$Current.Version.Release.Date <- as.Date(as.character(df$Current.Version.Release.Date),format ="%d/%m/%Y")
date_coll = as.Date("03/08/2019",format="%d/%m/%Y")
df$tenure <- difftime(date_coll,df$Original.Release.Date,units="day")
df$tenure <- as.numeric(df$tenure)
df$days_since_last_release <-difftime(date_coll,df$Current.Version.Release.Date,units="day")
df$days_since_last_release <- as.numeric(df$days_since_last_release)
df$Languages <- strsplit(as.character(df$Languages),",")
df$lang_count <- lengths(df$Languages)
df$New.In.app.Purchases = lapply(df$In.app.Purchases, as.numeric)
df$mean_in_app_purchase = lapply(df$Nedrops <- c("New.In.app.Purchases","In.app.purchases","Name","Original.Release.Date","Current.Version.Release.Date","Languages","languages","In.app.Purchases")
w.In.app.Purchases, mean)
df$sum_in_app_purchase = lapply(df$New.In.app.Purchases,sum)
df$mean_in_app_purchase<-unlist(df$mean_in_app_purchase,recursive=TRUE)
df$sum_in_app_purchase <- unlist(df$sum_in_app_purchase,recursive = TRUE)
df$mean_in_app_purchase[is.na(df$mean_in_app_purchase)]<-0
df$rating <- ifelse(df$Average.User.Rating>=4.5,1,0)
drops <- c("New.In.app.Purchases","In.app.purchases","Original.Release.Date","Current.Version.Release.Date","Languages","languages","In.app.Purchases")
df1<- df[ , !(names(df) %in% drops)]
head(df1)

with(df1, t.test(rating ~ Treatment))

datavar <- c("User.Rating.Count","Price","Size","tenure", "days_since_last_release","Adult","mean_in_app_purchase")
lapply(datavar, function(v) {
  t.test(df1[, v] ~ df1$rating)
})
describeBy(df1)
df1$ln_price <- log(df1$Price+1)
df1$ln_mean_in_app_purchase <- log(df1$mean_in_app_purchase+1)
df1$ln_size <- log(df1$Size+1)
df1$ln_user.rating.count <- log(df1$User.Rating.Count+1)

model_ps <- glm(Treatment~User.Rating.Count+ln_price+Size+tenure+days_since_last_release+Adult+mean_in_app_purchase,family=binomial(),data=df1)
summary(model_ps)

model_ps1 <- glm(Treatment~ln_user.rating.count+ln_price+ln_size+tenure+days_since_last_release+Adult+ln_mean_in_app_purchase,family=binomial(),data=df1)
summary(model_ps1)

model_ps2 <- glm(Treatment~ln_user.rating.count+ln_price+Size+tenure+days_since_last_release+Adult+ln_mean_in_app_purchase,family=binomial(),data=df1)
summary(model_ps2)

data_cov<- c("User.Rating.Count","ln_price","Size","tenure", "days_since_last_release","Adult","mean_in_app_purchase")

df1 %>%
  group_by(Treatment) %>%
  select(one_of(data_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))

prs_df <- data.frame(pr_score = predict(model_ps, type = "response"),
                     Treatment = model_ps$model$Treatment)

head(prs_df)
labs <- paste("Group type:", c("Treatment", "Control"))

prs_df %>%
  mutate(Treatment = ifelse(Treatment== 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score,color=Treatment)) +
  geom_histogram()


data_nomiss <- df1 %>%  
  select(rating, Treatment, one_of(data_cov)) %>%
  na.omit()
names(data_nomiss)

install.packages("MatchIt")
library(MatchIt)

mod_match <- matchit(Treatment~User.Rating.Count+ln_price+Size+tenure+days_since_last_release+Adult+mean_in_app_purchase,method = "nearest", data = data_nomiss, caliper = 0.1)

plot(mod_match)

dta_m <- match.data(mod_match)
dim(dta_m)

lapply(data_cov, function(v) {
  t.test(dta_m[, v] ~ dta_m$Treatment)
})

with(dta_m, t.test(rating ~ Treatment))


###### trying with rating as continuous variable

with(df1, t.test(Average.User.Rating ~ Treatment))

model_ps <- glm(Treatment~User.Rating.Count+ln_price+Size+tenure+days_since_last_release+Adult+mean_in_app_purchase,family=binomial(),data=df1)
summary(model_ps)

data_cov<- c("User.Rating.Count","ln_price","Size","tenure", "days_since_last_release","Adult","mean_in_app_purchase")

df1 %>%
  group_by(Treatment) %>%
  select(one_of(data_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))

prs_df <- data.frame(pr_score = predict(model_ps, type = "response"),
                     Treatment = model_ps$model$Treatment)

head(prs_df)
labs <- paste("Group type:", c("Treatment", "Control"))

prs_df %>%
  mutate(Treatment = ifelse(Treatment== 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score,color=Treatment)) +
  geom_histogram()


data_nomiss <- df1 %>%  
  select(Average.User.Rating, Treatment, one_of(data_cov)) %>%
  na.omit()
names(data_nomiss)


mod_match <- matchit(Treatment~User.Rating.Count+ln_price+Size+tenure+days_since_last_release+Adult+mean_in_app_purchase,method = "nearest", data = data_nomiss, caliper = 0.1)

plot(mod_match)

dta_m <- match.data(mod_match)
dim(dta_m)

lapply(data_cov, function(v) {
  t.test(dta_m[, v] ~ dta_m$Treatment)
})

with(dta_m, t.test(Average.User.Rating ~ Treatment))

###### trying with ln_variables

model_ps1 <- glm(Treatment~ln_user.rating.count+ln_price+ln_size+tenure+days_since_last_release+Adult+ln_mean_in_app_purchase,family=binomial(),data=df1)
summary(model_ps1)

data_cov<- c("ln_user.rating.count","ln_price","ln_size","tenure", "days_since_last_release","Adult","ln_mean_in_app_purchase")
df1 %>%
  group_by(Treatment) %>%
  select(one_of(data_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))

prs_df <- data.frame(pr_score = predict(model_ps, type = "response"),
                     Treatment = model_ps$model$Treatment)
head(prs_df)
labs <- paste("Group type:", c("Treatment", "Control"))
prs_df %>%
  mutate(Treatment = ifelse(Treatment== 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score,color=Treatment)) +
  geom_histogram()

data_nomiss <- df1 %>%  
  select(rating, Treatment, one_of(data_cov)) %>%
  na.omit()
names(data_nomiss)

install.packages("MatchIt")
library(MatchIt)
mod_match <- matchit(Treatment~ln_user.rating.count+ln_price+ln_size+tenure+days_since_last_release+Adult+ln_mean_in_app_purchase,method = "nearest", data = data_nomiss, caliper = 0.1)
plot(mod_match)
dta_m <- match.data(mod_match)
dim(dta_m)

lapply(data_cov, function(v) {
  t.test(dta_m[, v] ~ dta_m$Treatment)
})

with(dta_m, t.test(rating ~ Treatment))

##### Taking app updation in past 1 year as treatment variable along with log of independent variables

df1 <- df1%>% select(-treatment)
df_1year <- df1%>%select(-Treatment)

df_1year$Treatment <- ifelse(df_1year$days_since_last_release <=365,1,0)
names(df_1year)


with(df_1year, t.test(rating ~ Treatment))

datavar <- c("User.Rating.Count","Price","Size","tenure","Adult","mean_in_app_purchase","lang_count")
lapply(datavar, function(v) {
  t.test(df_1year[, v] ~ df1$rating)
})
describeBy(df_1year)
df_1year$ln_lang_count <- log(df$lang_count+1)

#model_ps <- glm(Treatment~User.Rating.Count+ln_lang_count+ln_price+Size+tenure+days_since_last_release+Adult+mean_in_app_purchase,family=binomial(),data=df1)
#summary(model_ps)

model_ps <- glm(Treatment~ln_user.rating.count+ln_lang_count+ln_price+ln_size+tenure+Adult+ln_mean_in_app_purchase,family=binomial(),data=df_1year)
summary(model_ps)

##model_ps2 <- glm(Treatment~ln_user.rating.count+lang_count+ln_price+Size+tenure+days_since_last_release+Adult+ln_mean_in_app_purchase,family=binomial(),data=df1)
#summary(model_ps2)

data_cov<- c("ln_user.rating.count","ln_price","ln_lang_count","ln_size","tenure", "Adult","ln_mean_in_app_purchase")

df1 %>%
  group_by(Treatment) %>%
  select(one_of(data_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))

prs_df <- data.frame(pr_score = predict(model_ps, type = "response"),
                     Treatment = model_ps$model$Treatment)

head(prs_df)
labs <- paste("Group type:", c("Treatment", "Control"))

prs_df %>%
  mutate(Treatment = ifelse(Treatment== 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score,color=Treatment)) +
  geom_histogram()


data_nomiss <- df_1year %>%  
  select(rating, Treatment, one_of(data_cov)) %>%
  na.omit()
names(data_nomiss)

install.packages("MatchIt")
library(MatchIt)

mod_match <- matchit(Treatment~ln_user.rating.count+ln_lang_count+ln_price+ln_size+tenure+Adult+ln_mean_in_app_purchase,method = "nearest", data = data_nomiss, caliper = 0.1)

plot(mod_match)

dta_m <- match.data(mod_match)
dim(dta_m)

lapply(data_cov, function(v) {
  t.test(dta_m[, v] ~ dta_m$Treatment)
})

with(dta_m, t.test(rating ~ Treatment))

####taking app updation as treatment variable with non log independent variables

##### Taking app updation in past 1 year as treatment variable along with log of independent variables

df1 <- df1%>% select(-treatment)
df_1year <- df1%>%select(-Treatment)

df_1year$Treatment <- ifelse(df_1year$days_since_last_release <=365,1,0)
names(df_1year)


with(df_1year, t.test(rating ~ Treatment))

datavar <- c("User.Rating.Count","Price","Size","tenure","Adult","mean_in_app_purchase","lang_count")
lapply(datavar, function(v) {
  t.test(df_1year[, v] ~ df1$rating)
})
describeBy(df_1year)
df_1year$ln_lang_count <- log(df$lang_count+1)

#model_ps <- glm(Treatment~User.Rating.Count+ln_lang_count+ln_price+Size+tenure+days_since_last_release+Adult+mean_in_app_purchase,family=binomial(),data=df1)
#summary(model_ps)

model_ps <- glm(Treatment~User.Rating.Count+lang_count+Price+Size+tenure+Adult+mean_in_app_purchase,family=binomial(),data=df_1year)
summary(model_ps)

##model_ps2 <- glm(Treatment~ln_user.rating.count+lang_count+ln_price+Size+tenure+days_since_last_release+Adult+ln_mean_in_app_purchase,family=binomial(),data=df1)
#summary(model_ps2)

data_cov<- c("User.Rating.Count","Price","lang_count","Size","tenure", "Adult","mean_in_app_purchase")

df1 %>%
  group_by(Treatment) %>%
  select(one_of(data_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))

prs_df <- data.frame(pr_score = predict(model_ps, type = "response"),
                     Treatment = model_ps$model$Treatment)

head(prs_df)
labs <- paste("Group type:", c("Treatment", "Control"))

prs_df %>%
  mutate(Treatment = ifelse(Treatment== 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score,color=Treatment)) +
  geom_histogram()


data_nomiss <- df_1year %>%  
  select(rating, Treatment, one_of(data_cov)) %>%
  na.omit()
names(data_nomiss)

install.packages("MatchIt")
library(MatchIt)

mod_match <- matchit(Treatment~User.Rating.Count+lang_count+Price+Size+tenure+Adult+mean_in_app_purchase,method = "nearest", data = data_nomiss, caliper = 0.1)

plot(mod_match)

dta_m <- match.data(mod_match)
dim(dta_m)

lapply(data_cov, function(v) {
  t.test(dta_m[, v] ~ dta_m$Treatment)
})

with(dta_m, t.test(rating ~ Treatment))







