#
# generate and save three models for use in the rshinyapp
#
# TODO:
# - build model on all data instead of train set
# - resolve directory problems
# - remove unnecesary code



library(readxl)
library(dplyr)
library(gridExtra)
library(caret)
library(DescTools)

user = "sherman"
#user = "sunny"
#user = "cam"
#user = "hamza"


if(user == "sherman") setwd(file.path("~/Desktop/LEARNING/M2/S1/Stat Consultin/data"))

names <- read_excel("dictionnaire.xlsx")
df <- read_excel("Base_notation.xlsx")

#EBITDA – Earnings Before Interest, Taxes, Depreciation, and Amortization
#EBE: Excedent Brut d'Exploitation (Gross Operation Surplus)

#####
#CLEANING
#######

#clean names
colnames(df) <- names$Traduction
colnames(df) <- gsub(" ", "", colnames(df), fixed=TRUE)
colnames(df) <- gsub(",", "", colnames(df), fixed=TRUE)
colnames(df) <- gsub("/", "", colnames(df), fixed=TRUE)
colnames(df) <- gsub("*", "", colnames(df), fixed=TRUE)
colnames(df) <- gsub("?", "", colnames(df), fixed=TRUE)
colnames(df)[17] <- "ebitda"
#convert to df

df <- as.data.frame(df)

#change types
df$ID <- as.factor(df$ID)
df$Status <- as.factor(df$Status)
df$Sectorofactivity <- as.factor(df$Sectorofactivity)
df$Financialrating <- as.numeric(df$Financialrating)
df$Qualitativerating <- as.numeric(df$Qualitativerating)

cols_fac <- c("Qualitativeratingaboutshareholderscontribution" ,"Qualitativeratingabouttransparency",
          "Favorableeconomicmarket","Sectorwillincrease","Managementquality","HoldbyabiggerFirm",
          "CEOinvolved","Helpfromthegrouponlegal")
df[cols_fac] <- lapply(df[cols_fac], as.factor)


cols_num <- c("Assets", "Liability", "Turnover", "ebitda",
            "Debtonequity", "grossoperatingsurplusglobalcosts",
            "grossoperatingsurplusTurover100")

df[cols_num] <- lapply(df[cols_num], as.numeric)


#extract only first two digits of NAF
df$sectorletter <- gsub('\\d[A-Z]','', as.character(df$Sectorofactivity), ignore.case=TRUE)
df$sectorletter <- as.numeric(df$sectorletter)
table(df$sectorletter)

#remove NAs
df <- df[complete.cases(df),]


#############DESCRIPTIVES


sectors_grouped <- read_excel("Copie de grouconv.xlsx")
#remove unknown rows
sectors_grouped <- sectors_grouped %>% filter(!is.na(group_name_mixed))



#convert relevant columns to correct data types
# sectors_grouped[,c("sector_no", "sector_counts")] <- sapply(sectors_grouped[,c("sector_no",
#                                                                                "sector_counts")], 
#                                                             FUN = as.numeric)


colnames(df) <- gsub("sectorletter", "sector_no", colnames(df))

#join data by sector_no
df2 <- left_join(df, sectors_grouped) %>% 
  filter(!is.na(group_name_mixed))


df2$group_name_mixed <- as.factor(df2$group_name_mixed)



group_plot <- function(cov) {
  df3 <- df2 %>% filter(group_name_mixed == cov)
  ggplot(data = df3, aes_string(y = Financialrating, x = cov)) + geom_point()
}

#remove all duplicates
df3 <- distinct(df2, Status, Sectorofactivity, Financialrating, Qualitativerating, Qualitativeratingabouttransparency, Qualitativeratingaboutshareholderscontribution,
Favorableeconomicmarket, Sectorwillincrease,
Managementquality, HoldbyabiggerFirm, CEOinvolved, 
Helpfromthegrouponlegal, Assets, Liability, Turnover, 
ebitda, Debtonequity, grossoperatingsurplusglobalcosts,
grossoperatingsurplusTurover100, .keep_all = TRUE)

####
#REGRESSION
####


set.seed(666)
train_idx <- createDataPartition(df2$Qualitativerating,p=0.8,list=FALSE)
training <- df2[train_idx,]
test <- df2[-train_idx,]

lm_quali<- lm(Qualitativerating ~ Qualitativeratingaboutshareholderscontribution
                + Qualitativeratingabouttransparency
                + Favorableeconomicmarket
                + Sectorwillincrease
                + Managementquality
                + HoldbyabiggerFirm
                + CEOinvolved
                + Helpfromthegrouponlegal, data = training)
#library(xtable)
# print(xtable(lm_quali, type = "latex", file = "quali_output.tex"))


quali_plot<- function(u){ 
                ggplot(training, aes_string(y = "Qualitativerating", x = u)) +
                        geom_point(aes(y = lm_quali$fitted.values)) 

}

plot_contribu <- quali_plot("Qualitativeratingaboutshareholderscontribution")
plot_transp <- quali_plot("Qualitativeratingabouttransparency")
plot_fav <- quali_plot("Favorableeconomicmarket")
plot_sec <- quali_plot("Sectorwillincrease")
plot_mana <- quali_plot("Managementquality")
plot_Hold <- quali_plot("HoldbyabiggerFirm")
plot_ceo <- quali_plot("CEOinvolved")
plot_help <- quali_plot("Helpfromthegrouponlegal")


pred_quali <- predict(lm_quali, test)


grid.arrange(plot_contribu, plot_transp,plot_fav, plot_sec, plot_mana, plot_Hold, plot_ceo, plot_help)


### Financial Rating


#define new columns for zero values across covariates

df3 <- df3 %>% mutate(zero_tover = if_else(Turnover == 0, 1, 0),
                      zero_ebitda = if_else(ebitda == 0, 1, 0),
                      zero_doe = if_else(Debtonequity == 0, 1, 0),
                      zero_gos = if_else(grossoperatingsurplusglobalcosts == 0,1,0),
                      zero_gos_100 = if_else(grossoperatingsurplusTurover100 == 0,1,0)
                      )


df3_industrie <- df3 %>% filter(group_name_mixed == "Industrie")

#removed the last 4 covariates since they are not statistically significant
ind_lm <- lm(Financialrating ~ Turnover + zero_tover + ebitda + zero_ebitda + Debtonequity + zero_doe, data = df3_industrie)

summary(ind_lm)

x <- predict(ind_lm)

library(mgcv)

ind_gam <- gam(Financialrating ~ s(Turnover) + zero_tover + s(ebitda) + zero_ebitda + s(Debtonequity) + zero_doe + grossoperatingsurplusglobalcosts + zero_gos_100 +  grossoperatingsurplusTurover100+ zero_gos_100, data = df3_industrie)

summary(ind_gam)
cbind(AIC(ind_lm), AIC(ind_gam))
#we can see that the GAM performs much worse than the linear model



#perform winsorisation 
library(DescTools)

upper_quantile <- quantile(df3_industrie$Turnover, 0.95)
# lower_quantile <- quantile(df3_industrie$Turnover, 0.05)

df3_industrie$Turnover[df3_industrie$Turnover > upper_quantile] <- NA
# df3_industrie$Turnover[df3_industrie$Turnover < lower_quantile] <- NA

df3_industrie <- df3_industrie %>% filter(!is.na(Turnover))

#take log transformation
df3_industrie$Turnover <- if_else(df3_industrie$Turnover < 0, 0, df3_industrie$Turnover)

df3_industrie$Turnover <- if_else(df3_industrie$Turnover > 0, log(df3_industrie$Turnover), df3_industrie$Turnover)

ggplot(df3_industrie, aes(x = Turnover, y = Financialrating)) + geom_point()

#now adjusted R^2 = 0.67
ind_gam <- gam(Financialrating ~ s(Turnover) + zero_tover, data = df3_industrie)
summary(ind_gam)

#remove extremes
upper_quantile <- quantile(df3_industrie$ebitda, 0.95)
df3_industrie$ebitda[df3_industrie$ebitda > upper_quantile] <- NA

df3_industrie <- df3_industrie %>% filter(!is.na(ebitda))

#treat negative values as zeros
df3_industrie$ebitda <- if_else(df3_industrie$ebitda < 0, 0, df3_industrie$ebitda)
df3_industrie$zero_ebitda <- if_else(df3_industrie$ebitda == 0, 1, 0)

#now adjusted R-squared is 0.733
ind_gam <- gam(Financialrating ~ s(Turnover) + zero_tover + s(ebitda) + zero_ebitda, data = df3_industrie)
summary(ind_gam)

upper_quantile <- quantile(df3_industrie$Debtonequity, 0.95)
df3_industrie$Debtonequity[df3_industrie$Debtonequity > upper_quantile] <- NA

df3_industrie <- df3_industrie %>% filter(!is.na(Debtonequity))


df3_industrie$Debtonequity <- if_else(df3_industrie$Debtonequity < 0, 0, df3_industrie$Debtonequity)

df3_industrie$zero_doe <- if_else(df3_industrie$Debtonequity == 0, 1, 0)

ind_gam <- gam(Financialrating ~ s(Turnover) + zero_tover + s(ebitda) + zero_ebitda + s(Debtonequity) + zero_doe, data = df3_industrie)

summary(ind_gam)

upper_quantile <- quantile(df3_industrie$grossoperatingsurplusglobalcosts, 0.95)
df3_industrie$grossoperatingsurplusglobalcosts[df3_industrie$grossoperatingsurplusglobalcosts > upper_quantile] <- NA

df3_industrie <- df3_industrie %>% filter(!is.na(grossoperatingsurplusglobalcosts))


df3_industrie$grossoperatingsurplusglobalcosts <- if_else(df3_industrie$grossoperatingsurplusglobalcosts < 0, 0, df3_industrie$grossoperatingsurplusglobalcosts)


ind_gam <- gam(Financialrating ~ s(Turnover) + zero_tover + s(ebitda) + zero_ebitda + s(Debtonequity) + zero_doe + s(grossoperatingsurplusglobalcosts) + zero_gos,data = df3_industrie)
summary(ind_gam)


upper_quantile <- quantile(df3_industrie$grossoperatingsurplusTurover100, 0.95)
df3_industrie$grossoperatingsurplusTurover100[df3_industrie$grossoperatingsurplusTurover100 > upper_quantile] <- NA

df3_industrie <- df3_industrie %>% filter(!is.na(grossoperatingsurplusTurover100))


df3_industrie$grossoperatingsurplusTurover100 <- if_else(df3_industrie$grossoperatingsurplusTurover100 < 0, 0, df3_industrie$grossoperatingsurplusTurover100)


ind_gam <- gam(Financialrating ~ s(Turnover) + zero_tover + s(ebitda) + zero_ebitda + s(Debtonequity) + zero_doe + s(grossoperatingsurplusglobalcosts) + zero_gos +  s(grossoperatingsurplusTurover100) + zero_gos_100 , data = df3_industrie)


## Conseil Droit


remove_outlier <- function(data, covariate, lower = "false") {
  df4 <- data
    upper_quantile <- quantile(df4[, covariate], probs = 0.95)
  
    df4[, covariate][df4[,covariate] > upper_quantile] <- NA
    
    df4 <- df4 %>% filter(!is.na(eval(parse(text = covariate))))
    
    if(lower == "true") {
    lower_quantile <- quantile(df4[, covariate], probs = 0.05)
  
    df4[, covariate][df4[,covariate] < lower_quantile] <- NA
    
    df4 <- df4 %>% filter(!is.na(eval(parse(text = covariate))))
    }
    return(df4)
}


treat_zeros <- function(data, covariate, ln = "true",
                        negative = "false") {
   df4 <- data
    #create zero indicator
  var_name <- paste("zero", covariate, sep = "_")
    df4 <- df4 %>% mutate("zero.{covariate}"  := if_else(eval(parse(text = covariate)) == 0, 1, 0))
    
    #treat negative values as zeros
    df4[, covariate] <- if_else(df4[,covariate] < 0, 0, df4[, covariate])
    
    if(ln == "true") {
    #take log for positive values
    df4[,covariate] <- if_else(df4[, covariate] > 0, log(df4[, covariate]), df4[, covariate])
    }
    if(negative == "true") {
    df4[, covariate] <- if_else(df4[ covariate] < 0,
                               (-1)*log(abs(df4[, covariate])), df4[, covariate])
    }
    return(df4)
}

df3_conseil <- df3 %>% filter(group_name_mixed == "Conseil droit") %>% remove_outlier("Turnover") %>% treat_zeros("Turnover") %>% remove_outlier("ebitda") %>% 
  treat_zeros("ebitda") %>% remove_outlier("Debtonequity") %>% treat_zeros("Debtonequity", ln = "false") %>% 
  remove_outlier("grossoperatingsurplusglobalcosts") %>%
  treat_zeros("grossoperatingsurplusglobalcosts") %>%
  remove_outlier("grossoperatingsurplusTurover100",
               lower = "true") %>% 
  treat_zeros("grossoperatingsurplusTurover100",
              ln = "false")

con_gam <- gam(Financialrating ~ s(Turnover) + zero.Turnover + s(ebitda) + zero.ebitda + s(Debtonequity) + 
                 zero.Debtonequity + 
                 s(grossoperatingsurplusglobalcosts) +
                 zero.grossoperatingsurplusglobalcosts +
                 s(grossoperatingsurplusTurover100) +
                 zero.grossoperatingsurplusTurover100,
             data = df3_conseil)

summary(con_gam)

#df3_industrie
df4 <- df3_industrie

df4$Turnover <- df4$Turnover * 0.7

temp <- predict.gam(ind_gam, df4)
plot( df3_industrie$Turnover - df4$Turnover,
                  predict.gam(ind_gam, df3_industrie) - temp)

#df3_industrie
df4 <- df3_industrie

df4$grossoperatingsurplusglobalcosts <- df4$grossoperatingsurplusglobalcosts * 0.7

temp <- predict.gam(ind_gam, df4)
plot(df3_industrie$grossoperatingsurplusglobalcosts - df4$grossoperatingsurplusglobalcosts, 
                 predict.gam(ind_gam, df3_industrie) - temp)

#df3_industrie
df4 <- df3_industrie

df4$ebitda <- df4$ebitda * 0.7

temp <- predict.gam(ind_gam, df4)


#df3_industrie
df4 <- df3_industrie

df4$grossoperatingsurplusTurover100 <- df4$grossoperatingsurplusTurover100 * 0.7

temp <- predict.gam(ind_gam, df4)
plot(df3_industrie$grossoperatingsurplusTurover100 - df4$grossoperatingsurplusTurover100, 
                 predict.gam(ind_gam, df3_industrie) - temp)

#df3_industrie
df4 <- df3_industrie

df4$Debtonequity <- df4$Debtonequity * 1.5

temp <- predict.gam(ind_gam, df4)
plot(df3_industrie$Debtonequity - df4$Debtonequity, 
                 predict.gam(ind_gam, df3_industrie) - temp)


###############
# SAVE MODELS #
###############
#
#saveRDS(object, file = "")
#
#readRDS(file, refhook = NULL)
###############

#qualitative model
saveRDS(lm_quali, file="quali.RData")
#financial - industrie
saveRDS(ind_gam, file="ind_gam.RData")
#financial - conseil
saveRDS(con_gam, file="con_gam.RData")