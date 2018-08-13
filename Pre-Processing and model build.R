###Extra FallMAP data linear regression


##All little bit of preproccessing
Lr_Ds <- subset(Extra_FallMAP_Data, select = c("Zip_Cd", "Geocode", "Tot_HH", "Car_Rte", "Med_Income", "Med_HV", "Med_Age", "BJs_Mbrs", "Past_Pds", "X1yr_RR", "Est_Non_Mbr_HHs"))


##log transform
Lr_Ds <- Lr_Ds %>% mutate(Med_Income_log = log(Med_Income + 0.01))
Lr_Ds <- Lr_Ds %>% mutate(Med_HV_log = log(Med_HV + 0.01))
Lr_Ds <- Lr_Ds %>% mutate(Med_Age_log = log(Med_Age + 0.01))
Lr_Ds <- Lr_Ds %>% mutate(BJs_Mbrs_log = log(BJs_Mbrs + 0.01))
Lr_Ds <- Lr_Ds %>% mutate(Past_Pds_log = log(Past_Pds + 0.01))
Lr_Ds <- Lr_Ds %>% mutate(X1yr_RR_log = log(X1yr_RR + 0.01))

Lr_Ds <- Lr_Ds[ , -which(names(Lr_Ds) %in% c("Est_Non_Mbr_HHs"))]
est_nons <- subset(Extra_FallMAP_Data, select = c("Geocode", "Est_Non_Mbr_HHs"))

Lr_Ds2 <- left_join(Lr_Ds, est_nons, by = "Geocode")

Lr_Ds2 <- Lr_Ds2 %>% mutate(Est_Non_Mbr_HHs_log = log(Est_Non_Mbr_HHs + 0.01))
Lr_Ds2 <- Lr_Ds2 %>% mutate(Tot_HH_log = log(Tot_HH + 0.01))

fit2 <- lm(Lr_Ds2$X1yr_RR_log ~ Lr_Ds2$Med_HV_log + Lr_Ds2$Med_Income_log+ Lr_Ds2$Med_Age_log + Lr_Ds2$Past_Pds_log + Lr_Ds2$BJs_Mbrs_log + Lr_Ds2$Est_Non_Mbr_HHs_log)
summary(fit1) 
Med_HV_x <- 103500
Med_Income_x <- 321000
Med_Age_x <- 54
Past_pds_x <- 59
BJs_Mbrs_x <- 229
Est_non_mbrs_x <- 429
RR_pred <- fit1$coefficients[1] + (Med_HV_x*(fit1$coefficients[2])) + (Med_Age_x*(fit1$coefficients[3])) + (Med_Income_x*(fit1$coefficients[4])) + (Past_pds_x*(fit1$coefficients[5]))+ (BJs_Mbrs_x*(fit1$coefficients[6])) + (Est_non_mbrs_x*(fit1$coefficients[7])) 
log(RR_pred)
ggpredict(fit1, interactive = T)

ggplot(Lr_Ds2_filtered, aes(x = Lr_Ds2_filtered$Med_Age, y = Lr_Ds2_filtered$X1yr_RR_log)) +
  geom_smooth() + 
  geom_jitter(aes(color = Lr_Ds2_filtered$Past_Pds))

library(ggplot2)
install.packages("ggeffects")
library(devtools)
library(ggiraphExtra)
###Filter out Values that are ruining the model

Lr_Ds2_filtered <- Lr_Ds2 %>%
  filter(Med_Age > 0, X1yr_RR_log > -4, Med_HV > 0, Med_Income > 0, Past_Pds >0, BJs_Mbrs >0, Est_Non_Mbr_HHs >0)
ggPredict(fit1, interactive = T)
library(ggeffects)


fit3 <- lm(Lr_Ds2_filtered$X1yr_RR_log ~ Lr_Ds2_filtered$Past_Pds_log)
ggeffect(fit3, terms =c("Lr_Ds2_filtered$X1yr_RR_log", "Lr_Ds2_filtered$Past_Pds_log"))



summary(fit3)


fit4 <- lm(Lr_Ds2_filtered$X1yr_RR_log ~ Lr_Ds2_filtered$Past_Pds + Lr_Ds2_filtered$Past_Pds*Lr_Ds2_filtered$Tot_HH + Lr_Ds2_filtered$Tot_HH)
summary(fit4)



pred <- fit4$coefficients[1] + (fit4$coefficients[4] * 22) + (fit4$coefficients[3] *  462)
pred  
  

##Not liking my model -- feature selection

library(ggplot2)
library(stats)

cormat_df <- subset(Lr_Ds2_filtered , select = c("Med_HV_log", "Med_Income_log", "Med_Age_log", "Past_Pds_log", "X1yr_RR_log", "BJs_Mbrs_log","Tot_HH_log", "Est_Non_Mbr_HHs_log"))
cormat <- cor(cormat_df)
head(cormat)
##Melt cormat
install.packages("reshape2")
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)


ggplot(melted_cormat, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile()

