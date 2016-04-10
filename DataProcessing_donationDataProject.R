############################################################
# Data Processing for Data Warehousing Project (Dec 2015)  #
# (Code Processing Snippets)                               #
############################################################
#EXPLANATION#
#DonationFact Table (accumulating snapshot)
#If all 23 promos were mailed to all donors
# 23 promos * 95412 unique donors = 2,194,476 rows
#We can omit one promo 97NK (incomplete RDATE info)
# 22 promos * 95412 unique donors = 2,099,064 rows
#11-28: Omit NAs based on MailDate
# 1,559,430 rows

#DonorHistoryFact Table (snapshot)
#If all 23 promos were mailed to all donors
# 23 promos * 95412 unique donors = 2,194,476 rows
#We can omit one promo 97NK (incomplete RDATE info)
# 22 promos * 95412 unique donors = 2,099,064 rows
#11-28: Omit NAs based on MailDate
# 1,559,430 rows

#DimCampaign
# 11-28: 22 campaigns kept (excluded 97NK)


#DimTime 
#Based on Campaigns (ignore DOB dates)
# Years = 1993- 1998 (earliest and latest mail years--see DimTime table creation)=6years
# Months= 01 - 12
# 6 years * 12 months = 72 YYMM time combinations

#DimDonor
#95,412 unique donors / rows (fromo original table)


#DimCensusData
#11-28: Find unique combinations of pre-selected variables (29 total) 
# Total: 61,578 (similar but a bit less than previous)


#DimMailOrder (if we choose to keep)-NOT USED
#14 rows

#MailOrderFact (if we choose to keep)--NOT USED
#14 MailOrderTypes * 95412 unique donors = 1,335,768 rows (if none are NA)-NOT USED



###Creating the FACT tables
#1. Subset columns for each campaign (subset)
#2. Change headers
#3. Rbind!

#################
# 1: Subset cols
#################
setwd("~/R")
data = read.csv("DonorDataV3.csv", header=TRUE)

colnames(data)
#Note:DonorID and CensusKeys are keys


#DonorID, (CensusKey), PromotionID=94NK, ADATE_24, RDATE_24, RAMNT_24

sub24 = subset(data,  select = c(DonorID, CensusKey, ADATE_24, RDATE_24, RAMNT_24))
pName24 = as.data.frame(rep("94NK",nrow(sub24))) #Repeat promotion label
sub24 = cbind(sub24, pName24)
colnames(sub24) = c("DonorID","CensusKey","MailDate","DonateDate","DonationAmnt","promoName")

#DonorID, (CensusID), PromotionID=94FS, ADATE_23, RDATE_23, RAMNT_23
sub23 = subset(data,  select = c(DonorID, CensusKey, ADATE_23, RDATE_23, RAMNT_23))
pName23 = as.data.frame(rep("94FS",nrow(sub23))) #Repeat promotion label
sub23 = cbind(sub23, pName23)
colnames(sub23) = c("DonorID","CensusKey","MailDate","DonateDate","DonationAmnt","promoName")

#DonorID, (CensusID), PromotionID=95XK, ADATE_22, RDATE_22, RAMNT_22
sub22 = subset(data,  select = c(DonorID, CensusKey, ADATE_22, RDATE_22, RAMNT_22))
pName22 = as.data.frame(rep("95XK",nrow(sub22))) #Repeat promotion label
sub22 = cbind(sub22, pName22)
colnames(sub22) = c("DonorID","CensusKey","MailDate","DonateDate","DonationAmnt","promoName")


#DonorID, (CensusID), PromotionID=95X1, ADATE_21, RDATE_21, RAMNT_21
sub21 = subset(data,  select = c(DonorID, CensusKey, ADATE_21, RDATE_21, RAMNT_21))
pName21 = as.data.frame(rep("95X1",nrow(sub21))) #Repeat promotion label
sub21 = cbind(sub21, pName21)
colnames(sub21) = c("DonorID","CensusKey","MailDate","DonateDate","DonationAmnt","promoName")



#DonorID, (CensusID), PromotionID=95WL, ADATE_20, RDATE_20, RAMNT_20
sub20 = subset(data,  select = c(DonorID, CensusKey, ADATE_20, RDATE_20, RAMNT_20))
pName20 = as.data.frame(rep("95WL",nrow(sub20))) #Repeat promotion label
sub20 = cbind(sub20, pName20)
colnames(sub20) = c("DonorID","CensusKey","MailDate","DonateDate","DonationAmnt","promoName")



#DonorID, (CensusID), PromotionID=95CC, ADATE_19, RDATE_19, RAMNT_19
sub19 = subset(data,  select = c(DonorID, CensusKey, ADATE_19, RDATE_19, RAMNT_19))
pName19 = as.data.frame(rep("95CC",nrow(sub19))) #Repeat promotion label
sub19 = cbind(sub19, pName19)
colnames(sub19) = c("DonorID","CensusKey","MailDate","DonateDate","DonationAmnt","promoName")

#DonorID, (CensusID), PromotionID=95GK, ADATE_18, RDATE_18, RAMNT_18
sub18 = subset(data,  select = c(DonorID, CensusKey, ADATE_18, RDATE_18, RAMNT_18))
pName18 = as.data.frame(rep("95GK",nrow(sub18))) #Repeat promotion label
sub18 = cbind(sub18, pName18)
colnames(sub18) = c("DonorID","CensusKey","MailDate","DonateDate","DonationAmnt","promoName")


#DonorID, (CensusID), PromotionID=95G1, ADATE_17, RDATE_17, RAMNT_17
sub17 = subset(data,  select = c(DonorID, CensusKey, ADATE_17, RDATE_17, RAMNT_17))
pName17 = as.data.frame(rep("95G1",nrow(sub17))) #Repeat promotion label
sub17 = cbind(sub17, pName17)
colnames(sub17) = c("DonorID","CensusKey","MailDate","DonateDate","DonationAmnt","promoName")


#DonorID, (CensusID), PromotionID=95LL, ADATE_16, RDATE_16, RAMNT_16
sub16 = subset(data,  select = c(DonorID, CensusKey, ADATE_16, RDATE_16, RAMNT_16))
pName16 = as.data.frame(rep("95LL",nrow(sub16))) #Repeat promotion label
sub16 = cbind(sub16, pName16)
colnames(sub16) = c("DonorID","CensusKey","MailDate","DonateDate","DonationAmnt","promoName")


#DonorID, (CensusID), PromotionID=95TK, ADATE_15, RDATE_15, RAMNT_15
sub15 = subset(data,  select = c(DonorID,CensusKey, ADATE_15, RDATE_15, RAMNT_15))
pName15 = as.data.frame(rep("95TK",nrow(sub15))) #Repeat promotion label
sub15 = cbind(sub15, pName15)
colnames(sub15) = c("DonorID","CensusKey","MailDate","DonateDate","DonationAmnt","promoName")


#DonorID, (CensusID), PromotionID=95NK, ADATE_14, RDATE_14, RAMNT_14
sub14 = subset(data,  select = c(DonorID, CensusKey, ADATE_14, RDATE_14, RAMNT_14))
pName14 = as.data.frame(rep("95NK",nrow(sub14))) #Repeat promotion label
sub14 = cbind(sub14, pName14)
colnames(sub14) = c("DonorID","CensusKey","MailDate","DonateDate","DonationAmnt","promoName")



#DonorID, (CensusID), PromotionID=95FS, ADATE_13, RDATE_13, RAMNT_13
sub13 = subset(data,  select = c(DonorID, CensusKey, ADATE_13, RDATE_13, RAMNT_13))
pName13 = as.data.frame(rep("95FS",nrow(sub13))) #Repeat promotion label
sub13 = cbind(sub13, pName13)
colnames(sub13) = c("DonorID","CensusKey","MailDate","DonateDate","DonationAmnt","promoName")



#DonorID, (CensusID), PromotionID=96XK, ADATE_12, RDATE_12, RAMNT_12
sub12 = subset(data,  select = c(DonorID, CensusKey, ADATE_12, RDATE_12, RAMNT_12))
pName12 = as.data.frame(rep("96XK",nrow(sub12))) #Repeat promotion label
sub12 = cbind(sub12, pName12)
colnames(sub12) = c("DonorID","CensusKey","MailDate","DonateDate","DonationAmnt","promoName")



#DonorID, (CensusID), PromotionID=96X1, ADATE_11, RDATE_11, RAMNT_11
sub11 = subset(data,  select = c(DonorID, CensusKey, ADATE_11, RDATE_11, RAMNT_11))
pName11 = as.data.frame(rep("96X1",nrow(sub11))) #Repeat promotion label
sub11 = cbind(sub11, pName11)
colnames(sub11) = c("DonorID","CensusKey","MailDate","DonateDate","DonationAmnt","promoName")



#DonorID, (CensusID), PromotionID=96WL, ADATE_10, RDATE_10, RAMNT_10
sub10= subset(data,  select = c(DonorID, CensusKey, ADATE_10, RDATE_10, RAMNT_10))
pName10 = as.data.frame(rep("96WL",nrow(sub10))) #Repeat promotion label
sub10 = cbind(sub10, pName10)
colnames(sub10) = c("DonorID","CensusKey","MailDate","DonateDate","DonationAmnt","promoName")


#DonorID, (CensusID), PromotionID=96CC, ADATE_9, RDATE_9, RAMNT_9
sub9 = subset(data,  select = c(DonorID, CensusKey, ADATE_9, RDATE_9, RAMNT_9))
pName9 = as.data.frame(rep("96CC",nrow(sub9))) #Repeat promotion label
sub9 = cbind(sub9, pName9)
colnames(sub9) = c("DonorID","CensusKey","MailDate","DonateDate","DonationAmnt","promoName")


#DonorID, (CensusID), PromotionID=96GK, ADATE_8, RDATE_8, RAMNT_8
sub8 = subset(data,  select = c(DonorID, CensusKey, ADATE_8, RDATE_8, RAMNT_8))
pName8 = as.data.frame(rep("96GK",nrow(sub8))) #Repeat promotion label
sub8 = cbind(sub8, pName8)
colnames(sub8) = c("DonorID","CensusKey","MailDate","DonateDate","DonationAmnt","promoName")


#DonorID, (CensusID), PromotionID=96G1, ADATE_7, RDATE_7, RAMNT_7
sub7 = subset(data,  select = c(DonorID, CensusKey, ADATE_7, RDATE_7, RAMNT_7))
pName7 = as.data.frame(rep("96G1",nrow(sub7))) #Repeat promotion label
sub7 = cbind(sub7, pName7)
colnames(sub7) = c("DonorID","CensusKey","MailDate","DonateDate","DonationAmnt","promoName")


#DonorID, (CensusID), PromotionID=96LL, ADATE_6, RDATE_6, RAMNT_6
sub6 = subset(data,  select = c(DonorID, CensusKey, ADATE_6, RDATE_6, RAMNT_6))
pName6 = as.data.frame(rep("96LL",nrow(sub6))) #Repeat promotion label
sub6 = cbind(sub6, pName6)
colnames(sub6) = c("DonorID","CensusKey","MailDate","DonateDate","DonationAmnt","promoName")


#DonorID, (CensusID), PromotionID=96SK, ADATE_5, RDATE_5, RAMNT_5
sub5 = subset(data,  select = c(DonorID, CensusKey, ADATE_5, RDATE_5, RAMNT_5))
pName5 = as.data.frame(rep("96SK",nrow(sub5))) #Repeat promotion label
sub5 = cbind(sub5, pName5)
colnames(sub5) = c("DonorID","CensusKey","MailDate","DonateDate","DonationAmnt","promoName")


#DonorID, (CensusID), PromotionID=96TK, ADATE_4, RDATE_4, RAMNT_4
sub4 = subset(data,  select = c(DonorID, CensusKey, ADATE_4, RDATE_4, RAMNT_4))
pName4 = as.data.frame(rep("96TK",nrow(sub4))) #Repeat promotion label
sub4 = cbind(sub4, pName4)
colnames(sub4) = c("DonorID","CensusKey","MailDate","DonateDate","DonationAmnt","promoName")


#DonorID, (CensusID), PromotionID=96NK, ADATE_3, RDATE_3, RAMNT_3
sub3 = subset(data,  select = c(DonorID, CensusKey, ADATE_3, RDATE_3, RAMNT_3))
pName3 = as.data.frame(rep("96NK",nrow(sub3))) #Repeat promotion label
sub3 = cbind(sub3, pName3)
colnames(sub3) = c("DonorID","CensusKey","MailDate","DonateDate","DonationAmnt","promoName")

###Column 
donationFact = rbind(sub3, sub4, sub5, sub6,  sub7,  sub8,  sub9,  sub10, sub11, sub12, sub13, sub14,
      sub15, sub16, sub17, sub18,  sub19,  sub20,  sub21,  sub22, sub23, sub24)

###Check for NAs
sum(is.na(donationFact$MailDate))
#539,634
sum(is.na(donationFact$DonateDate))
#1,821,004

donationFact$promoName
###DonorID, (CensusID), PromotionID=97NK, ADATE_2, RDATE_x, RAMNT_x

as.data.frame(table(donationFact$promoName))
#22 different promotions

#With NAs
write.csv(donationFact, "FactDonationWithNAs.csv")


#With no NAs
donationFact2 = donationFact[!is.na(donationFact$MailDate),]

write.csv(donationFact2, "FactDonation.csv")

#Get date & promo key later --->

#################################################################
#Creating the DONOR SNAPSHOT FACT table (restructure tables)    #
#1. Subset columns for each campaign (subset)                   #
#2. Change headers                                              #
#3. Rbind!                                                      #
#################################################################

#DonorID, CensusID, PromotionID 94NK, ADATE_24, RFA_24
rsub24 = subset(data,  select = c(DonorID, CensusKey, ADATE_24, RFA_24))
rpName24 = as.data.frame(rep("94NK",nrow(rsub24))) #Repeat promotion label
rsub24 = cbind(rsub24, rpName24)
colnames(rsub24) = c("DonorID","CensusKey","MailDate","RFA","promoName")


#DonorID, CensusID, PromotionID 94FS, ADATE_23, RFA_23
rsub23 = subset(data,  select = c(DonorID, CensusKey, ADATE_23, RFA_23))
rpName23 = as.data.frame(rep("94FS",nrow(rsub23))) #Repeat promotion label
rsub23 = cbind(rsub23, rpName23)
colnames(rsub23) = c("DonorID","CensusKey","MailDate","RFA","promoName")


#DonorID, CensusID, PromotionID 95XK, ADATE_22, RFA_22
rsub22 = subset(data,  select = c(DonorID,CensusKey, ADATE_22, RFA_22))
rpName22 = as.data.frame(rep("95XK",nrow(rsub22))) #Repeat promotion label
rsub22 = cbind(rsub22, rpName22)
colnames(rsub22) = c("DonorID","CensusKey","MailDate","RFA","promoName")


#DonorID, CensusID, PromotionID 95X1, ADATE_21, RFA_21
rsub21 = subset(data,  select = c(DonorID, CensusKey, ADATE_21, RFA_21))
rpName21 = as.data.frame(rep("95X1",nrow(rsub21))) #Repeat promotion label
rsub21 = cbind(rsub21, rpName21)
colnames(rsub21) = c("DonorID","CensusKey","MailDate","RFA","promoName")


#DonorID, CensusID, PromotionID 95WL, ADATE_20, RFA_20
rsub20 = subset(data,  select = c(DonorID,CensusKey, ADATE_20, RFA_20))
rpName20 = as.data.frame(rep("95WL",nrow(rsub20))) #Repeat promotion label
rsub20 = cbind(rsub20, rpName20)
colnames(rsub20) = c("DonorID","CensusKey","MailDate","RFA","promoName")


#DonorID, CensusID, PromotionID 95CC, ADATE_19, RFA_19
rsub19 = subset(data,  select = c(DonorID, CensusKey, ADATE_19, RFA_19))
rpName19 = as.data.frame(rep("95CC",nrow(rsub19))) #Repeat promotion label
rsub19 = cbind(rsub19, rpName19)
colnames(rsub19) = c("DonorID","CensusKey","MailDate","RFA","promoName")


#DonorID, CensusID, PromotionID 95GK, ADATE_18, RFA_18
rsub18 = subset(data,  select = c(DonorID, CensusKey, ADATE_18, RFA_18))
rpName18 = as.data.frame(rep("95GK",nrow(rsub18))) #Repeat promotion label
rsub18 = cbind(rsub18, rpName18)
colnames(rsub18) = c("DonorID","CensusKey","MailDate","RFA","promoName")


#DonorID, CensusID, PromotionID 95G1, ADATE_17, RFA_17
rsub17 = subset(data,  select = c(DonorID, CensusKey, ADATE_17, RFA_17))
rpName17 = as.data.frame(rep("95G1",nrow(rsub17))) #Repeat promotion label
rsub17 = cbind(rsub17, rpName17)
colnames(rsub17) = c("DonorID","CensusKey","MailDate","RFA","promoName")


#DonorID, CensusID, PromotionID 95LL, ADATE_16, RFA_16
rsub16 = subset(data,  select = c(DonorID, CensusKey, ADATE_16, RFA_16))
rpName16 = as.data.frame(rep("95LL",nrow(rsub16))) #Repeat promotion label
rsub16 = cbind(rsub16, rpName16)
colnames(rsub16) = c("DonorID","CensusKey","MailDate","RFA","promoName")


#DonorID, CensusID, PromotionID 95TK, ADATE_15, RFA_15
rsub15 = subset(data,  select = c(DonorID, CensusKey, ADATE_15, RFA_15))
rpName15 = as.data.frame(rep("95TK",nrow(rsub15))) #Repeat promotion label
rsub15 = cbind(rsub15, rpName15)
colnames(rsub15) = c("DonorID","CensusKey","MailDate","RFA","promoName")


#DonorID, CensusID, PromotionID 95NK, ADATE_14, RFA_14
rsub14 = subset(data,  select = c(DonorID, CensusKey, ADATE_14, RFA_14))
rpName14 = as.data.frame(rep("95NK",nrow(rsub14))) #Repeat promotion label
rsub14 = cbind(rsub14, rpName14)
colnames(rsub14) = c("DonorID","CensusKey","MailDate","RFA","promoName")


#DonorID, CensusID, PromotionID 95FS, ADATE_13, RFA_13
rsub13 = subset(data,  select = c(DonorID, CensusKey, ADATE_13, RFA_13))
rpName13 = as.data.frame(rep("95FS",nrow(rsub13))) #Repeat promotion label
rsub13 = cbind(rsub13, rpName13)
colnames(rsub13) = c("DonorID","CensusKey","MailDate","RFA","promoName")


#DonorID, CensusID, PromotionID 96XK, ADATE_12, RFA_12
rsub12 = subset(data,  select = c(DonorID, CensusKey, ADATE_12, RFA_12))
rpName12 = as.data.frame(rep("96XK",nrow(rsub12))) #Repeat promotion label
rsub12 = cbind(rsub12, rpName12)
colnames(rsub12) = c("DonorID","CensusKey","MailDate","RFA","promoName")


#DonorID, CensusID, PromotionID 96X1, ADATE_11, RFA_11
rsub11 = subset(data,  select = c(DonorID, CensusKey, ADATE_11, RFA_11))
rpName11 = as.data.frame(rep("96X1",nrow(rsub11))) #Repeat promotion label
rsub11 = cbind(rsub11, rpName11)
colnames(rsub11) = c("DonorID","CensusKey","MailDate","RFA","promoName")


#DonorID, CensusID, PromotionID 96WL, ADATE_10, RFA_10
rsub10 = subset(data,  select = c(DonorID, CensusKey, ADATE_10, RFA_10))
rpName10 = as.data.frame(rep("96WL",nrow(rsub10))) #Repeat promotion label
rsub10 = cbind(rsub10, rpName10)
colnames(rsub10) = c("DonorID","CensusKey","MailDate","RFA","promoName")


#DonorID, CensusID, PromotionID 96CC, ADATE_9, RFA_9
rsub9 = subset(data,  select = c(DonorID, CensusKey, ADATE_9, RFA_9))
rpName9 = as.data.frame(rep("96CC",nrow(rsub9))) #Repeat promotion label
rsub9 = cbind(rsub9, rpName9)
colnames(rsub9) = c("DonorID","CensusKey","MailDate","RFA","promoName")


#DonorID, CensusID, PromotionID 96GK, ADATE_8, RFA_8
rsub8 = subset(data,  select = c(DonorID, CensusKey, ADATE_8, RFA_8))
rpName8 = as.data.frame(rep("96GK",nrow(rsub8))) #Repeat promotion label
rsub8 = cbind(rsub8, rpName8)
colnames(rsub8) = c("DonorID","CensusKey","MailDate","RFA","promoName")

#DonorID, CensusID, PromotionID 96G1, ADATE_7, RFA_7
rsub7 = subset(data,  select = c(DonorID, CensusKey, ADATE_7, RFA_7))
rpName7 = as.data.frame(rep("96G1",nrow(rsub7))) #Repeat promotion label
rsub7 = cbind(rsub7, rpName7)
colnames(rsub7) = c("DonorID","CensusKey","MailDate","RFA","promoName")


#DonorID, CensusID, PromotionID 96LL, ADATE_6, RFA_6
rsub6 = subset(data,  select = c(DonorID, CensusKey, ADATE_6, RFA_6))
rpName6 = as.data.frame(rep("96LL",nrow(rsub6))) #Repeat promotion label
rsub6 = cbind(rsub6, rpName6)
colnames(rsub6) = c("DonorID","CensusKey","MailDate","RFA","promoName")


#DonorID, CensusID, PromotionID 96SK, ADATE_5, RFA_5
rsub5 = subset(data,  select = c(DonorID, CensusKey, ADATE_5, RFA_5))
rpName5 = as.data.frame(rep("96SK",nrow(rsub5))) #Repeat promotion label
rsub5 = cbind(rsub5, rpName5)
colnames(rsub5) = c("DonorID","CensusKey","MailDate","RFA","promoName")


#DonorID, CensusID, PromotionID 96TK, ADATE_4, RFA_4
rsub4 = subset(data,  select = c(DonorID, CensusKey, ADATE_4, RFA_4))
rpName4 = as.data.frame(rep("96TK",nrow(rsub4))) #Repeat promotion label
rsub4 = cbind(rsub4, rpName4)
colnames(rsub4) = c("DonorID","CensusKey","MailDate","RFA","promoName")

#DonorID, CensusID, PromotionID 96NK, ADATE_3, RFA_3
rsub3 = subset(data,  select = c(DonorID, CensusKey, ADATE_3, RFA_3))
rpName3 = as.data.frame(rep("96NK",nrow(rsub3))) #Repeat promotion label
rsub3 = cbind(rsub3, rpName3)
colnames(rsub3) = c("DonorID","CensusKey","MailDate","RFA","promoName")

donorHistFact = rbind(rsub3, rsub4, rsub5, rsub6,  rsub7,  rsub8,  rsub9,  rsub10, rsub11, rsub12, rsub13, rsub14,
                     rsub15, rsub16, rsub17, rsub18,  rsub19,  rsub20,  rsub21,  rsub22, rsub23, rsub24)

sum(is.na(donorHistFact$MailDate))
#539,634

as.data.frame(table(donorHistFact$promoName))
#22 different promotions

#WITH NAs
write.csv(donorHistFact, "donorHistFactWithNA.csv")


#Without NA's
donorHistFact2 = donorHistFact[!is.na(donationFact$MailDate),]

write.csv(donorHistFact2, "donorHistoryFact.csv")

#Get date & promo key later --->

###########################################################
#Creating CensusData                                      #
###########################################################
dimCensus = data[,c(77:79,83,84,101,104,107,132:137,147,148,200:204,291:298)]
colnames(dimCensus)

uniqueCensus = as.data.frame(unique(dimCensus)) #similar 61,578 combinations even with reduced variables (29)
write.csv(uniqueCensus, "dimCensus.csv")

#Get Census Key (match donor census conmbination with uniqueCensus)
library(prodlim)
classify = row.match(dimCensus, uniqueCensus)
classify = as.data.frame(classify) #Provides keys on Data end
#Change column name
colnames(classify) = "CensusKey"
#Append column to original dataset
data = cbind(data,classify)
#Write out dataset and Check column presence
write.csv(data, "DonorDataV4.csv")


#Adding CensusLabel
uniqueCensus = dimCensus
uniqTab = uniqueCensus
uniqTab=uniqTab[, c(-1,-2)]
subData = data[, 77:298]
match = subData

##Classification from to 61751
library(prodlim)
classify = row.match(match, uniqTab)
classify = as.data.frame(classify)
#Check number of unique classifications
nrow(unique(classify))
#Change column name
colnames(classify) = "CensusID"
#Append column to original dataset
data = cbind(data,classify)
#Check present of classify column
data$CensusID
#Write out dataset and Check column presence
write.csv(data, "DonorDataV3.csv")


#MSA, ADI, DMA unique combinations?
nrow(unique(data[197:199])) 
g =data[197:199]

##PREVIOUS COUNTING OF CENSUS
##subData = data[, 77:298]
##check column names -->  
##colnames(subData)

##library(plyr)
##count(subData)

##61,751 unique combinations (based on index)
#uniqueCensus = unique(subData)
#uniqueCensus = as.data.frame(uniqueC1)

######
#TEST#
######
#DonorID, (CensusID), PromotionID 94NK, ADATE_24, RDATE_24, RAMNT_24
dataA = subset(data, select = c(DonorID, ADATE_24, RDATE_24, RAMNT_24))
#DonorID, (CensusID), PromotionID 94FS, ADATE_23, RDATE_23, RAMNT_23
dataB = subset(data, select = c(DonorID, ADATE_23, RDATE_23, RAMNT_23))

colnames(dataA) <- c("DonorID", "ADATE", "RDATE", "RAMNT")
colnames(dataB) <- c("DonorID", "ADATE", "RDATE", "RAMNT")

dataCom =rbind(dataA, dataB)

#Test Result: Successful. Can successfully transform data by donor, campaign. 

#NOTE: Will need to delete null values later. (Null values -> ADATE_x = NA (RDATE and RAMNT = NA))

##################################
#Donor Dimension Table 11/28     #
##################################

# Note: Spilt #16 to urbanicity and SES code, Excluded HIT because that is a snapshot fact... 
# only for 1997 and for Mail Orders which we excluded
donorDim = data[,c(1:6,9,16,20,25:27,53:54, 57:74)]
colnames(donorDim) #Check variables extracted

#Write out dataset and Check column presence
write.csv(donorDim, "DimDonor.csv")
#Changes made to file:
#1.Eliminated NAs
#2.Separated Domain for Urbanacity and SES
#3. There was not a DTitle code. Wrote the following if statement in excel==that did not work because it used over 64 lvls of nesting. Will have to do with lookup codes
#=IF(E2=0,"", IF(E2=1,"MR.",IF(E2=1001,"MESSRS.",IF(E2=1002,"MR. & MRS.",IF(E2=2,"MRS.",IF(E2=2002,"MESDAMES",IF(E2=3, "MISS",IF(E2=3003,"MISSES",IF(E2=4,"DR.",IF(E2=4002,"DR. & MRS.",IF(E2=4004,"DOCTORS",IF(E2=5,"MADAME",IF(E2=6,"SERGEANT",IF(E2=9,"RABBI",IF(E2=10, "PROFESSOR",IF(E2=10002,"PROFESSOR & MRS.",IF(E2=10010 ,"PROFESSORS",IF(E2=11,"ADMIRAL",IF(E2=11002,"ADMIRAL & MRS.",IF(E2=12,"GENERAL",IF(E2=12002,"GENERAL & MRS.",IF(E2=13,"COLONEL",IF(E2=13002,"COLONEL & MRS.",IF(E2=14,"CAPTAIN",IF(E2=14002,"CAPTAIN & MRS.",IF(E2=015,"COMMANDER",IF(E2=15002,"COMMANDER & MRS.",IF(E2=16, "DEAN",IF(E2=17,"JUDGE",IF(E2=17002,"JUDGE & MRS.",IF(E2=18, "MAJOR",IF(E2=18002,"MAJOR & MRS.",IF(E2=19, "SENATOR",IF(E2=20,"GOVERNER",IF(E2=21002,"SERGEANT & MRS.",IF(E2=22002,"COLONEL & MRS.",IF(E2=24,"LIEUTENANT",IF(E2=26,"MONSIGNOR",IF(E2=27,"REVEREND",IF(E2=28,"MS.",IF(E2=28028,"MSS.",IF(E2=29,"BISHOP",IF(E2=31,"AMBASSADOR",IF(E2=31002,"AMBASSADOR & MRS.",IF(E2=33,"CANTOR",IF(E2=36,"BROTHER",IF(E2=37, "SIR",IF(E2=38,"COMMODORE",IF(E2=40,"FATHER",IF(E2=42,"SISTER",IF(E2=43,"PRESIDENT",IF(E2=44,"MASTER",IF(E2=46,"MOTHER",IF(E2=47,"CHAPLAIN",IF(E2=48,"CORPORAL",IF(E2=50,"ELDER",IF(E2=56,"MAYOR",IF(E2=59002,"LIEUTENANT & MRS.",IF(E2=62,"LORD",IF(E2=63, "CARDINAL",IF(E2=64,"FRIEND",IF(E2=65,"FRIENDS",IF(E2=68,"ARCHDEACON",IF(E2=69,"CANON",IF(E2=70,"BISHOP",IF(E2=72002,"REVEREND & MRS.",IF(E2=73,"PASTOR",IF(E2=75,"ARCHBISHOP",IF(E2=85,"SPECIALIST",IF(E2=87,"PRIVATE",IF(E2=89,"SEAMAN",IF(E2=90,"AIRMAN",IF(E2=91,"JUSTICE",IF(E2=92,"MR.JUSTICE",IF(E2=100,"M.",IF(E2=103,"MLLE.",IF(E2=104,"CHANCELLOR",IF(E2=106,"REPRESENTATIVE",IF(E2=107,"SECRETARY",IF(E2=108,"LT. GOVERNOR",IF(E2=109,"LIC.",IF(E2=111,"SA.",IF(E2=114,"DA.",IF(E2=116,"SR.",IF(E2=117,"SRA.",IF(E2=118,"SRTA.",IF(E2=120,"YOUR MAJESTY",IF(E2=122,"HIS HIGHNESS",IF(E2=123,"HER HIGHNESS",IF(E2=124,"COUNT",IF(E2=125,"LADY",IF(E2=126,"PRINCE",IF(E2=127,"PRINCESS",IF(E2=128,"CHIEF",IF(E2=129,"BARON",IF(E2=130,"SHEIK",IF(E2=131, "PRINCE AND PRINCESS",IF(E2=132,"YOUR IMPERIAL MAJESTY",IF(E2=135,"M. ET MME.",IF(E2=210,PROFESSOR))))))))))))))))))))))))))))))))))))))))))))


#################################################################################
# Add DATE keys: Time (MailDate and RecieveDate)--After Fact tables are created #
#################################################################################
#Earliest date: Search ADATE_24,23 94NK,94FS --> 9312--Beginning of 93
#Latest date: Search RDATE_3-12 96 campaigns --> 9806--End of 98 
#11-28

uniqueTime = read.csv("DimTime.csv", header = TRUE)
uniqueTime = uniqueTime[,2] #YearMonthNumber
uniqueTime = as.data.frame(uniqueTime) #1 row

#After Fact tables are created (the tables with no NAs)
mailDateDT = as.data.frame(donationFact2[,3])
donateDateDT = as.data.frame(donationFact2[,4])
#Check NAs (1,281,646): sum(is.na(donateDateDT)) 


mailDateDH = as.data.frame(donorHistFact2[,3])

#Matching to unique TimeKeys
library(prodlim)
classifyDTmd = as.data.frame(row.match(mailDateDT, uniqueTime))
classifyDTdd = as.data.frame(row.match(donateDateDT, uniqueTime))
#Donation
colnames(classifyDTmd) = "MailDateKey"
colnames(classifyDTdd) = "DonateDateKey"
#DonorHistorySnapshot
classifyDHmd = as.data.frame(row.match(mailDateDH, uniqueTime))
colnames(classifyDHmd) = "MailDateKey"

as.data.frame(table())

#Donor History with date Key only-no NAs version
donorHnoNAkey = cbind(donorHistFact2, classifyDHmd)

# to check[needs 0]: sum(is.na(donorHnoNAkey$MailDateKey))
write.csv(donorHnoNAkey, "DonorHistorySnapshotWithKey.csv") #Copy with date too



#DonationSnapshot with date Key
donationNoNAkey = cbind(donationFact2, classifyDTmd, classifyDTdd )


# to check[needs 0]: sum(is.na(donationNoNAkey$MailDateKey))
# #Check NAs (needs 1,281,646):  sum(is.na(donationNoNAkey$DonateDateKey))
write.csv(donationNoNAkey, "DonationFactWithKey.csv")  #Copy with date too


###################################
#Add key: promo (last key to add) #
###################################

#USING above
#donorHnoNAkey  data
#donationNoNAkey data

recoderFunc <- function(data, oldvalue, newvalue) {
  
  # convert any factors to characters
  
  if (is.factor(data))     data     <- as.character(data)
  if (is.factor(oldvalue)) oldvalue <- as.character(oldvalue)
  if (is.factor(newvalue)) newvalue <- as.character(newvalue)
  
  # create the return vector
  
  newvec <- data
  
  # put recoded values into the correct position in the return vector
  
  for (i in unique(oldvalue)) newvec[data == i] <- newvalue[oldvalue == i]
  
  newvec
  
}

promokeyDonationFact = as.data.frame(recoderFunc(donationNoNAkey$promoName,c("96NK","96TK","96SK","96LL","96G1",
                                                          "96GK","96CC","96WL","96X1","96XK",
                                                          "95FS","95NK","95TK","95LL","95G1",
                                                          "95GK","95CC","95WL","95X1","95XK",
                                                          "94FS","94NK")
                              ,c("1","2","3","4","5",
                                 "6","7","8","9","10",
                                 "11","12","13","14","15",
                                 "16","17","18","19","20",
                                 "21","22")))



promokeyDonorHistFact = as.data.frame(recoderFunc(donorHnoNAkey$promoName,c("96NK","96TK","96SK","96LL","96G1",
                                                         "96GK","96CC","96WL","96X1","96XK",
                                                         "95FS","95NK","95TK","95LL","95G1",
                                                         "95GK","95CC","95WL","95X1","95XK",
                                                         "94FS","94NK")
                               ,c("1","2","3","4","5",
                                  "6","7","8","9","10",
                                  "11","12","13","14","15",
                                  "16","17","18","19","20",
                                  "21","22")))

colnames(promokeyDonationFact) = "campaignKey"
colnames(promokeyDonorHistFact) = "campaignKey"

FnlDonationFact = cbind(donationNoNAkey,promokeyDonationFact)
FnlDonorHistFact= cbind(donorHnoNAkey,promokeyDonorHistFact)

#FnlDonorHistFact2= cbind(FnlDonorHistFact2,promokeyDonorHistFact)


#Check: as.data.frame(table(FnlDonationFact$campaignKey)) 
#Should be 22 unique rows

#Check: as.data.frame(table(FnlDonorHistFact$campaignKey)) 
#Should be 22 unique rows

#Convert keys to numeric
FnlDonationFact$campaignKey = as.numeric(FnlDonationFact$campaignKey) 
FnlDonorHistFact$campaignKey = as.numeric(FnlDonorHistFact$campaignKey) 


######################################################################
#Get DTITLE (not available in dataset) can only translate from TCODE #
######################################################################
data$TCODE #view

#Made sure vectors had same length before input in function
length(c("0","1","1001","1002","2",
         "2002","3","3003","4","4002",
         "4004","5","6","9","10",
         "10002","10010","11","11002","12",
         "12002","13","13002","14","14002",
         "15","15002","16","17","17002",
         "18","18002","19","20","21002",
         "22002","24","26","27","28",
         "28028","29","31","31002","33",
         "36","37","38","40","42",
         "43","44","46","47","48",
         "50","56","59002","62","63",
         "64","65","68","69","70",
         "72002","73","75","85","87",
         "89","90","91","92","100",
         "103","104","106","107","108",
         "109","111","114","116","117",
         "118","120","122","123","124",
         "125","126","127","128","129",
         "130","131","132","135","210"
))

length(c("_","MR","MESSRS","MR & MRS","MRS",
         "MESDAMES","MISS","MISSES","DR","DR. & MRS.",
         "DOCTORS" , "MADAME","SERGEANT","RABBI","PROFESSOR",
         "PROFESSOR & MRS","PROFESSORS","ADMIRAL","ADMIRAL & MRS","GENERAL",
         "GENERAL & MRS","COLONEL", "COLONEL & MRS.","CAPTAIN","CAPTAIN & MRS",
         "COMMANDER","COMMANDER & MRS","DEAN","JUDGE","JUDGE & MRS",
         "MAJOR","MAJOR & MRS","SENATOR","GOVERNOR", "SERGEANT & MRS",
         "COLONEL & MRS","LIEUTENANT","MONSIGNOR","REVEREND","MS",
         "MSS","BISHOP","AMBASSADOR","AMBASSADOR & MRS","CANTOR",
         "BROTHER","SIR","COMMODORE","FATHER","SISTER",
         "PRESIDENT","MASTER","MOTHER","CHAPLAIN","CORPORAL",
         "ELDER","MAYOR","LIEUTENANT & MRS","LORD","CARDINAL",
         "FRIEND","FRIENDS","ARCHDEACON","CANON","BISHOP",
         "REVEREND & MRS","PASTOR","ARCHBISHOP","SPECIALIST","PRIVATE",
         "SEAMAN","AIRMAN","JUSTICE","MR JUSTICE","M",
         "MLLE","CHANCELLOR","REPRESENTATIVE","SECRETARY","LT GOVERNOR",
         "LIC" , "SA" ,"DA","SR","SRA",
         "SRTA","YOUR MAJESTY","HIS HIGHNESS","HER HIGHNESS","COUNT",
         "LADY","PRINCE","PRINCESS","CHIEF","BARON",
         "SHEIK","PRINCE AND PRINCESS","YOUR IMPERIAL MAJESTY","M ET MME","PROFESSOR"
))


dtitle = recoderFunc(as.character(data$TCODE),
                                   c("0","1","1001","1002","2",
                                     "2002","3","3003","4","4002",
                                     "4004","5","6","9","10",
                                     "10002","10010","11","11002","12",
                                     "12002","13","13002","14","14002",
                                     "15","15002","16","17","17002",
                                     "18","18002","19","20","21002",
                                     "22002","24","26","27","28",
                                     "28028","29","31","31002","33",
                                     "36","37","38","40","42",
                                     "43","44","46","47","48",
                                     "50","56","59002","62","63",
                                     "64","65","68","69","70",
                                     "72002","73","75","85","87",
                                     "89","90","91","92","100",
                                     "103","104","106","107","108",
                                     "109","111","114","116","117",
                                     "118","120","122","123","124",
                                     "125","126","127","128","129",
                                     "130","131","132","135","210"
                                     )
                                   ,c("_","MR","MESSRS","MR & MRS","MRS",
                                      "MESDAMES","MISS","MISSES","DR","DR. & MRS.",
                                      "DOCTORS" , "MADAME","SERGEANT","RABBI","PROFESSOR",
                                      "PROFESSOR & MRS","PROFESSORS","ADMIRAL","ADMIRAL & MRS","GENERAL",
                                      "GENERAL & MRS","COLONEL", "COLONEL & MRS.","CAPTAIN","CAPTAIN & MRS",
                                      "COMMANDER","COMMANDER & MRS","DEAN","JUDGE","JUDGE & MRS",
                                      "MAJOR","MAJOR & MRS","SENATOR","GOVERNOR", "SERGEANT & MRS",
                                      "COLONEL & MRS","LIEUTENANT","MONSIGNOR","REVEREND","MS",
                                      "MSS","BISHOP","AMBASSADOR","AMBASSADOR & MRS","CANTOR",
                                      "BROTHER","SIR","COMMODORE","FATHER","SISTER",
                                      "PRESIDENT","MASTER","MOTHER","CHAPLAIN","CORPORAL",
                                      "ELDER","MAYOR","LIEUTENANT & MRS","LORD","CARDINAL",
                                      "FRIEND","FRIENDS","ARCHDEACON","CANON","BISHOP",
                                      "REVEREND & MRS","PASTOR","ARCHBISHOP","SPECIALIST","PRIVATE",
                                      "SEAMAN","AIRMAN","JUSTICE","MR JUSTICE","M",
                                      "MLLE","CHANCELLOR","REPRESENTATIVE","SECRETARY","LT GOVERNOR",
                                      "LIC" , "SA" ,"DA","SR","SRA",
                                      "SRTA","YOUR MAJESTY","HIS HIGHNESS","HER HIGHNESS","COUNT",
                                      "LADY","PRINCE","PRINCESS","CHIEF","BARON",
                                      "SHEIK","PRINCE AND PRINCESS","YOUR IMPERIAL MAJESTY","M ET MME","PROFESSOR"
                                      ))
#Note: Separated from function to avoid an error...
dtitle = as.data.frame(dtitle)
colnames(dtitle) = "DTITLE"

#append to current dimDonor
donorDim2 = cbind(donorDim, dtitle)

write.csv(donorDim2, "DonorDim.csv")

###############
#Split RFA
##############
FnlDonorHistFact = read.csv("DonorHistorySnapshotWithKey.csv", header=TRUE)

RECENCY= as.data.frame(substr(FnlDonorHistFact$RFA,1,1))
FREQUENCY= as.data.frame(substr(FnlDonorHistFact$RFA,2,2))
AMOUNT= as.data.frame(substr(FnlDonorHistFact$RFA,3,3))

colnames(RECENCY) = "Recency"
colnames(FREQUENCY) = "Frequency"
colnames(AMOUNT) ="Amount"

FnlDonorHistFact2 = cbind(FnlDonorHistFact,RECENCY,FREQUENCY, AMOUNT)


#####################################################
# Get rid of extra columns in tables for data input #
#####################################################
FnlDonorHistFact3 = FnlDonorHistFact2[,c(-1,-4,-5,-6)]
colnames(FnlDonorHistFact3) =c("DonorKey","CensusKey","TimeKey","DonorStatus","NumberofRecentGifts","AmountLastGiftRange", "CampaignKey")
write.csv(FnlDonorHistFact3,"DonorHistFact1128.csv")

FnlDonationFact = read.csv("DonationFactWithKey.csv", header=TRUE)
#FnlDonationFact2 = cbind(FnlDonationFact,promokeyDonationFact)
FnlDonationFact3 = FnlDonationFact2[,c(-1,-4,-5,-7)]
colnames(FnlDonationFact3) =c("DonorKey","CensusKey","DonationAmnt","CampaignMailedTimeKey","DonationReceivedTimeKey", "CampaignKey")

write.csv(FnlDonationFact3,"DonationFact1128.csv")


###
#For future reference:

write.csv(FnlDonorHistFact2, "extraColFnlDonorHistFact.csv")
write.csv(FnlDonationFact2, "extraColFnlDonationFact.csv")



#############################
# Replacing NAs with blanks #
#############################

donations = read.csv("DonationFact1128.csv", header = TRUE)
donorHist = read.csv("DonorHistFact1128.csv", header = TRUE)

donations$DonationAmnt <- as.character(donations$DonationAmnt)
donations$DonationReceivedTimeKey <- as.character(donations$DonationReceivedTimeKey)

donorHist$DonationAmnt <- as.character(donorHist$DonationAmnt)

df$FirstDate[is.na(df$FirstDate)] = " "
df$FirstDate[is.na(df$FirstDate)] = " "



#################################
# Adding Region column to donor #
#################################

region = read.csv("regionLookup.csv", header = TRUE)
donorDim = read.csv("DimDonor.csv", header= TRUE)

colnames(region) = c("STATE", "REGION")
colnames(donorDim)
colnames(region)


# merge two data frames by state
data = merge(region, donorDim ,by="STATE", all = TRUE)
 
write.csv(data ,"donorDimWithRegion.csv")

########################
#Sum of DonationAmount #
########################
data = read.csv("DonationFact1130.csv")
sum(data$DonationAmnt)

sub = subset(data, !is.na(DonationAmnt))
sum(sub$DonationAmnt)

data2 = read.csv("DonorHistFact1130.csv")

sum(is.na(data2$DonorStatus))
sum(is.na(data2$NumberofRecentGifts))
sum(is.na(data2$AmountLastGiftRange))

subset(data2, DonorKey==61928)

orig = read.csv("DonorDataV3.csv")


x = subset(orig, DonorID==61928)

69128
18856
82071

#Delete null status
#Move bits

####################################
#Deleting null rows out of the RFA #
####################################



donorHist = read.csv("DonorHistFact1130.csv", header = TRUE)

donorHist = donorHist[,-1]

noNA = subset(donorHist, NumberofRecentGifts !="" & AmountLastGiftRange != "" & DonorStatus !="" )

write.csv(noNA, "DonorHistFact_1206.csv")


donations$DonationAmnt <- as.character(donations$DonationAmnt)
donations$DonationReceivedTimeKey <- as.character(donations$DonationReceivedTimeKey)

donorHist$DonationAmnt <- as.character(donorHist$DonationAmnt)

df$FirstDate[is.na(df$FirstDate)] = " "
df$FirstDate[is.na(df$FirstDate)] = " "
