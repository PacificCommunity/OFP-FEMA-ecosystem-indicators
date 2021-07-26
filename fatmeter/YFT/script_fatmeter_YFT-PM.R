#------------------------------------------------------#
# Date : 07/2021                                       #
# Update : 07/2021                                     #
# Created by : Pauline Machful                         #
# FATMETER DATA - YELLOWFIN                            #
#------------------------------------------------------#

# Set working directory
setwd("P:/OFPEMA/Pauline Machful/R/fatmeter/YFT/plots") #PM

# Connect to Biodasys database 
library(RODBC)
Biodasys <- odbcConnectAccess2007("C:/Ecology/BioDaSys.accdb") #PM

# Data extraction
fatmeter_data <- sqlQuery(Biodasys,"SELECT BIO_trip.trip_id, 
                                         BIO_trip.trip_name, 
                                         BIO_trip.trip_no, 
                                         REF_trip_type.trip_type_desc, 
                                         BIO_fish.fish_id, 
                                         REF_species.asfis_code, 
                                         BIO_analysis_fatmeter_values.chosen_values_avg, 
                                         BIO_analysis_fatmeter_values.validity, 
                                         BIO_analysis_fatmeter_values.fatmeter_measure_id, 
                                         REF_fatmeter_measure.fatmeter_measure_desc, 
                                         BIO_analysis_fatmeter.produce, 
                                         BIO_analysis_fatmeter_values.comment, 
                                         BIO_fish.length_mm, BIO_fish.length_code_id, 
                                         BIO_fish.calculated_wt_gr, 
                                         BIO_set_base.set_date_local, 
                                         BIO_set_base.set_time_local, 
                                         Year(BIO_set_base.set_date_local) AS [Year], 
                                         Month(BIO_set_base.set_date_local) AS [Month], 
                                         BIO_set_base.latitude_dec, 
                                         BIO_set_base.longitude_dec, 
                                         BIO_set_base.longhurst_code, 
                                         REF_gear.gear_desc, 
                                         REF_school_association.school_association_desc
                                    FROM ((((((BIO_trip INNER JOIN (((BIO_fish INNER JOIN REF_species ON BIO_fish.species_id = REF_species.species_id) 
                                    INNER JOIN BIO_sample_base ON BIO_fish.fish_id = BIO_sample_base.fish_id) 
                                    INNER JOIN BIO_set_base ON BIO_fish.set_base_id = BIO_set_base.set_base_id) ON BIO_trip.trip_id = BIO_set_base.trip_id) 
                                    INNER JOIN BIO_analysis_base ON BIO_sample_base.sample_base_id = BIO_analysis_base.sample_base_id) 
                                    INNER JOIN (BIO_analysis_fatmeter INNER JOIN BIO_analysis_fatmeter_values ON BIO_analysis_fatmeter.analysis_base_fatmeter_id = BIO_analysis_fatmeter_values.analysis_base_fatmeter_id) ON BIO_analysis_base.analysis_base_id = BIO_analysis_fatmeter.analysis_base_fatmeter_id) 
                                    INNER JOIN REF_fatmeter_measure ON BIO_analysis_fatmeter_values.fatmeter_measure_id = REF_fatmeter_measure.fatmeter_measure_id) 
                                    INNER JOIN REF_gear ON BIO_trip.gear_id = REF_gear.gear_id) 
                                    INNER JOIN REF_school_association ON BIO_set_base.school_association_id = REF_school_association.school_association_id)
                                    INNER JOIN REF_trip_type ON BIO_trip.trip_type_id = REF_trip_type.trip_type_id
                                    WHERE (((BIO_analysis_fatmeter_values.validity)='Yes'))", max=0, stringsAsFactors=FALSE)

#5128 obs. of 24 var.
# VALIDED data - all species

library(dplyr)
library(ggplot2)


#------------------#
#                  #
#    YELLOWFIN     #
#                  #
#------------------#

# ___ EXPLORE DATA ___

yellowfin_all <- fatmeter_data %>%
  filter(asfis_code == "YFT") # all YFT ; 1625 obs.

yellowfin <- fatmeter_data %>%
  filter(asfis_code == "YFT", fatmeter_measure_id == "V") # 1531 obs.
# all YFT with Classical Vertical measure
#write.csv(yellowfin, file="P:/OFPEMA/Pauline Machful/R/fatmeter/YFT/yellowfin.csv", row.names=FALSE) 

summary(yellowfin$length_mm) 
summary(yellowfin$calculated_wt_gr)

table(yellowfin$produce)
# 1517 YELLOWFIN-1
# 14 NON YELLOWFIN-1

    # COMMENT FOR NON YELLOWFIN-1 DATA
    #Fatmeter error with wrong Produce and/or wrong date and/or wrong value 
    #downloaded despite the fact that Produce, date and value were correct when 
    #conducting the measurement; keep value recorded by hand
    
    #Fatmeter error with wrong Produce and/or wrong date and/or wrong value 
    #downloaded despite the fact that Produce, date and value were correct when 
    #conducting the measurement; in this session, but this value is OK according 
    #to the value recorded by hand

# --> NON YELLOWFIN-1 VALUE OK = NO NEED CALIBRATION <-- #



# ___ CHECK AND STANDARDIZE LENGTH DATA ____

table(yellowfin_all$length_code_id) # 1622 records with "UF" (fork length) code.
table(yellowfin$length_code_id) # 1528 records with "UF" (fork length) code.

yellowfin_all$length_cm <- yellowfin_all$length_mm/10 # convert lengths from mm to cm 
yellowfin$length_cm <- yellowfin$length_mm/10 # convert lengths from mm to cm 

# if there are other codes, run :
# WW = 3.195e-005 x UF^2.9113 (YFT length-weight conversion factor)
# yellowfin$length_cm<-ifelse(!yellowfin$length_code_id=="UF", 
#                         ((yellowfin$calculated_wt_gr/1000)/(3.195e-05))^(1/2.9113), 
#                         yellowfin$length_cm)



# ___ ARRANGE DATA ___

# ALL YFT
yellowfin_all$produce2[!yellowfin_all$produce == "YELLOWFIN-1" & ! yellowfin_all$produce == "Yellowfin-1"] <- "non YELLOWFIN-1"
yellowfin_all$produce2[yellowfin_all$produce == "YELLOWFIN-1" | yellowfin_all$produce == "Yellowfin-1"] <- "YELLOWFIN-1"

yellowfin_all$measure <- yellowfin_all$fatmeter_measure_desc
yellowfin_all$measure[!yellowfin_all$fatmeter_measure_id == "V"] <- "Non Classical vertical"
yellowfin_all$measure <- factor(yellowfin_all$measure,c("Non Classical vertical","Classical vertical"))

# YFT with CV measure
yellowfin$produce2[!yellowfin$produce == "YELLOWFIN-1" & ! yellowfin$produce == "Yellowfin-1"] <- "non YELLOWFIN-1"
yellowfin$produce2[yellowfin$produce == "YELLOWFIN-1" | yellowfin$produce == "Yellowfin-1"] <- "YELLOWFIN-1"


yellowfin$serie[yellowfin$Year<2019] <- "before 2019"
yellowfin$serie[yellowfin$Year>=2019] <- "since 2019"

YFT_before19 <- yellowfin %>% filter (Year <2019)
YFT_since19 <- yellowfin %>% filter (Year >=2019)



## ___ PLOTS ___

### ALL DATA
plot(chosen_values_avg ~ length_mm, data = yellowfin)
title("YFT - all data")

hist(yellowfin$chosen_values_avg)

ggplot(yellowfin_all, aes(length_cm, chosen_values_avg, color=measure))+
  geom_point()+
  facet_grid(~asfis_code)+
  theme(strip.background = element_rect(fill="lightgoldenrod1"), 
        panel.background = element_rect(fill = "gray95"))+
  labs(y= "Fat content (%)", x="Length (cm)")+
  ggtitle("Fatcontent - YFT (n=1625) - ALL DATA")


### ALL DATA BY PRODUCE : CLASSICAL V / NON CLASSICAL V

P <- yellowfin_all %>% filter(fatmeter_measure_id== "P") %>%
          select(fish_id, Chyu_toro = chosen_values_avg) 

V <- yellowfin_all %>% filter(fatmeter_measure_id== "V") %>%
  select(fish_id, Classical_Vertical = chosen_values_avg) 

measure <- inner_join(P,V)

plot(Classical_Vertical ~ Chyu_toro, data=measure)
title("YFT (n=63)")


### ALL CLASSICAL VERTICAL DATA
ggplot(yellowfin, aes(length_cm, chosen_values_avg, color=produce2))+
  geom_point()+
  facet_grid(~asfis_code)+
  theme(strip.background = element_rect(fill="lightgoldenrod1"), 
        panel.background = element_rect(fill = "gray95"))+
  labs(y= "Fat content (%)", x="Length (cm)")+
  ggtitle("Fatcontent - YFT (n=1528) - ALL DATA (Classical Vertical)")


### CLASSICAL VERTICAL DATA BEFORE / SINCE 2019
ggplot(yellowfin, aes(length_cm, chosen_values_avg, color=serie))+
  geom_point(alpha=0.6)+
  facet_grid(~asfis_code)+
  theme(strip.background = element_rect(fill="lightgoldenrod1"), 
        panel.background = element_rect(fill = "gray95"))+
  scale_color_manual(values=c("ivory4","gray0"),drop=FALSE)+  
  labs(y= "Fat content (%)", x="Length (cm)")+
  ggtitle("Fatcontent - YFT (n=1528) - ALL DATA (Classical Vertical)")

# CLASSICAL VERTICAL DATA BEFORE 2019
ggplot(YFT_before19, aes(length_cm, chosen_values_avg))+
  geom_point(color="gray25", size = 1.5, alpha =0.5)+
  facet_grid(~asfis_code)+
  theme(strip.background = element_rect(fill="lightgoldenrod1"), 
        panel.background = element_rect(fill = "gray95"))+
  labs(y= "Fat content (%)", x="Length (cm)")+
  ggtitle("Fatcontent - YFT (n=1428) - Before 2019 (Classical Vertical)")

# CLASSICAL VERTICAL DATA SINCE 2019
ggplot(YFT_since19, aes(length_cm, chosen_values_avg))+
  geom_point(color="gray25", size = 1.5, alpha =0.5)+
  facet_grid(~asfis_code)+
  theme(strip.background = element_rect(fill="lightgoldenrod1"), 
        panel.background = element_rect(fill = "gray95"))+
  labs(y= "Fat content (%)", x="Length (cm)")+
  ggtitle("Fatcontent - YFT (n=100) - Since 2019 (Classical Vertical)")

# -->  KEEP ALL CLASSICAL VERTICAL DATA <-- #



### DATA BY YEAR

by_year_YFT = yellowfin %>% group_by(Year, asfis_code) %>% 
  summarize(n=length(fish_id), mean_fat=mean(chosen_values_avg), 
                              lower_quant=quantile(chosen_values_avg)[2],
                              median_fat=median(chosen_values_avg), 
                              upper_quant=quantile(chosen_values_avg)[4],
                              sd=sd(chosen_values_avg))
by_year_YFT$Year_n <- paste(by_year_YFT$Year, by_year_YFT$n, sep="\nn=")


# MEAN / YEAR
ggplot(by_year_YFT, aes(as.factor(Year_n), mean_fat))+
  geom_col(size = 3, alpha =0.6)+ 
  geom_errorbar(aes(ymin= mean_fat - sd, 
                    ymax= mean_fat + sd), width=.2,
                position= position_dodge(.8))+
  facet_wrap(asfis_code~.)+
  theme(strip.background = element_rect(fill="lightgoldenrod1"), 
        panel.background = element_rect(fill = "gray95"))+
  labs(y= "Mean fat content (%)", x="Year")+
  ggtitle("Fat content - YFT (n=1531)")

# MEDIAN / YEAR
ggplot(by_year_YFT, aes(as.factor(Year_n), median_fat))+
  geom_col(size = 3, alpha =0.6)+ 
  geom_errorbar(aes(ymin= median_fat - sd, 
                    ymax= median_fat + sd), width=.2,
                position=position_dodge(.8))+
  facet_wrap(asfis_code~.)+
  theme(strip.background = element_rect(fill="lightgoldenrod1"), 
        panel.background = element_rect(fill = "gray95"))+
  labs(y= "Median fat content (%)", x="Year")+
  ggtitle("Fat content - YFT (n=1531)")


### HISTOGRAM LENGTH / YEAR
Year_n <- by_year_YFT %>% select(Year, Year_n)
yellowfin<- merge(yellowfin, Year_n)

ggplot(yellowfin, aes(length_cm))+
  geom_histogram(size = 3, alpha =0.6)+
  facet_wrap(Year_n~.)+
  theme(strip.background = element_rect(fill="lightgoldenrod1"), 
        panel.background = element_rect(fill = "gray95"))+
  labs(x="Length (cm)")+
  ggtitle("Fat content - YFT (n=1528)")


### HISTOGRAM FATcontent / YEAR

library(ggplot2)

ggplot(yellowfin, aes(chosen_values_avg))+
  geom_histogram(size = 3, alpha =0.6)+
  facet_wrap(Year_n~.)+
  theme(strip.background = element_rect(fill="lightgoldenrod1"), 
        panel.background = element_rect(fill = "gray95"))+
  labs(x="Length (cm)")+
  ggtitle("Fat content - YFT (n=1528)")


# -->  KEEP FISH FROM 40-60 CM <-- #


### 40-60 CM ###

YFT_4060 <- yellowfin %>% filter(length_cm >= 40 & length_cm <= 60) #879

by_year_YFT_4060 = YFT_4060 %>% group_by(Year, asfis_code) %>% 
  summarize(n=length(fish_id), mean_fat=mean(chosen_values_avg), 
            lower_quant=quantile(chosen_values_avg)[2],
            median_fat=median(chosen_values_avg), 
            upper_quant=quantile(chosen_values_avg)[4],
            sd=sd(chosen_values_avg))
by_year_YFT_4060$Year_n2 <- paste(by_year_YFT_4060$Year, by_year_YFT_4060$n, sep="\nn=")
Year_4060 <- by_year_YFT_4060 %>% select(Year, Year_n2)
YFT_4060 <- inner_join(YFT_4060, Year_4060)


## 4060 - DATA BY SIZE
ggplot(YFT_4060, aes(length_cm, chosen_values_avg))+
  geom_point(color="gray25", size = 1.5, alpha =0.5)+
  facet_grid(~asfis_code)+
  theme(strip.background = element_rect(fill="lightgoldenrod1"), 
        panel.background = element_rect(fill = "gray95"))+
  labs(y= "Fat content (%)", x="Length (cm)")+
  ggtitle("Fatcontent - YFT (n=879) 40-60cm")


## 4060 - DATA BY YEAR
ggplot(YFT_4060 , aes(length_cm))+
  geom_histogram(size = 3, alpha =0.6)+
  facet_wrap(Year_n2~.)+
  theme(strip.background = element_rect(fill="lightgoldenrod1"), 
        panel.background = element_rect(fill = "gray95"))+
  labs(x="Length (cm)")+
  ggtitle("Fat content - YFT (n=879)  40-60cm")

  # 4060 MEAN / YEAR
ggplot(by_year_YFT_4060, aes(as.factor(Year_n2), mean_fat))+
  geom_col(size = 3, alpha =0.6)+ 
  geom_errorbar(aes(ymin= mean_fat - sd, 
                    ymax= mean_fat + sd), width=.2,
                position= position_dodge(.8))+
  facet_wrap(asfis_code~.)+
  theme(strip.background = element_rect(fill="lightgoldenrod1"), 
        panel.background = element_rect(fill = "gray95"))+
  labs(y= "Mean fat content (%)", x="Year")+
  ggtitle("Fat content - YFT (n=879)  40-60cm")

  # 4060 MEDIAN / YEAR
ggplot(by_year_YFT_4060, aes(as.factor(Year_n2), median_fat))+
  geom_col(size = 3, alpha =0.6)+ 
  geom_errorbar(aes(ymin= median_fat - sd, 
                    ymax= median_fat + sd), width=.2,
                position=position_dodge(.8))+
  facet_wrap(asfis_code~.)+
  theme(strip.background = element_rect(fill="lightgoldenrod1"), 
        panel.background = element_rect(fill = "gray95"))+
  labs(y= "Median fat content (%)", x="Year")+
  ggtitle("Fat content - YFT (n=879)  40-60cm")



## __ Time serie

fat_time_serie_YFT_4060 <- by_year_YFT_4060 %>% select(!Year_n2) # 40-60cm ; 8 obs. of 8 var.
#write.csv(fat_time_serie_YFT_4060, file="P:/OFPEMA/Pauline Machful/R/fatmeter/YFT/fat_time_serie_YFT_4060.csv", row.names = FALSE)


