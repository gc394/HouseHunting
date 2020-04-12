 # Load Libraries

library(tidyverse)
library(lubridate)
library(mgsub)
library(stringi)
library(class)
library(ISLR)

# Load and Tidy Dataset

pp19 <- read_csv(paste0(getwd(),'/pp-2019.csv'), col_names = F) %>%
  base:: as.data.frame() %>%
  dplyr:: rename(
    TUI = X1,
    Price = X2,
    Date = X3,
    Postcode = X4,
    PropertyType = X5, # D = Detached, S = Semi-Detached, T = Terraced, F = Flat, O = Other
    OldNew = X6,
    Duration = X7, # F = Freehold, L = Leasehold
    PAON = X8,
    SAON = X9,
    Street = X10,
    Locality = X11,
    TownCity = X12,
    District = X13,
    County = X14,
    CategoryType = X15, # A = Standard Price Paid entry, B = Additional Price Paid
    RecordStatus = X16 # A = Addition, C = Change, D = Delete
                ) %>%
  dplyr:: mutate(OldNew = recode(OldNew, 
    N = "O",
    Y = "N"        
                ),
    County = stri_replace_all_fixed(County, 
                                    pattern = c("CITY OF ", 
                                                "BOURNEMOUTH, CHRISTCHURCH AND POOLE",
                                                "BOURNEMOUTH",
                                                "POOLE"), 
                                    replacement = c("",
                                                    rep("DORSET",3)), 
                                    vectorize_all = FALSE)
                )

geomap <- read_csv(paste0(getwd(),'/geomapping.csv'), col_names = T) %>%
  dplyr:: mutate(CTY17NM = ifelse(is.na(CTY17NM), 
                                  str_to_upper(LAD17NM),
                                  str_to_upper(CTY17NM)),
                 CTY17NM = stri_replace_all_fixed(CTY17NM, 
                                                 pattern = c(", CITY OF", 
                                                             "OUTER LONDON",
                                                             "RHONDDA CYNON TAF",
                                                             "VALE",
                                                             ", COUNTY OF",
                                                             "TELFORD AND ",
                                                             "POOLE",
                                                             "BOURNEMOUTH"), 
                                                 replacement = c("",
                                                                 "GREATER LONDON",
                                                                 "RHONDDA CYNON TAFF",
                                                                 "THE VALE",
                                                                 rep("",2),
                                                                 rep("DORSET",2)), 
                                                 vectorize_all = FALSE)
                ) %>%
  dplyr:: select("CTY17NM", "GOR10NM", "CTRY17NM")  %>%
  dplyr:: rename(
    County = CTY17NM,
    Region = GOR10NM,
    Country = CTRY17NM
                ) %>%
  stats:: na.omit()

# pp19geo <- left_join(pp19, geomap, by = "County") # Error: vector memory exhausted (limit reached?)

regions <- unique(geomap$Region)

gc()

tst <- pp19[pp19$County %in% geomap[geomap$Region %in% c("South East"),]$County,] #London

# KNN

## Example

set.seed(1)

attach(Smarket)

 train.fil.tst <- (Year <2005)
 train.tst <- cbind(Smarket$Lag1 ,Smarket$Lag2)[train.fil.tst,]
 test.tst <- cbind(Smarket$Lag1,Smarket$Lag2)[!train.fil.tst,]
 train.dir.tst <- Direction[train.fil.tst]
 Direction.2005 <- Direction[!train.fil.tst]
 
 knn.eval <- data.frame(acc = rep(NA,100), knum = rep(NA,100))
 
 for (i in 1:10){
 
 knn.tst <- knn(train.tst,test.tst,train.dir.tst,k = i)

 knn.tbl.tst <- table(knn.tst, Direction.2005)

 knn.eval$knum[i] <- i
 
 knn.eval$acc[i] <- (knn.tbl.tst[1,1]+knn.tbl.tst[2,2])/sum(knn.tbl.tst)
 
 }

 ggplot(knn.eval, aes(x = knum, y= acc)) +
   geom_point()
   
## Property Type

 # Visualisation of Dataset

ggplot(tst[tst$PropertyType != "O"&pp19$Price<300000,], aes(x = PropertyType, y = Price)) +
  geom_boxplot(outlier.colour="black", outlier.shape = 16,
               outlier.size = 2, notch = FALSE)

tst <- pp19 %>%
  dplyr:: group_by(Month = month(Date), County) %>%
  dplyr:: summarise(MeanPrice = mean(Price))

ggplot(tst %>%
         dplyr:: filter(Price < 500000) %>%
         dplyr:: group_by(Month = month(Date, label = T), County) %>%
         dplyr:: summarise(MeanPrice = mean(Price),
                           NSale = n())) +
  geom_point(aes(x = Month, y = MeanPrice, colour = County, size = NSale))


