
# Starting Admin ####

## Load Env

source(paste0(getwd(), "/env.R"))

## Load and Tidy Datasets

hpi <- read_csv(paste0(getwd(),'/hpi_Full.csv'), col_names = T) %>%
  dplyr:: mutate(Date = as_date(dmy(Date)))

## https://www.gov.uk/government/publications/about-the-uk-house-price-index/about-the-uk-house-price-index#republishing-data?utm_medium=GOV.UK&utm_source=datadownload&utm_campaign=republishing&utm_term=9.30_25_03_20

census_lad <- read.xlsx(paste0(getwd(),'/censusdata.xlsx'), sheet = 3, startRow = 8) %>%
  dplyr:: rename(AreaCode = Code,
                 RegionCounty = `Region/Country`,
                 Supergroup = Supergroup.name,
                 Group = Group.name,
                 Subgroup = Subgroup.name) 

br <- read_csv(paste0(getwd(),'/bankrate.csv'), col_names = T) %>%
  dplyr:: rename(Date = `Date Changed`) %>%
  dplyr:: mutate(Date = as_date(dmy(Date))) %>%
  tidyr:: complete(Date = seq.Date(min(Date), max(Date), by="day")) %>%
  tidyr:: fill(Rate)

hpi_lad <- hpi %>%
  dplyr:: left_join(census_lad[ , (names(census_lad)) %in% 
                                         c("AreaCode", "RegionCounty", "Supergroup", "Group", "Subgroup")], 
                    by = "AreaCode") %>%
  dplyr:: mutate(Country = case_when(RegionCounty == "Scotland" ~ "Scotland",
                                     RegionCounty == "Wales" ~ "Wales",
                                     RegionCounty == "Northern Ireland" ~ "NorthernIreland",
                                     TRUE ~ "England"),
                 RegionCounty = case_when(RegionCounty == Country ~ RegionName,
                                    RegionCounty != Country ~ RegionCounty)) %>%
  dplyr:: left_join(br, by = "Date") %>%
  dplyr:: filter(!is.na(RegionCounty)) %>%
  dplyr:: mutate_if(is.character, trimws)# this data set has everything but LAD level removed

rm(hpi,br)
gc()

# Couple of Graphs ####



## Housing Types

PriceVolGraphR <- function(df = hpi_lad, 
                           name = "HousingType",
                           vars = c("AveragePrice",
                                    "DetachedPrice",
                                    "FlatPrice",
                                    "SemiDetachedPrice",
                                    "TerracedPrice"),
                           datefil = "1995-01-01",
                           ttl = c("Average House Price Changes Over Time for each Housing Type",
                                   "Average House Sales Volume Changes Over Time")){
  
  
  
  dfgg <- data.frame(df) %>%
    tidyr:: gather(key = !!eval(expr(paste(!!!name))), 
                   value = 'Price', 
                   vars) %>%
    dplyr:: filter(!is.na(Price)|
                     Date >= datefil) %>%
    dplyr:: group_by_at(c(name, "Date")) %>%
    dplyr:: summarise(MeanPrice = mean(Price, na.rm = T),
                      SDPrice = sd(Price, na.rm = T),
                      MeanVol = mean(SalesVolume, na.rm = T),
                      SDVol = sd(SalesVolume, na.rm = T))
  
  grid.arrange(
    
    ggplot(dfgg, aes_string(x = "Date", y = "MeanPrice", colour = name)) +
      geom_line() + 
      ggtitle(paste0(ttl[1])),
    
    ggplot(dfgg, aes_string(x = "Date", y = "MeanVol")) +
      geom_line() + 
      ggtitle(paste0(ttl[2])),
    
    ncol=2)
}

PriceVolGraphR()

## Buyer Type (FTB/FOO)

PriceVolGraphR(name = "BuyerType",
               vars = c("FOOPrice", 
                        "FTBPrice"),
               datefil = "2012-01-01",
               ttl = c("Average House Prices for different buyer types over time",
                 "Average House Sales Volume Changes Over Time"))


# Regressions ####

## Create Test and Train Datasets

set.seed(1)

split <- sample(1:nrow(hpi_lad),0.75*nrow(hpi_lad))
train <- hpi_lad[split,]
test <- hpi_lad[-split,]

## House Prices by Overall Volume and Rate

mod1 <- lm(data = train, formula = AveragePrice ~ SalesVolume + Rate)
summary(mod1)#$r.squared

## Sales Volume by with Buyer Type and Rate

mod2 <- lm(data = train, formula = SalesVolume ~ FTBPrice + Rate)
summary(mod2)#$r.squared

## House Prices with Sales Volume (NI doesn't include Old/New Split) and Rate interacting with countries

mod3 <- lm(data = train, formula = AveragePrice ~ SalesVolume + Rate*Country)
summary(mod3)#$r.squared

## House Price by Supergroup interacting with Rate

mod4 <- lm(data = train, formula = AveragePrice ~ Supergroup*log(Rate))
summary(mod4)#$r.squared

ggplot2::ggplot(ggpredict(model = mod4, terms = c("Rate[exp]","Supergroup")), 
                aes(x = x, y = predicted, colour = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin=predicted-1.96*std.error, ymax=predicted+1.96*std.error), alpha = 0.1) +
  xlab("BoE Base Rate") +
  ylab("Predicted Average Price") +
  ggtitle("Average House Prices given Log of BoE Base Rate in Supergroups") + 
  labs(colour = "Supergroup") 

rm(mod1,mod2,mod3,mod4)
gc()
 
# Classification Model ####

## London Cosmopolitan vs Countryside Living

train <- train[train$Supergroup %in% c("London Cosmopolitan","Countryside Living"),] %>%
  dplyr:: mutate(Supergroup = recode(Supergroup, 'London Cosmopolitan' = 1,
                                     'Countryside Living' = 0))

test <- test[test$Supergroup %in% c("London Cosmopolitan","Countryside Living"),] %>%
  dplyr:: mutate(Supergroup = recode(Supergroup, 'London Cosmopolitan' = 1,
                                     'Countryside Living' = 0))

## GLM for Average Price, Sales Volume and Country as Identifiers

glmod1 <- glm(data=train, formula = Supergroup ~ AveragePrice+SalesVolume+Country,family = binomial)

summary(glmod1)

## GLM for Type of Sales Volume and Funding status

glmod2 <- glm(data=train_lcvcl, formula = Supergroup ~ CashSalesVolume + NewSalesVolume, family=binomial)

summary(glmod2)

## K-Nearest Neighbours


