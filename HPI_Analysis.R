
# Starting Admin ####

## Load Env

source(paste0(getwd(), "/env.R"))

options(scipen = 999)

## Load and Tidy HPI Dataset
#### https://www.gov.uk/government/publications/about-the-uk-house-price-index/about-the-uk-house-price-index#republishing-data?utm_medium=GOV.UK&utm_source=datadownload&utm_campaign=republishing&utm_term=9.30_25_03_20

hpi <- read_csv(paste0(getwd(),'/HPI_Full.csv'), col_names = T) %>%
  dplyr:: mutate(Date = as_date(dmy(Date))) %>%
  dplyr:: select(-IndexSA)

## Load and Tidy Workplace and Residence Earnings Dataset

# earnings <- dplyr:: full_join((read.xlsx(paste0(getwd(), "/workplaceearnings.xlsx"), sheet = 18, startRow = 7, na.strings = ":") %>%
#                                 tidyr:: gather(key = "Year",
#                                                value = "WorkplaceEarnings",
#                                                -Region.code, -Region.name, -Code, -Name) %>%
#                                 dplyr:: select(Code, Year, WorkplaceEarnings)),
#                               (read.xlsx(paste0(getwd(), "/residenceearnings.xlsx"), sheet = 18, startRow = 7, na.strings = ":") %>%
#                                 tidyr:: gather(key = "Year",
#                                                value = "ResidenceEarnings",
#                                                -Region.code, -Region.name, -Local.authority.code, -Local.authority.name) %>%
#                                 dplyr:: select(Local.authority.code, Year, ResidenceEarnings)),
#                               by = c("Year", 
#                                      "Code" = "Local.authority.code")) %>%
#   dplyr:: mutate(Date = as_date(ISOdate(Year, 1, 1))) %>%
#   tidyr:: complete(Date = seq.Date(min(Date), max(Date), by="month")) %>%
#   tidyr:: fill(WorkplaceEarnings, ResidenceEarnings) %>%
#   dplyr:: select(-Year)

## Load and Tidy RPI Dataset

rpi <- read.xlsx(paste0(getwd(),'/inflation.xlsx'), sheet = 49, startRow = 7,cols = c(2,4:15)) %>%
  dplyr:: mutate_if(is.character, trimws) %>%
  dplyr:: filter_at(vars(names(.)[2:13]),  all_vars(. != "..")) %>%
  tidyr:: gather(key = Month,
                 value = RPI,
                 -X1) %>%
  dplyr:: mutate(Date = as_date(ymd(paste(X1,Month,1,sep = "-"))),
                 RPI = as.double(RPI),
                 MonthPerChange = (RPI - lag(RPI))/lag(RPI)) %>%
  dplyr:: select(Date, RPI, MonthPerChange)

## Load and Tidy 2011 Census Dataset

census2011_lad <- read.xlsx(paste0(getwd(),'/censusdata.xlsx'), sheet = 3, startRow = 8) %>%
  dplyr:: rename(AreaCode = Code,
                 RegionCounty = `Region/Country`,
                 Supergroup = Supergroup.name,
                 Group = Group.name,
                 Subgroup = Subgroup.name) %>%
  dplyr:: select(-c(Name, contains(".code"), `Country/region`)) %>%
  dplyr:: mutate_if(is.character, trimws)

## Load and Tidy Bankrate Dataset

br <- read_csv(paste0(getwd(),'/bankrate.csv'), col_names = T) %>%
  dplyr:: rename(Date = `Date Changed`) %>%
  dplyr:: mutate(Date = as_date(dmy(Date))) %>%
  tidyr:: complete(Date = seq.Date(min(Date), max(Date), by="day")) %>%
  tidyr:: fill(Rate)

# Combine Datasets

hpi_lad <- hpi %>%
  dplyr:: left_join(census2011_lad[ , (names(census2011_lad)) %in% 
                                         c("Name","AreaCode", "RegionCounty", "Supergroup", "Group", "Subgroup")], 
                    by = "AreaCode") %>%
  dplyr:: mutate(Country = case_when(RegionCounty == "Scotland" ~ "Scotland",
                                     RegionCounty == "Wales" ~ "Wales",
                                     RegionCounty == "Northern Ireland" ~ "NorthernIreland",
                                     TRUE ~ "England"),
                 RegionCounty = case_when(RegionCounty == Country ~ RegionName,
                                    RegionCounty != Country ~ RegionCounty)) %>%
  dplyr:: left_join(br, 
                    by = "Date") %>%
  dplyr:: select(Date, AreaCode, RegionName, RegionCounty, Country, 
                 Supergroup, Group, Subgroup, everything()) %>%
  dplyr:: filter(!is.na(RegionCounty)) %>% # this data set has everything but LAD level removed
  dplyr:: rename(Area = RegionName,
                 Region = RegionCounty)
  
rm(hpi,br,earnings)
gc()

# Couple of Graphs ####

## Graphing Functions

PriceVolGraphR <- function(df, 
                           name,
                           vars,
                           datefil,
                           ttl){
  
  dfgg <- data.frame(df) %>%
    tidyr:: gather(key = !!eval(expr(paste(!!!name))), 
                   value = 'Price', 
                   all_of(vars)) %>%
    dplyr:: filter(!is.na(Price)|
                     Date >= datefil) %>%
    dplyr:: group_by_at(c(name, "Date")) %>%
    dplyr:: summarise(MeanPrice = mean(Price, na.rm = T),
                      SDPrice = sd(Price, na.rm = T),
                      MeanVol = mean(SalesVolume, na.rm = T),
                      SDVol = sd(SalesVolume, na.rm = T))
  
  grid.arrange(
    
    ggplot(dfgg, 
           aes_string(x = "Date", y = "MeanPrice", colour = name), na.rm = T) +
      geom_line()+
      ggtitle(paste0(ttl[1])),
    
    ggplot(dfgg, 
           aes(x = Date, y = MeanVol)) +
      geom_line() +
      ggtitle(paste0(ttl[2])),
    
    ncol=2)
} 
#### Shows differences in price based on chosen variables and volume over time

AreaRegionCountryCompareR <- function(df, 
                                      area,
                                      datefil){
  
  reg <- unique(df[df$Area == area,]$Region)
  country <- unique(df[df$Area == area,]$Country)
  places <- c(area, 
              reg, 
              country)
  
  type <- c("Area",
            "Region",
            "Country")
  
  dfcreate <- function(x){
    
    dfgg <- x %>%
      dplyr:: filter_at(vars(contains(type[i])),  any_vars(. == places[i])) %>%
      dplyr:: group_by_at(c(type[i], "Date")) %>%
      dplyr:: summarise(MeanPrice = mean(AveragePrice, na.rm = T)) %>%
      dplyr:: ungroup()
    
    names(dfgg)[1] <- "Location"
    
    return(dfgg)
    
  }
  
  prices <- list()
  
  for (i in 1:length(places)){
    
   prices[[i]] <- assign(paste0(places[i], "Price"), dfcreate(hpi_lad))
    
  }

  dfgg1 <- data.frame(df[df$Area == area,]) %>%
    tidyr:: gather(key = !!eval(expr(paste(!!!area))), 
                   value = 'Price', 
                   all_of(c("AveragePrice",
                            "DetachedPrice",
                            "FlatPrice",
                            "SemiDetachedPrice",
                            "TerracedPrice"))) %>%
    dplyr:: filter(!is.na(Price)|
                   Date >= datefil) %>%
    dplyr:: group_by_at(c(area, "Date")) %>%
    dplyr:: summarise(MeanPrice = mean(Price, na.rm = T),
                      SDPrice = sd(Price, na.rm = T),
                      MeanVol = mean(SalesVolume, na.rm = T),
                      SDVol = sd(SalesVolume, na.rm = T))  

  grid.arrange(
    
    ggplot(bind_rows(prices),
           aes(Date, MeanPrice, colour = Location)) +
      geom_line() +
      ggtitle(paste0(area," Price Comparison")),
    
    ggplot(dfgg1, 
           aes_string(x = "Date", y = "MeanPrice", colour = paste0("`",area,"`")), na.rm = T) +
      geom_line() +
      ggtitle(paste0(area," Housing Price Split")),
    
    nrow = 2)
     
} 
#### Shows how area compares to its region and country and the split ver property types

## Housing Types

PriceVolGraphR(df = hpi_lad, 
               name = "HousingType",
               vars = c("AveragePrice",
                        "DetachedPrice",
                        "FlatPrice",
                        "SemiDetachedPrice",
                        "TerracedPrice"),
               datefil = "1995-01-01",
               ttl = c("Average House Price Changes Over Time for each Housing Type",
                       "Average House Sales Volume Changes Over Time"))

## Buyer Type (FTB/FOO)

PriceVolGraphR(df = hpi_lad,
               name = "BuyerType",
               vars = c("FOOPrice", 
                        "FTBPrice"),
               datefil = "2012-01-01",
               ttl = c("Average House Prices for different buyer types over time",
                 "Average House Sales Volume Changes Over Time"))

## West Berkshire Compared to England

AreaRegionCountryCompareR(df = hpi_lad, 
                          area = "West Berkshire",
                          datefil = "1995-01-01")

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

mod4 <- lm(data = train, formula = log(AveragePrice) ~ Supergroup*Rate)
summary(mod4)#$r.squared

ggplot2::ggplot(ggpredict(model = mod4, terms = c("Rate","Supergroup")), 
                aes(x = x, y = predicted, colour = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high),
              alpha = 0.1) +
  xlab("BoE Base Rate") +
  ylab("Predicted Average Price") +
  ggtitle(label = "Log of Average House Prices given BoE Base Rate in Supergroups",
          sublabwl = "With 95% confidence intervals") + 
  labs(colour = "Supergroup") 

rm(mod1,mod2,mod3,mod4)
gc()
 
# Classification Models ####

## London Cosmopolitan vs Countryside Living

train <- train[train$Supergroup %in% c("London Cosmopolitan","Countryside Living"),] %>%
  dplyr:: mutate(Supergroup = recode(Supergroup, 'London Cosmopolitan' = 1,
                                     'Countryside Living' = 0))

test <- test[test$Supergroup %in% c("London Cosmopolitan","Countryside Living"),] %>%
  dplyr:: mutate(Supergroup = recode(Supergroup, 'London Cosmopolitan' = 1,
                                     'Countryside Living' = 0))

## GLM for Average Price, Sales Volume and Country as Identifiers

glmod1 <- glm(data=train, formula = Supergroup ~ AveragePrice + SalesVolume+Country,
              family = binomial)

summary(glmod1)

## GLM for Type of Sales Volume and Funding status

glmod2 <- glm(data=train_lcvcl, formula = Supergroup ~ CashSalesVolume + NewSalesVolume, 
              family=binomial)

summary(glmod2)


