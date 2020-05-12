 # Load Env

source(paste0(getwd(), "/env.R"))

# Create Connection

con <- dbConnect(RSQLite::SQLite(), ":memory:")

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

dbWriteTable(con, "pp19", pp19)

rm(pp19)

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
 # dplyr:: select("CTY17NM", "GOR10NM", "CTRY17NM")  %>%
  dplyr:: rename(
    County = CTY17NM,
    Region = GOR10NM,
    Country = CTRY17NM
                ) %>%
  stats:: na.omit()

dbWriteTable(con, "geomap", geomap)

rm(geomap)

gc()

dbListFields(con, "pp19")

dbListFields(con, "geomap")

res <- dbSendQuery(con, "SELECT Price, Date, Postcode, PropertyType, County, 
                                Region, Country  
                         FROM pp19
                         LEFT JOIN geomap
                         USING (County);")


x <- dbFetch(res)

###

CountyGraphR <- function(Regs, PriceLim){

tst <- pp19[pp19$County %in% geomap[geomap$Region %in% Regs,]$County,] # London

ggplot(tst %>%
         dplyr:: filter(Price < PriceLim) %>%
         dplyr:: group_by(Month = month(Date, label = T), County) %>%
         dplyr:: summarise(MeanPrice = mean(Price),
                           NSale = n())) +
  geom_point(aes(x = Month, y = MeanPrice, colour = County, size = NSale))

}

CountyGraphR(c("London", "South East"), 500000)
