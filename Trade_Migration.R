setwd("~/Research/Thesis/Migration_Data")

library(dplyr)
library(readxl)
library(grr)
library(ggmap)
library(maps)
library(housingData)

# Collecting IRS migration YoY data into one df
## Loading in data

outflow16 = read.csv("countyoutflow1516.csv")
inflow16 = read.csv("countyinflow1516.csv")
outflow15 = read.csv("countyoutflow1415.csv")
inflow15 = read.csv("countyinflow1415.csv")
outflow14 = read.csv("countyoutflow1314.csv")
inflow14 = read.csv("countyinflow1314.csv")
outflow13 = read.csv("countyoutflow1213.csv")
inflow13 = read.csv("countyinflow1213.csv")
outflow12 = read.csv("countyoutflow1112.csv")
inflow12 = read.csv("countyinflow1112.csv")

outflow11 = read.csv("countyoutflow1011.csv")
inflow11 = read.csv("countyinflow1011.csv")
outflow10 = read.csv("countyoutflow0910.csv")
inflow10 = read.csv("countyinflow0910.csv")
outflow09 = read.csv("countyoutflow0809.csv")
inflow09 = read.csv("countyinflow0809.csv")

outflow08 = read.delim("co0708us.dat", sep = "", header = F, stringsAsFactors = F)
inflow08 = read.delim("ci0708us.dat", sep = "", header = F, stringsAsFactors = F)
outflow07 = read.delim("countyout0607.dat", sep = "", header = F, stringsAsFactors = F)
inflow07 = read.delim("countyin0607.dat", sep = "", header = F, stringsAsFactors = F)
# 05-06 data lacks county-by-county data
#outflow06 = read.delim("countyout0506.dat", sep = "", header = F, stringsAsFactors = F)
#inflow06 = read.delim("countyin0506.dat", sep = "", header = F, stringsAsFactors = F)
outflow05 = read.delim("countyout0405us1.dat", sep = "", header = F, stringsAsFactors = F)
inflow05 = read.delim("countyin0405us1.dat", sep = "", header = F, stringsAsFactors = F)

# Combining into one df
## First, fixing the .dat datasets
fixdat = function(dfs){
  for (i in 1:nrow(dfs)){
    if (dfs[i,12] == "" | is.na(dfs[i,12])) {
      dfs[i,12:15] = dfs[i,8:11]
      dfs[i,8:11] = NA
    } else if (dfs[i,13] == "" | is.na(dfs[i,13])) {
      dfs[i,12:15] = dfs[i,9:12]
      dfs[i,9:11] = NA
    } else if (dfs[i,14] == "" | is.na(dfs[i,14])) {
      dfs[i,12:15] = dfs[i,10:13]
      dfs[i,10:11] = NA
    } else if (dfs[i,14] == "" | is.na(dfs[i,15])) {
      dfs[i,12:15] = dfs[i,11:14]
      dfs[i,11] = NA
    }
  }
  
  dfs[,6:11] = NULL
  #dfs = subset(dfs, V5 != "US" & V3 < 96)
  
  colnames(dfs)[1:4] = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips")
  dfs$Flow = as.numeric(dfs$V12) + as.numeric(dfs$V13)
  
  return(dfs)
}

outflows08 = fixdat(outflow08)
outflows07 = fixdat(outflow07)
outflows05 = fixdat(outflow05)

outflows08[,5:9] = NULL
outflows07[,5:9] = NULL
outflows05[,5:9] = NULL

colnames(outflows08)[5] = "Outflow08"
colnames(outflows07)[5] = "Outflow07"
colnames(outflows05)[5] = "Outflow05"

inflows08 = fixdat(inflow08)
inflows07 = fixdat(inflow07)
inflows05 = fixdat(inflow05)

inflows08[,5:9] = NULL
inflows07[,5:9] = NULL
inflows05[,5:9] = NULL

colnames(inflows08)[5] = "Inflow08"
colnames(inflows07)[5] = "Inflow07"
colnames(inflows05)[5] = "Inflow05"

# Fixing other datasets
outflow16$Outflow16 = outflow16$n1+outflow16$n2
outflow16[,5:9] = NULL

outflow15$Outflow15 = outflow15$n1+outflow15$n2
outflow15[,5:9] = NULL

outflow14$Outflow14 = outflow14$n1+outflow14$n2
outflow14[,5:9] = NULL

outflow13$Outflow13 = outflow13$n1+outflow13$n2
outflow13[,5:9] = NULL

outflow12$Outflow12 = outflow12$n1+outflow12$n2
outflow12[,5:9] = NULL

outflow11$Outflow11 = outflow11$Return_Num+outflow11$Exmpt_Num
outflow11[,5:9] = NULL
colnames(outflow11)[1:4] = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips")

outflow10$Outflow10 = outflow10$Return_Num+outflow10$Exmpt_Num
outflow10[,5:9] = NULL
colnames(outflow10)[1:4] = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips")

outflow09$Outflow09 = outflow09$Return_Num+outflow09$Exmpt_Num
outflow09[,5:9] = NULL
colnames(outflow09)[1:4] = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips")

inflow16$Inflow16 = inflow16$n1+inflow16$n2
inflow16[,5:9] = NULL

inflow15$Inflow15 = inflow15$n1+inflow15$n2
inflow15[,5:9] = NULL

inflow14$Inflow14 = inflow14$n1+inflow14$n2
inflow14[,5:9] = NULL

inflow13$Inflow13 = inflow13$n1+inflow13$n2
inflow13[,5:9] = NULL

inflow12$Inflow12 = inflow12$n1+inflow12$n2
inflow12[,5:9] = NULL

inflow11$Inflow11 = inflow11$Return_Num+inflow11$Exmpt_Num
inflow11[,5:9] = NULL
colnames(inflow11)[1:4] = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips")

inflow10$Inflow10 = inflow10$Return_Num+inflow10$Exmpt_Num
inflow10[,5:9] = NULL
colnames(inflow10)[1:4] = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips")

inflow09$Inflow09 = inflow09$Return_Num+inflow09$Exmpt_Num
inflow09[,5:9] = NULL
colnames(inflow09)[1:4] = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips")

# Merge datasets
flow_df = merge(outflow16, outflow15, by = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips"),all=T)

flow_df = merge(flow_df, outflow14, by = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips"),all=T)
flow_df = merge(flow_df, outflow13, by = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips"),all=T)
flow_df = merge(flow_df, outflow12, by = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips"),all=T)
flow_df = merge(flow_df, outflow11, by = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips"),all=T)
flow_df = merge(flow_df, outflow10, by = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips"),all=T)
flow_df = merge(flow_df, outflow09, by = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips"),all=T)
flow_df = merge(flow_df, outflows08, by = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips"),all=T)
flow_df = merge(flow_df, outflows07, by = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips"),all=T)
flow_df = merge(flow_df, outflows05, by = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips"),all=T)

flow_df = merge(flow_df, inflow16, by = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips"),all=T)
flow_df = merge(flow_df, inflow15, by = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips"),all=T)
flow_df = merge(flow_df, inflow14, by = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips"),all=T)
flow_df = merge(flow_df, inflow13, by = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips"),all=T)
flow_df = merge(flow_df, inflow12, by = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips"),all=T)
flow_df = merge(flow_df, inflow11, by = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips"),all=T)
flow_df = merge(flow_df, inflow10, by = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips"),all=T)
flow_df = merge(flow_df, inflow09, by = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips"),all=T)
flow_df = merge(flow_df, inflows08, by = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips"),all=T)
flow_df = merge(flow_df, inflows07, by = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips"),all=T)
flow_df = merge(flow_df, inflows05, by = c("y1_statefips", "y1_countyfips", "y2_statefips", "y2_countyfips"),all=T)

# Adding population data
cty00 = read.csv("co-est00int-tot.csv")[,-c(1:3,6:8,19,20)]
cty10 = read.csv("co-est2018-alldata.csv")[,c(4,5,10:18)]

flow_df = merge(flow_df, cty00, by.x=c("y1_statefips", "y1_countyfips"), by.y = c("STATE", "COUNTY"),all.x=T)
colnames(flow_df)[(ncol(flow_df)-ncol(cty00)+3):ncol(flow_df)] = paste0("orig_",
                                                                        colnames(flow_df)[(ncol(flow_df)-ncol(cty00)+3):ncol(flow_df)])

flow_df = merge(flow_df, cty00, by.x=c("y2_statefips", "y2_countyfips"), by.y = c("STATE", "COUNTY"),all.x=T)
colnames(flow_df)[(ncol(flow_df)-ncol(cty00)+3):ncol(flow_df)] = paste0("dest_",
                                                                        colnames(flow_df)[(ncol(flow_df)-ncol(cty00)+3):ncol(flow_df)])

flow_df = merge(flow_df, cty10, by.x=c("y1_statefips", "y1_countyfips"), by.y = c("STATE", "COUNTY"),all.x=T)
colnames(flow_df)[(ncol(flow_df)-ncol(cty10)+3):ncol(flow_df)] = paste0("orig_",
                                                                        colnames(flow_df)[(ncol(flow_df)-ncol(cty10)+3):ncol(flow_df)])

flow_df = merge(flow_df, cty10, by.x=c("y2_statefips", "y2_countyfips"), by.y = c("STATE", "COUNTY"),all.x=T)
colnames(flow_df)[(ncol(flow_df)-ncol(cty10)+3):ncol(flow_df)] = paste0("dest_",
                                                                        colnames(flow_df)[(ncol(flow_df)-ncol(cty10)+3):ncol(flow_df)])


# Key is getting distances:
## Use mean lat/long of each CFS area (basically means of county boundaries in CFS Areas)
## We don't weigh the distance by the population
geoCounty$statefips = as.numeric(substr(as.character(geoCounty$fips), 1,2))
geoCounty$countyfips = as.numeric(substr(as.character(geoCounty$fips), 3,5))

#cty_center = t(combn(geoCounty$fips,2))
#colnames(cty_center) = c("orig","dest")
flow_df$orig_lon = geoCounty[match(paste(flow_df$y1_statefips,flow_df$y1_countyfips,sep=","),
                                   paste(geoCounty$statefips,geoCounty$countyfips,sep=",")),4]
flow_df$orig_lat = geoCounty[match(paste(flow_df$y1_statefips,flow_df$y1_countyfips,sep=","),
                                   paste(geoCounty$statefips,geoCounty$countyfips,sep=",")),5]
flow_df$dest_lon = geoCounty[match(paste(flow_df$y2_statefips,flow_df$y2_countyfips,sep=","),
                                   paste(geoCounty$statefips,geoCounty$countyfips,sep=",")),4]
flow_df$dest_lat = geoCounty[match(paste(flow_df$y2_statefips,flow_df$y2_countyfips,sep=","),
                                   paste(geoCounty$statefips,geoCounty$countyfips,sep=",")),5]

spherical_law_cosines = function(lat1,lon1,lat2,lon2){
  return(cos(sin(lat1*pi/180)*sin(lat2*pi/180) + 
               cos(lat1*pi/180)*cos(lat2*pi/180)*
               cos(lon2*pi/180-lon1*pi/180)) * 6371000)
}

flow_df$distance = spherical_law_cosines(flow_df$orig_lat,
                                            flow_df$orig_lon,
                                            flow_df$dest_lat,
                                            flow_df$dest_lon)

# When aggregating to CFS, multiply by population and 
## then divide by the sum of the population of each county
## For populating weighting

# Read in trade data
cfs07 = read.csv("../CFS_2007_00A20/CFS_2007_00A20.csv")
cfs12 = read.csv("../CFS_2012_00A19/CFS_2012_00A19.csv")
# cfs12 = read.csv("../cfs_2012_pumf_csv.txt")
# 
# # Aggregating data for 2012 (since it's at shipment level)
# cfs12 = cfs12 %>%
#   group_by(ORIG_CFS_AREA, DEST_CFS_AREA) %>%
#   summarize(ORIG_STATE = ORIG_STATE[1],
#             ORIG_MA = ORIG_MA[1],
#             DEST_STATE = DEST_STATE[1],
#             DEST_MA = DEST_MA[1],
#             SHIPMT_VALUE = sum(SHIPMT_VALUE,na.rm=T))


cfs_cwk = read_xlsx("../cfs-area-lookup-2007-and-2012.xlsx", sheet = "CFS Areas", skip=1)

cfs_cwk$`ANSI ST` = as.numeric(as.character(cfs_cwk$`ANSI ST`))
cfs_cwk$`ANSI CNTY` = as.numeric(as.character(cfs_cwk$`ANSI CNTY`))

flow_df$orig_cfs = cfs_cwk[match(paste(flow_df$y1_statefips, flow_df$y1_countyfips,sep=","), paste(cfs_cwk$`ANSI ST`, cfs_cwk$`ANSI CNTY`,sep=",")),9]$CFS07_GEOID
flow_df$dest_cfs = cfs_cwk[match(paste(flow_df$y2_statefips, flow_df$y2_countyfips,sep=","), paste(cfs_cwk$`ANSI ST`, cfs_cwk$`ANSI CNTY`,sep=",")),8]$CFS07_DDESTGEO

flow_df$orig_cfs12 = cfs_cwk[match(paste(flow_df$y1_statefips, flow_df$y1_countyfips,sep=","), paste(cfs_cwk$`ANSI ST`, cfs_cwk$`ANSI CNTY`,sep=",")),10]$CFS12_GEOID
flow_df$dest_cfs12 = cfs_cwk[match(paste(flow_df$y2_statefips, flow_df$y2_countyfips,sep=","), paste(cfs_cwk$`ANSI ST`, cfs_cwk$`ANSI CNTY`,sep=",")),10]$CFS12_GEOID

# orig_cfs12_row = match(paste(flow_df$y1_statefips, flow_df$y1_countyfips,sep=","), paste(cfs_cwk$`ANSI ST`, cfs_cwk$`ANSI CNTY`,sep=","))
# dest_cfs12_row = match(paste(flow_df$y2_statefips, flow_df$y2_countyfips,sep=","), paste(cfs_cwk$`ANSI ST`, cfs_cwk$`ANSI CNTY`,sep=","))
# 
# flow_df$orig_cfs12 = paste(cfs_cwk$`ANSI ST`[orig_cfs12_row], cfs_cwk$CFS12_AREA[orig_cfs12_row], sep=",")
# flow_df$dest_cfs12 = paste(cfs_cwk$`ANSI ST`[dest_cfs12_row], cfs_cwk$CFS12_AREA[dest_cfs12_row], sep=",")

# Let's remove all the regions/US/foreign figures
flow_df = subset(flow_df, y1_statefips < 57 & y2_statefips < 57 & y1_statefips >= 0 & y2_statefips >= 0)

flow_df$trade_out_07 = cfs07$VAL[match(paste(flow_df$orig_cfs,as.numeric(flow_df$dest_cfs)), paste(as.character(cfs07$GEO.id),cfs07$DDESTGEO.id))]
flow_df$trade_out_12 = cfs12$VAL[match(paste(flow_df$orig_cfs12,flow_df$dest_cfs12), paste(as.character(cfs12$GEO.id),cfs12$DDESTGEO.id))]

# flow_df$trade_out_12 = cfs12$SHIPMT_VALUE[match(paste(flow_df$orig_cfs12,flow_df$dest_cfs12), 
#                                        paste(paste(cfs12$ORIG_STATE, cfs12$ORIG_MA, sep=","),
#                                              paste(cfs12$DEST_STATE, cfs12$DEST_MA, sep=",")))]

# Add in trade inflows as well
flow_df$trade_in_07 = flow_df$trade_out_07[match(paste(paste(flow_df$y1_statefips, flow_df$y1_countyfips,sep=","), 
                                                       paste(flow_df$y2_statefips, flow_df$y2_countyfips,sep=",")), 
                                                 paste(paste(flow_df$y2_statefips, flow_df$y2_countyfips,sep=","), 
                                                       paste(flow_df$y1_statefips, flow_df$y1_countyfips,sep=",")))]

flow_df$trade_in_12 = flow_df$trade_out_12[match(paste(paste(flow_df$y1_statefips, flow_df$y1_countyfips,sep=","), 
                                                       paste(flow_df$y2_statefips, flow_df$y2_countyfips,sep=",")), 
                                                 paste(paste(flow_df$y2_statefips, flow_df$y2_countyfips,sep=","), 
                                                       paste(flow_df$y1_statefips, flow_df$y1_countyfips,sep=",")))]

# flow_df$trade_in_12 = flow_df$trade_out_12[match(paste(flow_df$orig_cfs12, flow_df$dest_cfs12, sep=","), 
#                                                  paste(flow_df$dest_cfs12, flow_df$orig_cfs12, sep=","))]


# flow_cfs$trade_out_12 = flow_cfs$trade_out_12 / 1000000
# flow_cfs$trade_in_12 = flow_cfs$trade_in_12 / 1000000

# Getting population estimate per CFS Area
flow_df2 = flow_df %>%
  group_by(y1_statefips, y1_countyfips) %>%
  mutate_at(vars(orig_POPESTIMATE2000:orig_POPESTIMATE2018),
            function(x){ifelse(all(is.na(x)), NA, x[which(!is.na(x))])})


# Unique origin counties
unik <- !(duplicated(paste(flow_df$y1_countyfips,flow_df$y1_statefips)))  ## logical vector of unique values
destk <- !(duplicated(paste(flow_df$y2_countyfips, flow_df$y2_statefips)))  ## logical vector of unique dest values

# Rough Approximation: Assuming population values not duplicated for any county in a CFS Area

funs_cfs = list(
  origs=function(x){
    unik <- !(duplicated(x))
    sum(x[unik],na.rm=T)},
  dests=function(x){
    destk <- !(duplicated(x))
    sum(x[destk],na.rm=T)},
  migs=function(x){sum(x,na.rm=T)})


# Aggregating data at CFS level
## Not Population Weighted Distance (CFS Area should take care of that)
flow_cfs = flow_df %>%
  group_by(orig_cfs12,dest_cfs12) %>%
  mutate(trade_out_07 = ifelse(all(is.na(trade_out_07)),NA,trade_out_07[which(!is.na(trade_out_07))]),
         trade_in_07 = ifelse(all(is.na(trade_in_07)),NA,trade_in_07[which(!is.na(trade_in_07))]),
         trade_out_12 = ifelse(all(is.na(trade_out_12)),NA,trade_out_12[which(!is.na(trade_out_12))]),
         trade_in_12 = ifelse(all(is.na(trade_in_12)),NA,trade_in_12[which(!is.na(trade_in_12))]),
         distances = mean(distance,na.rm=T)) %>%
  group_by(orig_cfs12, dest_cfs12, 
           trade_out_07, trade_in_07, trade_out_12, trade_in_12,distances) %>%
  summarise_at(vars(Outflow16:Inflow05,orig_POPESTIMATE2000:dest_POPESTIMATE2018),
               funs_cfs)

# Removing unnecessary columns
flow_cfs = flow_cfs[,-c(152:189)]
flow_cfs = flow_cfs[,-c(112:120)]
flow_cfs = flow_cfs[,-c(61:101)]
flow_cfs = flow_cfs[,-c(42:51)]
flow_cfs = flow_cfs[,-c(10:31)]




# 09/23/19: Let's calculate the gravity equation 
# Gravity Equation for Migration
## ln(F_{ij}) = \beta_0 + \beta_1 ln(M_i) + \beta_2 ln(M_j) - \beta_3 ln(D_{ij}) + \epsilon_{ij}
## Here, M stands for population and D for distance

# Regression for 2007
flow_cfs_reg = subset(flow_cfs, Outflow08_migs+Inflow08_migs > 0)

mig_reg1 = summary(lm(I(log(Outflow08_migs+Inflow08_migs)) ~ I(log(distances)) + 
                        I(log(orig_POPESTIMATE2006_origs)) + I(log(dest_POPESTIMATE2006_dests)) + 
                        I(log(trade_out_07+trade_in_07)), data=flow_cfs_reg))

mig_reg2 = summary(lm(I(log(Outflow08_migs+Inflow08_migs)) ~ I(log(distances)) + 
                        I(log(orig_POPESTIMATE2006_origs)) + I(log(dest_POPESTIMATE2006_dests)), data=flow_cfs_reg))


# Regression for 2012
flow_cfs_reg2 = subset(flow_cfs, Outflow13_migs+Inflow13_migs > 0)

mig_reg12 = summary(lm(I(log(Outflow13_migs+Inflow13_migs)) ~ I(log(distances)) + 
                        I(log(orig_POPESTIMATE2011_origs)) + I(log(dest_POPESTIMATE2011_dests)) + 
                        I(log(trade_out_07+trade_in_07)), data=flow_cfs_reg2))

mig_reg12_2 = summary(lm(I(log(Outflow13_migs+Inflow13_migs)) ~ I(log(distances)) + 
                        I(log(orig_POPESTIMATE2011_origs)) + I(log(dest_POPESTIMATE2011_dests)), data=flow_cfs_reg2))


# Subsetting to top 20 CFS Areas by population
flow_cfs_max = subset(flow_cfs,orig_cfs12==dest_cfs12)
flow_cfs_max$orig_POPESTIMATE2012_origs[order(-flow_cfs_max$orig_POPESTIMATE2012_origs)[1:20]]



flow_cfs$orig_POPESTIMATE2012



flow_cfs_reg2 = subset(flow_cfs_reg, )










# Trade per capita
pop_red = flow_cfs %>%
  group_by(dest_cfs) %>%
  summarize(POP07 = ifelse(all(is.na(dest_POPESTIMATE2007)),NA,dest_POPESTIMATE2007[which(!is.na(dest_POPESTIMATE2007))[1]]))

trade_per07 = sum(flow_cfs$trade_out_07,na.rm=T)/sum(pop_red$POP07)

pop_red2 = flow_cfs %>%
  group_by(dest_cfs12) %>%
  summarize(POP12 = ifelse(all(is.na(dest_POPESTIMATE2012)),NA,dest_POPESTIMATE2012[which(!is.na(dest_POPESTIMATE2012))[1]]))
trade_per = sum(flow_cfs$trade_out_12,na.rm=T)/sum(pop_red2$POP12)


# What would we expect trade flows to be in 2012 from 2007 with just pop. growth
## (Existing trade network, outflow change only depends on pop. growth in area receiving flows)
flow_cfs$Exp_Trade = flow_cfs$dest_POPESTIMATE2012/flow_cfs$dest_POPESTIMATE2007 * flow_cfs$trade_out_07 * trade_per/trade_per07
flow_cfs$Exp_Trade2 = flow_cfs$orig_POPESTIMATE2012/flow_cfs$orig_POPESTIMATE2007 * flow_cfs$trade_in_07 * trade_per/trade_per07

# EDA: Which counties had greatest percent inrease in outflows from '07 to '12
surprises = flow_cfs %>%
  group_by(orig_cfs12) %>%
  summarize(change = (sum(Outflow12,na.rm=T)-sum(Outflow07,na.rm=T))/sum(Outflow07,na.rm=T))

## New Orleans had the biggest change, seeing 40% drop in outflows.
## Look at '07 migration data to see where they went from '05 to '06
### And actually where they went from '05 to '11. Then, compare to flows

flow_cfs$Trade_Out_Fin = flow_cfs$trade_out_07
flow_cfs$Trade_In_Fin = flow_cfs$trade_in_07



#norleans = subset(flow_cfs, flow_cfs$dest=new_orleans)
flow_cfs$Tot_Out = flow_cfs$Outflow07 + flow_cfs$Outflow08+flow_cfs$Outflow09+
  flow_cfs$Outflow10+flow_cfs$Outflow11+flow_cfs$Outflow12 # Maybe subtract inflows?
flow_cfs$Tot_In = flow_cfs$Inflow07+flow_cfs$Inflow08+flow_cfs$Inflow09+
  flow_cfs$Inflow10+flow_cfs$Inflow11+flow_cfs$Inflow12

flow_cfs$dest_cfs12_repeat = flow_cfs$dest_cfs12
outflow_fin = function(norleans){
  norleans$existing_net = NA
  for (i in 1:nrow(norleans)){
    trades = flow_cfs[matches(norleans$orig_cfs12[i], flow_cfs$orig_cfs12, all.y=F)[,2],
                      c("orig_cfs12","dest_cfs12","trade_out_07")]
    
    trades$pct_trade = trades$trade_out_07 / sum(trades$trade_out_07,na.rm=T)
    
    norleans$Tot_Mig = norleans$Tot_Out
    norleans$Tot_Mig2 = norleans$Tot_Out - norleans$Tot_In
    
    cty_inc = norleans$Tot_Mig[i] * trades$pct_trade * trade_per
    
    norleans$Trade_Out_Fin = rowSums(cbind(norleans$Trade_Out_Fin, cty_inc[match(norleans$orig_cfs12, trades$dest_cfs12)]),na.rm=T)
    
    if(norleans$orig_cfs12[i] == norleans$dest_cfs12_repeat[1]){
      norleans$existing_net = norleans$trade_out_07 / sum(norleans$trade_out_07,na.rm=T)
    }
  }
  # Adding in trade increase expected from natural population increase (births-deaths)
  norleans$Trade_Out_Fin = rowSums(cbind(norleans$Trade_Out_Fin, (norleans$dest_POPESTIMATE2012 - norleans$dest_POPESTIMATE2007 - 
                                                                    sum(norleans$Tot_Mig,na.rm=T)) * trade_per * norleans$existing_net),na.rm=T)
  return(norleans)
}


# Removing NAs from analysis and then getting change to existing network
outflow_post = subset(flow_cfs, !is.na(orig_cfs12) & !is.na(dest_cfs12_repeat)) %>%
  group_by(dest_cfs12) %>%
  group_map(~ outflow_fin(.x))

flow_cfs$orig_cfs12_repeat = flow_cfs$orig_cfs12
inflow_fin = function(norleans){
  for (i in 1:nrow(norleans)){
    trades = flow_cfs[matches(norleans$dest_cfs12[i], flow_cfs$dest_cfs12, all.y=F)[,2],
                      c("orig_cfs12","dest_cfs12","trade_out_07")]
    trades$pct_trade = trades$trade_out_07 / sum(trades$trade_out_07,na.rm=T)
    norleans$Tot_Mig = norleans$Tot_Out
    norleans$Tot_Mig2 = norleans$Tot_Out - norleans$Tot_In
    
    #norleans$pop_inc = norleans$pop12 - norleans$pop07
    
    cty_inc = norleans$Tot_Mig[i] * trades$pct_trade * trade_per
    norleans$Trade_In_Fin = rowSums(cbind(norleans$Trade_In_Fin, cty_inc[match(norleans$dest_cfs12, trades$orig_cfs12)]),na.rm=T)
  }
  
  # Adding in trade increase expected from natural population increase (births-deaths)
  # norleans$Trade_In_Fin = rowSums(cbind(norleans$Trade_In_Fin, (norleans$orig_POPESTIMATE2012 - norleans$orig_POPESTIMATE2007 + 
  #                                                     sum(norleans$Tot_Mig,na.rm=T)) * trade_per),na.rm=T)
  
  return(norleans)
}

inflow_post = flow_cfs %>%
  group_by(orig_cfs12) %>%
  group_map(~ inflow_fin(.x))

# Mapping
counties = map_data("county")
data("county.fips")

county.fips$region = substr(county.fips$polyname, 1,(regexpr("\\,",county.fips$polyname)-1))
county.fips$subregion = substr(county.fips$polyname, (regexpr("\\,",county.fips$polyname)+1), nchar(county.fips$polyname))
county.fips$subregion = ifelse(regexpr("\\:",county.fips$subregion) == -1,
                               county.fips$subregion,substr(county.fips$subregion,1,
                                                            regexpr("\\:",county.fips$subregion)-1))

full_county = merge(counties, county.fips, by = c("region","subregion"),all.x=T)

full_county$`ANSI CNTY` = as.numeric(substr(as.character(full_county$fips),nchar(as.character(full_county$fips))-2,nchar(as.character(full_county$fips))))
full_county$`ANSI ST` = as.numeric(substr(as.character(full_county$fips),
                                          1,nchar(as.character(full_county$fips))-3))

fuller = merge(full_county, cfs_cwk, by = c("ANSI ST", "ANSI CNTY"),all.x=T)
testing = merge(full_county, cfs_cwk, by = c("ANSI ST", "ANSI CNTY"),all=T)

# Mapping New Orleans
for(i in 1:length(inflow_post)){
  if(inflow_post[[i]]$orig_cfs12_repeat[1] == "E330000US2240600000"){
    #fuller2 = merge(fuller, inflow_post[[i]],by.x="CFS12_GEOID",by.y = "dest_cfs12",all.x=T)
    fuller$Trade_Fin_In = inflow_post[[i]]$Trade_In_Fin[match(fuller$CFS12_GEOID, inflow_post[[i]]$dest_cfs12)]
    fuller$Exp_Trade = inflow_post[[i]]$Exp_Trade[match(fuller$CFS12_GEOID, inflow_post[[i]]$dest_cfs12)]
    fuller$Exp_Trade2 = inflow_post[[i]]$Exp_Trade2[match(fuller$CFS12_GEOID, inflow_post[[i]]$dest_cfs12)]
    fuller$trade_in_12 = inflow_post[[i]]$trade_in_12[match(fuller$CFS12_GEOID, inflow_post[[i]]$dest_cfs12)]
    fuller$trade_in_07 = inflow_post[[i]]$trade_in_07[match(fuller$CFS07_DDESTGEO, inflow_post[[i]]$dest_cfs)]
    break
  }
}


ditch_the_axis <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

fuller2 = subset(fuller, CFS12_GEOID != "E330000US2240600000")

# My prediction
ggplot(data = fuller, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray") +
  geom_polygon(data = fuller2, 
               aes(fill = Trade_Fin_In)) +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() + 
  scale_fill_gradientn(colours = rev(rainbow(7))) + 
  ditch_the_axis +
  labs(fill="Trade Amount")

# Expected Prediction
ggplot(data = fuller, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray") +
  geom_polygon(data = fuller2, 
               aes(fill = Exp_Trade2)) +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() + 
  scale_fill_gradientn(colours = rev(rainbow(7))) + 
  ditch_the_axis +
  labs(fill="Trade Amount")

# Actual Value - 2007
ggplot(data = fuller, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray") +
  geom_polygon(data = fuller2, 
               aes(fill = trade_in_07)) +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() + 
  scale_fill_gradientn(colours = rev(rainbow(7))) + 
  ditch_the_axis +
  labs(fill="Trade Amount")

# Actual Value - 2012
ggplot(data = fuller, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray") +
  geom_polygon(data = fuller2, 
               aes(fill = trade_in_12)) +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() + 
  scale_fill_gradientn(colours = rev(rainbow(7))) + 
  ditch_the_axis +
  labs(fill="Trade Amount")


# Predicted vs. Actual Trade Flows (compared to proprtional)
my_err = c()
exp_err = c()
act_err=c()
my_sum = 0
exp_sum = 0
act_sum=0
for (i in 1:length(outflow_post)){
  my_err = c(my_err, mean(abs(outflow_post[[i]]$Trade_Out_Fin - outflow_post[[i]]$trade_out_12),na.rm=T))
  exp_err = c(exp_err, mean(abs(outflow_post[[i]]$Exp_Trade - outflow_post[[i]]$trade_out_12),na.rm=T))
  act_err = c(act_err, mean((outflow_post[[i]]$trade_out_12),na.rm=T))
  
  my_sum = my_sum + sum(outflow_post[[i]]$Trade_Out_Fin,na.rm=T)
  exp_sum = exp_sum + sum(outflow_post[[i]]$Exp_Trade,na.rm=T)
  act_sum = act_sum + sum(outflow_post[[i]]$trade_out_12,na.rm=T)
  
}

my_sum
exp_sum
act_sum

median(my_err,na.rm=T)
median(exp_err,na.rm=T)
median(act_err,na.rm=T)


# 1. Adjust for population increase in my model
# 2. Multiply by coefficient for exp_trade to account for faster trade growth 
## (or multiply by pop. growth in corresponding economy -- remember gravity equation)
# 3. Make map for New Orleans

# Questions:
## Why is the size of trades in the functions only 100 rather than 101?
## Maybe the issue with why we are getting total sums less than actual flows is because of NAs during calculations?
## The other issue could be that trade per capita exploded by 2012 compared to 2007 and we are using 2007 figures.
### From 0.0692427 to 0.07735276


# 09/21/2019
# Gravity Equation for Migration
## ln(F_{ij}) = \beta_0 + \beta_1 ln(M_i) + \beta_2 ln(M_j) - \beta_3 ln(D_{ij}) + \epsilon_{ij}
## Here, M stands for population and D for distance

fuller2 = merge(fuller2, cty_center,by.x=c(),
                by.y=c("orig","dest"),all.x=T)

# Regression
lm(ln(Migration_Out) ~ I(ln(distance)) + I(ln(orig_pop)) + I(ln(dest_pop)),data=fuller2)

# Filter to top 20 cities by population


#fuller2$Trade_Fin_Out = 






