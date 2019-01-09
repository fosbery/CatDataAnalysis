library("openxlsx")
library('data.table')
library('dplyr')
path<-'/Users/jamesfung/Dropbox/Graduate School/Categorical Data Analysis/Project/'


#Read in crime data
crime<-fread('/Users/jamesfung/Dropbox/Graduate School/Categorical Data Analysis/Project/Chicago_Crimes_2012_to_2017.csv')

#Read in other datasets for merges.
comm<-read.csv('/Users/jamesfung/Dropbox/Graduate School/Categorical Data Analysis/Project/CommAreasMod.csv')

#Change column name to be able to merge, merge.
colnames(crime)[colnames(crime)=='Community Area']<-'CommunityArea'
crime2<-merge(crime,comm,by='CommunityArea')

#Import income by community area.
#income<-read.xlsx('/Users/jamesfung/Dropbox/Graduate School/Categorical Data Analysis/Project/Income by Neighborhood.xlsx')
#income[,1]<-toupper(income[,1])
#crime3<-merge(crime2,income,by='COMMUNITY')

#Import demographic information.
demo<-read.xlsx('/Users/jamesfung/Dropbox/Graduate School/Categorical Data Analysis/Project/USE_DEMO_REGION.xlsx')

#Merge into dataframe.
crime3<-merge(crime2,demo,by='COMMUNITY')

#Convert income into categorical values.
crime3$incomecat<-cut(crime3$MEDINC,c(0,24257,47177,100000,1000000),labels=c('Poverty','Lower Middle Class','Middle Class','Upper Middle Class'))

#Drop columns.
keeps<-c('COMMUNITY','Primary Type','Location Description','incomecat','Arrest','Domestic','Year','MED_AGE','UNEMP_PERC','OPEN_SPACE_ACRES','AVG_VMT_PER_100','RACE.MAJORITY','MED_ROOMS','INST_ACRES')
crime4<-select(crime3,keeps)
head(crime4,5)

#Keep only 2016 data for ease.
crime2016<-crime4[crime4$Year==2016]

#Get a count of number of observations in each crime.
freq<-as.data.frame(table(crime2016$`Primary Type`))
freq

#Append to dataset.
colnames(freq)[colnames(freq)=='Var1']<-'Primary Type'
crime2016_2<-merge(crime2016,freq,by='Primary Type')

#Filter for crimes that have more than 10 observations.
crime2016_3<-crime2016_2[crime2016_2$Freq>=10,]
freq_filter<-as.data.frame(table(crime2016_3$`Primary Type`))

#Recategorize crime variables.
crime2016_3$crimecat<-ifelse(
  crime2016_3$`Primary Type`=='ARSON' | crime2016_3$`Primary Type`=='HOMICIDE'|crime2016_3$`Primary Type`=='KIDNAPPING','Serious Other Crimes',
  
  ifelse(crime2016_3$`Primary Type`=='ASSAULT' | crime2016_3$`Primary Type`=='BATTERY','Assault',
         
         ifelse(crime2016_3$`Primary Type`=='BURGLARY' | crime2016_3$`Primary Type`=='MOTOR VEHICLE THEFT'|crime2016_3$`Primary Type`=='ROBBERY' | crime2016_3$`Primary Type`=='THEFT','Theft',
                
                ifelse(crime2016_3$`Primary Type`=='SEX OFFENSE' | crime2016_3$`Primary Type`=='CRIM SEXUAL ASSAULT','Sexual Offense',
                       
                       ifelse(crime2016_3$`Primary Type`=='DECEPTIVE PRACTICE' | crime2016_3$`Primary Type`=='GAMBLING'|crime2016_3$`Primary Type`=='CRIMINAL TRESPASS'|crime2016_3$`Primary Type`=='INTERFERENCE WITH PUBLIC OFFICER'|crime2016_3$`Primary Type`=='PUBLIC PEACE VIOLATION'|crime2016_3$`Primary Type`=='PROSTITUTION'|crime2016_3$`Primary Type`=='CRIMINAL DAMAGE','Minor Illegal Practices',
                              
                              ifelse(crime2016_3$`Primary Type`=='LIQUOR LAW VIOLATION'|crime2016_3$`Primary Type`=='NARCOTICS','Drugs and Alcohol','Other'))))))

#Get a new count of number of observations in new crime category.
freq2 <-as.data.frame(table(crime2016_3$crimecat))
freq2

#Drop other.
crime2016_3<-crime2016_3[crime2016_3$crimecat!='Other',]
freq3 <-as.data.frame(table(crime2016_3$crimecat))

#Round numbers.
crime2016_3$UNEMP_PERC=round(crime2016_3$UNEMP_PERC*100,0)
crime2016_3$OPEN_SPACE_ACRES=round(crime2016_3$OPEN_SPACE_ACRES,0)
crime2016_3$AVG_VMT_PER_100=round(crime2016_3$AVG_VMT_PER_100,0)
crime2016_3$MED_ROOMS=round(crime2016_3$MED_ROOMS,0)
crime2016_3$INST_ACRES=round(crime2016_3$INST_ACRES,0)

#Keep used columns.
keeps2<-c('Primary Type','incomecat','Arrest','MED_AGE','UNEMP_PERC','OPEN_SPACE_ACRES','AVG_VMT_PER_100','RACE.MAJORITY','MED_ROOMS','crimecat','INST_ACRES')
crime2016_fin<-select(crime2016_3,keeps2)

#Export out for SAS useage.
write.csv(crime2016_fin,file='/Users/jamesfung/Dropbox/Graduate School/Categorical Data Analysis/Project/crime2016_chicago.csv',row.names=TRUE)

