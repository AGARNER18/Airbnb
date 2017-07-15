
# find name of file needed
dir()

#*******************NEW_YORK************************

# load data
new_york<-read.csv("new_york.csv")

# look at names
names(new_york)

# subset to only include relevant variables
new_york<-new_york[,c(1,20, 22, 23, 24, 26, 27, 29, 34, 40,43, 49, 50, 52:54, 61,68,69,75,77,80:86,90,91)]

names(new_york)

# change rows that are missing to NA
new_york$host_response_time[new_york$host_response_time %in% 'N/A']<-NA
new_york$host_response_rate[new_york$host_response_rate %in% 'N/A']<-NA
new_york<-droplevels(new_york)

# find missing values in each variable
apply(is.na(new_york),2,sum)

# remove rows where values are missing to reduce number of rows in dataset before sampling
new_york<-new_york[complete.cases(new_york),]

library(sqldf)
# find neigbourhoods that have the most listings
sqldf('select neighbourhood_cleansed, count(*) from new_york group by neighbourhood_cleansed order by count(*) desc')
# subset to only include the neighborhoods with the most listings in new york
new_york<-new_york[new_york$neighbourhood_cleansed %in% c('Williamsburg', 'Bedford-Stuyvesant', 
                                                          'Harlem', 'East Village', 'Bushwick', 
                                                          'Upper West Side', 'Hell\'s Kitchen', 
                                                          'Upper East Side'),]
# drop unused levels
new_york<-droplevels(new_york)
# convert long form of state into abbreviation
new_york$host_location<-as.character(new_york$host_location)
new_york$host_location[grepl("New York", new_york$host_location, ignore.case=FALSE)] <- "NY"
new_york$host_location[grepl("NY", new_york$host_location, ignore.case=FALSE)] <- "NY"
new_york$host_location[grepl("new york", new_york$host_location, ignore.case=FALSE)] <- "NY"
new_york$host_location[grepl("Florida", new_york$host_location, ignore.case=FALSE)] <- "FL"
new_york$host_location[grepl("California", new_york$host_location, ignore.case=FALSE)] <- "CA"
new_york$host_location[grepl("Connecticut", new_york$host_location, ignore.case=FALSE)] <- "CT"
new_york$host_location[grepl("Maryland", new_york$host_location, ignore.case=FALSE)] <- "MA"
new_york$host_location[grepl("District of Columbia", new_york$host_location, ignore.case=FALSE)] <- "DC"
new_york$host_location[grepl("Pennsylvania", new_york$host_location, ignore.case=FALSE)] <- "PA"
new_york$host_location[grepl("Virginia", new_york$host_location, ignore.case=FALSE)] <- "VA"
new_york$host_location[grepl("Arizona", new_york$host_location, ignore.case=FALSE)] <- "AZ"
new_york$host_location[grepl("Missouri", new_york$host_location, ignore.case=FALSE)] <- "MO"
new_york$host_location[grepl("Colorado", new_york$host_location, ignore.case=FALSE)] <- "CO"
new_york$host_location[grepl("Ohio", new_york$host_location, ignore.case=FALSE)] <- "OH"
new_york$host_location[grepl("Washington", new_york$host_location, ignore.case=FALSE)] <- "WA"
new_york$host_location[grepl("Utah", new_york$host_location, ignore.case=FALSE)] <- "UT"
new_york$host_location[grepl("Massachusetts", new_york$host_location, ignore.case=FALSE)] <- "MA"
new_york$host_location[grepl("Texas", new_york$host_location, ignore.case=FALSE)] <- "TX"
new_york$host_location[grepl("Oregon", new_york$host_location, ignore.case=FALSE)] <- "OR"
new_york$host_location[grepl("Nebraska", new_york$host_location, ignore.case=FALSE)] <- "NE"
new_york$host_location[grepl("Michigan", new_york$host_location, ignore.case=FALSE)] <- "MI"
new_york$host_location[grepl("Louisiana", new_york$host_location, ignore.case=FALSE)] <- "LA"
new_york$host_location[grepl("Nevada", new_york$host_location, ignore.case=FALSE)] <- "NV"
new_york$host_location[grepl("Hawaii", new_york$host_location, ignore.case=FALSE)] <- "HI"
new_york$host_location[grepl("Iowa", new_york$host_location, ignore.case=FALSE)] <- "IA"
new_york$host_location[grepl("upstairs", new_york$host_location, ignore.case=FALSE)] <- "NY"
new_york$host_location[grepl("Georgia", new_york$host_location, ignore.case=FALSE)] <- "GA"
new_york$host_location[grepl("New Mexico", new_york$host_location, ignore.case=FALSE)] <- "NM"
new_york$host_location[grepl("Brooklyn", new_york$host_location, ignore.case=FALSE)] <- "NY"
new_york$host_location[grepl("brooklyn", new_york$host_location, ignore.case=FALSE)] <- "NY"
new_york$host_location[grepl("New Jersey", new_york$host_location, ignore.case=FALSE)] <- "NJ"
new_york$host_location[grepl("Kansas", new_york$host_location, ignore.case=FALSE)] <- "KS"
new_york$host_location[grepl("South Carolina", new_york$host_location, ignore.case=FALSE)] <- "SC"
new_york$host_location[grepl("Rhode Island", new_york$host_location, ignore.case=FALSE)] <- "RI"
new_york$host_location[grepl("Minnesota", new_york$host_location, ignore.case=FALSE)] <- "MN"
new_york$host_location[grepl("Tennessee", new_york$host_location, ignore.case=FALSE)] <- "TN"
new_york$host_location[grepl("North Dakota", new_york$host_location, ignore.case=FALSE)] <- "ND"
new_york$host_location[grepl("North Carolina", new_york$host_location, ignore.case=FALSE)] <- "NC"
new_york$host_location[grepl("Illinois", new_york$host_location, ignore.case=FALSE)] <- "IL"
new_york$host_location[grepl("Vermont", new_york$host_location, ignore.case=FALSE)] <- "VT"
new_york$host_location[grepl("Nre York", new_york$host_location, ignore.case=FALSE)] <- "KS"
new_york$host_location[grepl("United States", new_york$host_location, ignore.case=FALSE)] <- "US"
new_york$host_location[grepl("Kansas", new_york$host_location, ignore.case=FALSE)] <- "KS"
new_york$host_location[grepl("Spain", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Venezuela", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("France", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Germany", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Chile", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Japan", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Mexico", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Italy", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Canada", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Austria", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Australia", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Switzerland", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Portugal", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Brazil", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Israel", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Sweden", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Taiwan", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Costa Rica", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Russia", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Turkey", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Singapore", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Uzbekistan", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Czech Republic", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Iceland", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("harlem", new_york$host_location, ignore.case=FALSE)] <- "NY"
new_york$host_location[grepl("Uruguay", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Spain", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Montenegro", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Thailand", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Colombia", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("China", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Denmark", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("United Kingdom", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Argentina", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("United Arab Emirates", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("India", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Netherlands", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Ireland", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Ecuador", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Queensland", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Hong Kong", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Philippines", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Puerto Rico", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Croatia", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Lithuania", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("London", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Manhattan", new_york$host_location, ignore.case=FALSE)] <- "NY"
new_york$host_location[grepl("FR", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("downstairs", new_york$host_location, ignore.case=FALSE)] <- "NY"
new_york$host_location[grepl("AU", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("BR", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("HR", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("CZ", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("ES", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("GB", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("ES", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("KR", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Londra", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("IT", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("MX", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("NO", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Ramat HaSharon", new_york$host_location, ignore.case=FALSE)] <- NA
new_york$host_location[grepl("Harlem", new_york$host_location, ignore.case=FALSE)] <- "NY"
new_york$host_location[grepl("Tel Aviv", new_york$host_location, ignore.case=FALSE)] <- NA
new_york$host_location[grepl("Spain", new_york$host_location, ignore.case=FALSE)] <- "outside US"
new_york$host_location[grepl("Ramat Gan", new_york$host_location, ignore.case=FALSE)] <- NA
# verify all host locations were converted to abbreviations
table(as.factor(new_york$host_location))

# remove rows where there are missing values
new_york<-new_york[complete.cases(new_york),]
# convert the host location column back to a factor data type
new_york$host_location<-as.factor(new_york$host_location)

# look at all the levels in the state column
table(as.factor(new_york$state))

# fix error in state column
new_york$state[grepl("MP", new_york$state, ignore.case=FALSE)] <- NA
new_york<-droplevels(new_york)

# get only the year from the host since date
new_york$host_since<-format(as.POSIXct(new_york$host_since,format='%m/%d/%Y'),format='%Y')
# check that it correctly pulled only the year
head(new_york$host_since)
# change to integer for calculation
new_york$host_since<-as.integer(new_york$host_since)
# creat new column where the year of join is subtracted from this year
new_york$years_host<-2017- new_york$host_since
# verify changes
head(new_york$years_host)
# remove the host since column
new_york$host_since<-NULL

# divide the data into two data frames
# out_state has all the listings where the host lives out of the state
# in_state has all the listings where the host lives in the state 
out_state<-new_york[new_york$host_location != "NY",]
in_state<-new_york[new_york$host_location %in% "NY",]


# take sample of 125 random rows for in state and out of state host listings
sample_in_state<-in_state[sample(nrow(in_state), 125), ]
sample_out_state<-out_state[sample(nrow(out_state), 125), ]

# combine the two samples
sample_ny<-rbind(sample_in_state, sample_out_state)

# verify the changes were done correctly
sqldf('select count(*) from sample_ny where host_location!="NY"')

# find values of neighborhood to create location table
b<-table(as.factor(sample_ny$neighbourhood_cleansed))

write.csv(b, "locations_new_york.csv")

#*********************LOS ANGELES**********************************

# find name of file needed
dir()

# load data
la<-read.csv("los_angeles.csv")

# look at names
names(la)

# subset to only include relevant variables
la<-la[,c(1,20, 22, 23, 24, 26, 27, 29, 34, 40,43, 49, 50, 52:54, 61,68,69,75,77,80:86,90,91)]

names(la)

# change rows that are missing to NA
la$host_response_time[la$host_response_time %in% 'N/A']<-NA
la$host_response_rate[la$host_response_rate %in% 'N/A']<-NA

la<-droplevels(la)

# find missing values in each variable
apply(is.na(la),2,sum)

# remove rows where values are missing to reduce number of rows in dataset before sampling
la<-la[complete.cases(la),]

library(sqldf)
# find neigbourhoods that have the most listings
sqldf('select neighbourhood_cleansed, count(*) from la group by neighbourhood_cleansed order by count(*) desc')

# subset to only include the neighborhoods with the most listings
la<-la[la$neighbourhood_cleansed %in% c('Venice', 'Hollywood', 
                                        'Long Beach', 'Santa Monica', 'Downtown', 
                                        'West Hollywood', 'Hollywood Hills', 
                                        'Silver Lake'),]
# drop unused levels
la<-droplevels(la)

# convert the host location to the state abbreviation
la$host_location<-as.character(la$host_location)
la$host_location[grepl("New York", la$host_location, ignore.case=FALSE)] <- "NY"
la$host_location[grepl("NY", la$host_location, ignore.case=FALSE)] <- "NY"
la$host_location[grepl("new york", la$host_location, ignore.case=FALSE)] <- "NY"
la$host_location[grepl("Florida", la$host_location, ignore.case=FALSE)] <- "FL"
la$host_location[grepl("California", la$host_location, ignore.case=FALSE)] <- "CA"
la$host_location[grepl("Connecticut", la$host_location, ignore.case=FALSE)] <- "CT"
la$host_location[grepl("Maryland", la$host_location, ignore.case=FALSE)] <- "MA"
la$host_location[grepl("District of Columbia", la$host_location, ignore.case=FALSE)] <- "DC"
la$host_location[grepl("Pennsylvania", la$host_location, ignore.case=FALSE)] <- "PA"
la$host_location[grepl("Virginia", la$host_location, ignore.case=FALSE)] <- "VA"
la$host_location[grepl("Arizona", la$host_location, ignore.case=FALSE)] <- "AZ"
la$host_location[grepl("Missouri", la$host_location, ignore.case=FALSE)] <- "MO"
la$host_location[grepl("Colorado", la$host_location, ignore.case=FALSE)] <- "CO"
la$host_location[grepl("Ohio", la$host_location, ignore.case=FALSE)] <- "OH"
la$host_location[grepl("Washington", la$host_location, ignore.case=FALSE)] <- "WA"
la$host_location[grepl("Utah", la$host_location, ignore.case=FALSE)] <- "UT"
la$host_location[grepl("Massachusetts", la$host_location, ignore.case=FALSE)] <- "MA"
la$host_location[grepl("Texas", la$host_location, ignore.case=FALSE)] <- "TX"
la$host_location[grepl("Oregon", la$host_location, ignore.case=FALSE)] <- "OR"
la$host_location[grepl("Nebraska", la$host_location, ignore.case=FALSE)] <- "NE"
la$host_location[grepl("Michigan", la$host_location, ignore.case=FALSE)] <- "MI"
la$host_location[grepl("Louisiana", la$host_location, ignore.case=FALSE)] <- "LA"
la$host_location[grepl("Nevada", la$host_location, ignore.case=FALSE)] <- "NV"
la$host_location[grepl("Hawaii", la$host_location, ignore.case=FALSE)] <- "HI"
la$host_location[grepl("Iowa", la$host_location, ignore.case=FALSE)] <- "IA"
la$host_location[grepl("upstairs", la$host_location, ignore.case=FALSE)] <- "NY"
la$host_location[grepl("Georgia", la$host_location, ignore.case=FALSE)] <- "GA"
la$host_location[grepl("New Mexico", la$host_location, ignore.case=FALSE)] <- "NM"
la$host_location[grepl("Brooklyn", la$host_location, ignore.case=FALSE)] <- "NY"
la$host_location[grepl("brooklyn", la$host_location, ignore.case=FALSE)] <- "NY"
la$host_location[grepl("New Jersey", la$host_location, ignore.case=FALSE)] <- "NJ"
la$host_location[grepl("Kansas", la$host_location, ignore.case=FALSE)] <- "KS"
la$host_location[grepl("South Carolina", la$host_location, ignore.case=FALSE)] <- "SC"
la$host_location[grepl("Rhode Island", la$host_location, ignore.case=FALSE)] <- "RI"
la$host_location[grepl("Minnesota", la$host_location, ignore.case=FALSE)] <- "MN"
la$host_location[grepl("Tennessee", la$host_location, ignore.case=FALSE)] <- "TN"
la$host_location[grepl("North Dakota", la$host_location, ignore.case=FALSE)] <- "ND"
la$host_location[grepl("North Carolina", la$host_location, ignore.case=FALSE)] <- "NC"
la$host_location[grepl("Illinois", la$host_location, ignore.case=FALSE)] <- "IL"
la$host_location[grepl("Vermont", la$host_location, ignore.case=FALSE)] <- "VT"
la$host_location[grepl("Nre York", la$host_location, ignore.case=FALSE)] <- "KS"
la$host_location[grepl("United States", la$host_location, ignore.case=FALSE)] <- "US"
la$host_location[grepl("Kansas", la$host_location, ignore.case=FALSE)] <- "KS"
la$host_location[grepl("Spain", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Venezuela", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("France", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Germany", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Chile", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Japan", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Mexico", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Italy", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Canada", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Austria", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Australia", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Switzerland", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Portugal", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Brazil", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Israel", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Sweden", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Taiwan", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Costa Rica", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Russia", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Turkey", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Singapore", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Uzbekistan", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Czech Republic", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Iceland", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("harlem", la$host_location, ignore.case=FALSE)] <- "NY"
la$host_location[grepl("Uruguay", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Spain", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Montenegro", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Thailand", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Colombia", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("China", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Denmark", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("United Kingdom", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Argentina", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("United Arab Emirates", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("India", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Netherlands", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Ireland", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Ecuador", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Queensland", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Hong Kong", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Philippines", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Puerto Rico", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Croatia", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Lithuania", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("London", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Manhattan", la$host_location, ignore.case=FALSE)] <- "NY"
la$host_location[grepl("FR", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("downstairs", la$host_location, ignore.case=FALSE)] <- "NY"
la$host_location[grepl("AU", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("BR", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("HR", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("CZ", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("ES", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("GB", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("ES", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("KR", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Londra", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("IT", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("MX", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("NO", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Ramat HaSharon", la$host_location, ignore.case=FALSE)] <- NA
la$host_location[grepl("Harlem", la$host_location, ignore.case=FALSE)] <- "NY"
la$host_location[grepl("Tel Aviv", la$host_location, ignore.case=FALSE)] <- NA
la$host_location[grepl("Spain", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Ramat Gan", la$host_location, ignore.case=FALSE)] <- NA
la$host_location[grepl("Los Angeles", la$host_location, ignore.case=FALSE)] <- "CA"
la$host_location[grepl("ca", la$host_location, ignore.case=FALSE)] <- "CA"
la$host_location[grepl("CA", la$host_location, ignore.case=FALSE)] <- "CA"
la$host_location[grepl("Bulgaria", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Venice Beach", la$host_location, ignore.case=FALSE)] <- "CA"
la$host_location[grepl("Czechia", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("Indonesia", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("LAX", la$host_location, ignore.case=FALSE)] <- "CA"
la$host_location[grepl("Budapest", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("photographer", la$host_location, ignore.case=FALSE)] <- NA
la$host_location[grepl("Bicoastal", la$host_location, ignore.case=FALSE)] <- NA
la$host_location[grepl("nearby", la$host_location, ignore.case=FALSE)] <- "CA"
la$host_location[grepl("USA", la$host_location, ignore.case=FALSE)] <- "US"
la$host_location[grepl("TR", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("JP", la$host_location, ignore.case=FALSE)] <- "outside US"
la$host_location[grepl("VG", la$host_location, ignore.case=FALSE)] <- "outside US"
table(as.factor(la$host_location))

# remove all rows where there are missing values
la<-la[complete.cases(la),]
# convert host location back to a factor
la$host_location<-as.factor(la$host_location)

# look at all values in the state column
table(as.factor(la$state))
# fix errors in the state column
la$state[grepl("ca", la$state, ignore.case=FALSE)] <- "CA"
la$state[grepl("Ca", la$state, ignore.case=FALSE)] <- "CA"
la<-droplevels(la)

# convert from excel number format to a date
la$host_since<-as.Date(la$host_since, origin = "1899-12-30")
# get only the year from the host since date
la$host_since<-format(as.POSIXct(la$host_since,format='%Y/%m/%d'),format='%Y')
# check that it correctly pulled only the year
head(la$host_since)
# change to integer for calculation
la$host_since<-as.integer(la$host_since)
# creat new column where the year of join is subtracted from this year
la$years_host<-2017- la$host_since
# verify changes
head(la$years_host)
# remove the host since column
la$host_since<-NULL

# create two seperate data frames
# out_state where host lives out of the state
# in_state where host lives in the state
out_state<-la[la$host_location != "CA",]
in_state<-la[la$host_location %in% "CA",]

# take sample of 125 random rows in each data frame
sample_in_state<-in_state[sample(nrow(in_state), 125), ]
sample_out_state<-out_state[sample(nrow(out_state), 125), ]

# combine the two data frames
sample_la<-rbind(sample_in_state, sample_out_state)

# veify the changes 
sqldf('select count(*) from sample_la where host_location!="CA"')

# find values of neighborhood to create location table
b<-table(as.factor(sample_la$neighbourhood_cleansed))

write.csv(b, "locations_la.csv")

#*********************COMBINE*********************

# combine the rows from new york and la
all<-rbind(sample_ny,sample_la)
apply(is.na(all), 2, sum)

# create new column named neighbourhood_id
all$neighbourhood_id[all$neighbourhood_cleansed %in% 'Bedford-Stuyvesant']<-1
all$neighbourhood_id[all$neighbourhood_cleansed %in% 'Bushwick']<-2
all$neighbourhood_id[all$neighbourhood_cleansed %in% 'East Village']<-3
all$neighbourhood_id[all$neighbourhood_cleansed %in% 'Harlem']<-4
all$neighbourhood_id[all$neighbourhood_cleansed %in% 'Hell\'s Kitchen']<-5
all$neighbourhood_id[all$neighbourhood_cleansed %in% 'Upper East Side']<-6
all$neighbourhood_id[all$neighbourhood_cleansed %in% 'Upper West Side']<-7
all$neighbourhood_id[all$neighbourhood_cleansed %in% 'Williamsburg']<-8
all$neighbourhood_id[all$neighbourhood_cleansed %in% 'Downtown']<-9
all$neighbourhood_id[all$neighbourhood_cleansed %in% 'Hollywood']<-10
all$neighbourhood_id[all$neighbourhood_cleansed %in% 'Hollywood Hills']<-11
all$neighbourhood_id[all$neighbourhood_cleansed %in% 'Long Beach']<-12
all$neighbourhood_id[all$neighbourhood_cleansed %in% 'Santa Monica']<-13
all$neighbourhood_id[all$neighbourhood_cleansed %in% 'Silver Lake']<-14
all$neighbourhood_id[all$neighbourhood_cleansed %in% 'Venice']<-15
all$neighbourhood_id[all$neighbourhood_cleansed %in% 'West Hollywood']<-16

names(all)

# create csv files for the new tables
host<-all[,c(2:8,29,30)]
listing<-all[,c(1,2,30,11:19, 28)]
reviews<-all[,c(1,20:27)]

# remove duplicate host id
n_occur <- data.frame(table(host$host_id))
occur<-n_occur[n_occur$Freq > 1,]
occur<-occur$Var1
host <- host[ ! host$host_id %in% occur,]

# remove duplicate listing id
n_occur <- data.frame(table(all2$listing_id))
n_occur[n_occur$Freq > 1,]

write.csv(host, "host.csv")
write.csv(listing, "listing.csv")
write.csv(reviews, "reviews.csv")
