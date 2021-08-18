library(readr);
library(rvest);
setwd("/Users/jaredthacker/STAT_Courses/multi_vars/project")

### === Define and initialize a vector of .html strings: === ###
### === Scrape Wikipedia tables of class wikitable.sortable === ###
htmls <- c("https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Alabama",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Arizona",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Arkansas",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Colorado",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_California", 
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Colorado",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Connecticut",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Delaware",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Florida",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Georgia",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Idaho",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Illinois",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Indiana",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Iowa",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Kansas",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Kentucky",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Louisiana",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Maine",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Maryland",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Massachusetts",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Michigan",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Minnesota",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Mississippi",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Missouri",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Montana",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Nebraska",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Nevada",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_New_Hampshire",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_New_Jersey",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_New_Mexico",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_New_York",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_North_Carolina",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_North_Dakota",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Ohio",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Oklahoma",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Oregon",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Pennsylvania",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Rhode_Island",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_South_Carolina",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_South_Dakota",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Tennessee",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Texas",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Utah",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Vermont",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Virginia",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Washington_(state)",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_West_Virginia",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Wisconsin",
           "https://en.wikipedia.org/wiki/1992_United_States_presidential_election_in_Wyoming");

### === Declare list of what will be the parsed .html tables === ###
tbls_ls <- lst();

### === Perform scrape filling the elements of tbls_ls with the  === ###
### === with parsed .html tables === ###
for (i in 1:49) {
  webpage <- read_html(htmls[i]);
  tbls_ls[[i]] <- html_nodes(webpage, ".wikitable.sortable") %>% html_table(fill=T);
}

### === Not all of the tables are uniformly strcuture, inidices of type 1 and type 2 === ###
### === make up the largest of tables that have similary structure === ###
indices_type1 <- c(1, 2, 9, 11, 13, 15, 23, 26, 33, 35, 39, 40, 42, 43, 45, 49);

indices_type2 <- c(3, 10, 14, 16, 17, 18, 19, 21, 22, 24, 27, 28, 29, 31, 34, 36, 37, 41, 44, 46,
                   48);

length(indices_type1);
length(indices_type2);
indices_type1[1];

### === Declare and initialize tbls_ls_type_1 and tbls_ls_type_2 as the lists that === ###
### === will contain the different tibbles of the wiki tables of type 1 and type 2 === ###
### === ALso, make column names for the first two types of tables that are uniform === ###
### === otherwise, we won't be able to join them properyly === ###
tbls_ls_type_1 <- tbls_ls[indices_type1];
tbls_ls_type_2 <- tbls_ls[indices_type2];
labels_type_1 <- c("county_name", "bush", "bush_perc", "clinton", "clinton_perc");
states_type_1 <- c("AL", "AZ", "FL", "ID", "IN", "KS", "MS", "NE", "ND", "OK", "SC", "SD", "TX", "UT", "VA",
                   "WY")
labels_type_2 <- c("county_name", "clinton", "clinton_perc", "bush", "bush_perc");
states_type_2 <- c("AR", "GA", "IA", "KY", "LA", "ME", "MD", "MI", "MN", "MO", "NV", "NH", "NJ", "NY", "OH",
                   "OR", "PA", "TN", "VT", "WA", "WI")
length(tbls_ls_type_1)

### === This for-loop automates the cleaning for the tables of type 1 === ###
### === Tasks to be done are in order: === ###
### === (1) drop first row === ###
### === (2) drop last row === ###
### === (3) Only include columns 1 - 5 === ###
### === (4) Rename column names === ###
### === (5) Insert a nominal state factor === ###
for (i in 1:length(tbls_ls_type_1)) {
  tbls_ls_type_1[[i]][[1]] <- tbls_ls_type_1[[i]][[1]][-1,]
  tbls_ls_type_1[[i]][[1]] <- tbls_ls_type_1[[i]][[1]][-nrow(tbls_ls_type_1[[i]][[1]]),];
  tbls_ls_type_1[[i]][[1]] <- tbls_ls_type_1[[i]][[1]][,c(1:5)];
  colnames(tbls_ls_type_1[[i]][[1]]) <- labels_type_1;
  tbls_ls_type_1[[i]][[1]]$state <- states_type_1[i];
}

### === This for-loop automates additional cleaning for tables of type 2 === ###
### === Tasks to be done are in order: == ###
### === (1) Drop columns 1 and 2 === ###
### === (2 + 3) Replace all "%" signs with empty strings === ###
### === (4) Convert percentages from type "character" to type "double" === ###
### === (5) Make a varialbe called "clinton_win" whose value indicates if === ###
### === if Clinton beat Bush === ###
for (i in 1:length(tbls_ls_type_1)) {
  tbls_ls_type_1[[i]][[1]] <- tbls_ls_type_1[[i]][[1]][,-c(2,4)]
  tbls_ls_type_1[[i]][[1]] <- tbls_ls_type_1[[i]][[1]] %>% mutate(bush_perc = str_replace_all(unlist(tbls_ls_type_1[[i]][[1]][,"bush_perc"]),
                                                                                              pattern=c("\\%" = ""))) %>%
    mutate(clinton_perc = str_replace_all(unlist(tbls_ls_type_1[[i]][[1]][,"clinton_perc"]),
                                          pattern=c("\\%" = "")))
  
  tbls_ls_type_1[[i]][[1]] <- tbls_ls_type_1[[i]][[1]] %>% mutate_at(vars(bush_perc, clinton_perc), as.double)
  tbls_ls_type_1[[i]][[1]]$clinton_win <- (tbls_ls_type_1[[i]][[1]]$clinton_perc > tbls_ls_type_1[[i]][[1]]$bush_perc)
}

### === This for-loop automates the cleaning for the tables of type 2 === ###
### === Tasks to be done are in order: === ###
### === (1) drop first row === ###
### === (2) drop last row === ###
### === (3) Only include columns 1 - 5 === ###
### === (4) Rename column names === ###
### === (5) Insert a nominal state factor === ###
for (i in 1:length(tbls_ls_type_2)) {
  tbls_ls_type_2[[i]][[1]] <- tbls_ls_type_2[[i]][[1]][-1,]
  tbls_ls_type_2[[i]][[1]] <- tbls_ls_type_2[[i]][[1]][-nrow(tbls_ls_type_2[[i]][[1]]),];
  tbls_ls_type_2[[i]][[1]] <- tbls_ls_type_2[[i]][[1]][,c(1:5)];
  colnames(tbls_ls_type_2[[i]][[1]]) <- labels_type_2;
  tbls_ls_type_2[[i]][[1]]$state <- states_type_2[i];
}

### === This for-loop automates additional cleaning for tables of type 2 === ###
### === Tasks to be done are in order: == ###
### === (1) Drop columns 1 and 2 === ###
### === (2 + 3) Replace all "%" signs with empty strings === ###
### === (4) Convert percentages from type "character" to type "double" === ###
### === (5) Make a varialbe called "clinton_win" whose value indicates if === ###
### === if Clinton beat Bush === ###
for ( i in 1:length(tbls_ls_type_2)) {
  tbls_ls_type_2[[i]][[1]] <- tbls_ls_type_2[[i]][[1]][,-c(2,4)]
  tbls_ls_type_2[[i]][[1]] <- tbls_ls_type_2[[i]][[1]] %>% mutate(bush_perc = str_replace_all(unlist(tbls_ls_type_2[[i]][[1]][,"bush_perc"]),
                                                                                              pattern=c("\\%" = ""))) %>%
    mutate(clinton_perc = str_replace_all(unlist(tbls_ls_type_2[[i]][[1]][,"clinton_perc"]),
                                          pattern=c("\\%" = "")))
  
  tbls_ls_type_2[[i]][[1]] <- tbls_ls_type_2[[i]][[1]] %>% mutate_at(vars(bush_perc, clinton_perc), as.double)
  tbls_ls_type_2[[i]][[1]]$clinton_win <- (tbls_ls_type_2[[i]][[1]]$clinton_perc > tbls_ls_type_2[[i]][[1]]$bush_perc)
}

## deal with these individually 4, 5, 6, 7, 8, 12, 20, 25, 30, 32, 38, 47

indices_type3 <- c(4, 30, 38);
states_type_3 <- c("CO", "NM", "RI");
labels_type_3 <- c("county_name", "clinton_perc", "clinton", "bush_perc", "bush")
indices_type4 <- c(5, 7, 20);
states_type_4 <- c("CA", "CT", "MA");
labels_type_4 <- c("county_name", "clinton_perc", "clinton", "bush_perc", "bush")

tbls_ls_type_3 <- tbls_ls[indices_type3]
tbls_ls_type_4 <- tbls_ls[indices_type4]

### === This for-loop automates the cleaning for the tables of type 3 === ###
### === Tasks to be done are in order: === ###
### === (1) drop first row === ###
### === (2) Only include columns 1 - 5 === ###
### === (3) Rename column names === ###
### === (4) Insert a nominal state factor === ###
for (i in 1:length(tbls_ls_type_3)) {
  tbls_ls_type_3[[i]][[1]] <- tbls_ls_type_3[[i]][[1]][-1,]
  tbls_ls_type_3[[i]][[1]] <- tbls_ls_type_3[[i]][[1]][,c(1:5)]
  colnames(tbls_ls_type_3[[i]][[1]]) <- labels_type_3;
  tbls_ls_type_3[[i]][[1]]$state <- states_type_3[i];
}

### === This for-loop automates additional cleaning for tables of type 3 === ###
### === Tasks to be done are in order: == ###
### === (1) Drop columns 3 and 5 === ###
### === (2 + 3) Replace all "%" signs with empty strings === ###
### === (4) Convert percentages from type "character" to type "double" === ###
### === (5) Make a varialbe called "clinton_win" whose value indicates if === ###
### === if Clinton beat Bush === ###
for (i in 1:length(tbls_ls_type_3)) {
  tbls_ls_type_3[[i]][[1]] <- tbls_ls_type_3[[i]][[1]][,-c(3,5)]
  tbls_ls_type_3[[i]][[1]] <- tbls_ls_type_3[[i]][[1]] %>% mutate(bush_perc = str_replace_all(unlist(tbls_ls_type_3[[i]][[1]][,"bush_perc"]),
                                                                                              pattern=c("\\%" = ""))) %>%
    mutate(clinton_perc = str_replace_all(unlist(tbls_ls_type_3[[i]][[1]][,"clinton_perc"]),
                                          pattern=c("\\%" = "")))
  
  tbls_ls_type_3[[i]][[1]] <- tbls_ls_type_3[[i]][[1]] %>% mutate_at(vars(bush_perc, clinton_perc), as.double)
  tbls_ls_type_3[[i]][[1]]$clinton_win <- (tbls_ls_type_3[[i]][[1]]$clinton_perc > tbls_ls_type_3[[i]][[1]]$bush_perc)
}

### === This for-loop automates cleaning for tables of type 4 === ###
### === Tasks to be completed are in order: === ###
### === Select only colums 1 - 5, inclusive === ###
### === Replace the column names  "county_name", "clinton_perc", "clinton", "bush_perc", "bush" === ###
### === Add a column named 'state' and add the appropriate state abbrevation === ###
for (i in 1:length(tbls_ls_type_4)) {
  tbls_ls_type_4[[i]][[1]] <- tbls_ls_type_4[[i]][[1]][,c(1:5)];
  colnames(tbls_ls_type_4[[i]][[1]]) <- labels_type_4;
  tbls_ls_type_4[[i]][[1]]$state <- states_type_4[i]
  
}

### === This for-loop automates additional cleaning for tables of type 4 === ###
### === Tasks to be completed are in order: === ###
### === Drop columns 3 - 5, inclusive === ###
### === Replace all occurrences of '%' with the empty string === ###
### === Convert the variable 'bush_perc' to type double === ###
### === Add a variable names 'clinton_win' that determines if Clinton won the county or not === ###
for (i in 1:length(tbls_ls_type_4)) {
  tbls_ls_type_4[[i]][[1]] <- tbls_ls_type_4[[i]][[1]][,-c(3,5)]
  tbls_ls_type_4[[i]][[1]] <- tbls_ls_type_4[[i]][[1]] %>% mutate(bush_perc = str_replace_all(unlist(tbls_ls_type_4[[i]][[1]][,"bush_perc"]),
                                                                                              pattern=c("\\%" = ""))) %>%
    mutate(clinton_perc = str_replace_all(unlist(tbls_ls_type_4[[i]][[1]][,"clinton_perc"]),
                                          pattern=c("\\%" = "")))
  
  tbls_ls_type_4[[i]][[1]] <- tbls_ls_type_4[[i]][[1]] %>% mutate_at(vars(bush_perc, clinton_perc), as.double)
  tbls_ls_type_4[[i]][[1]]$clinton_win <- (tbls_ls_type_4[[i]][[1]]$clinton_perc > tbls_ls_type_4[[i]][[1]]$bush_perc)
}

### === These cleaning tasks deal with Delaware, Illinois, Montana and North Carolina
### === since their .html tables were structured a bit differently than the other states === ###
### === The cleaning tasks to be done are similar in nature to the previous tasks === ###
### === and the previous for-loops can be referred to for any uncertainties === ###
delaware <- tbls_ls[[8]][[1]]
delaware <- delaware[-1,]
delaware <- delaware[,c(1:5)]
colnames(delaware) <- labels_type_2
delaware$state <- "DE";
delaware <- delaware %>% mutate(clinton_perc = str_replace_all(unlist(delaware[,"clinton_perc"]),
                                                   pattern=c("\\%" = "")))

delaware <- delaware %>% mutate(bush_perc = str_replace_all(unlist(delaware[,"bush_perc"]),
                                                               pattern=c("\\%" = "")))
delaware <- delaware %>% mutate_at(vars(bush_perc, clinton_perc), as.double);
delaware$clinton_win <- (delaware$clinton_perc > delaware$bush_perc);
delaware <- delaware[, -c(2,4)]




illinois <- tbls_ls[[12]][[3]]
illinois <- illinois[-c(1, nrow(illinois)),];
illinois <- illinois[, c(1:5)]
colnames(illinois) <- labels_type_2;
illinois$state <- "IL"
illinois <- illinois %>% mutate(bush_perc = str_replace_all(unlist(illinois[,"bush_perc"]),
                                                pattern=c("\\%" = ""))) %>%
  mutate(clinton_perc = str_replace_all(unlist(illinois[,"clinton_perc"]),
                                        pattern=c("\\%" = "")))

illinois <- illinois %>% mutate_at(vars(bush_perc, clinton_perc), as.double);
head(illinois)
illinois$clinton_win <- (illinois$clinton_perc > illinois$bush_perc)
illinois <- illinois[,-c(2,4)]


montana <- tbls_ls[[25]][[1]]
montana <- montana[which(1:nrow(montana) %% 2 == 1),]
montana <- montana[-c(1, nrow(montana)-1,nrow(montana)),]
montana <- montana[,c(1,3:4)]
colnames(montana) <- c("county_name", "clinton_perc", "bush_perc")
montana$state <- "MT";
montana <- montana %>% mutate(clinton_perc = str_replace_all(unlist(montana[,"clinton_perc"]),
                                                  pattern=c("\\%" = "")))

montana <- montana %>% mutate(bush_perc = str_replace_all(unlist(montana[,"bush_perc"]),
                                                             pattern=c("\\%" = "")))
montana <- montana %>% mutate_at(vars(bush_perc, clinton_perc), as.double);

montana$clinton_win <- (montana$clinton_perc > montana$bush_perc)

north_carolina <- tbls_ls[[32]][[1]]
north_carolina <- north_carolina[-1,]
north_carolina <- north_carolina[, c(1:5)]
colnames(north_carolina) <- c("county_name", "bush_perc", "bush", "clinton_perc", "clinton")
north_carolina <- north_carolina %>% mutate(bush_perc = str_replace_all(unlist(north_carolina[,"bush_perc"]),
                                                      pattern=c("\\%" = "")))

north_carolina <- north_carolina %>% mutate(clinton_perc = str_replace_all(unlist(north_carolina[,"clinton_perc"]),
                                                                        pattern=c("\\%" = "")))


north_carolina <- north_carolina %>% mutate_at(vars(bush_perc, clinton_perc), as.double)
head(north_carolina)

north_carolina$clinton_win <- (north_carolina$clinton_perc > north_carolina$bush_perc);
north_carolina <- north_carolina[, -c(3, 5)]

### === Join the different data for all of the states into a single dataframe === ###
data_to_join <- bind_rows(tbls_ls_type_1[[1]][[1]],
                          tbls_ls_type_1[[2]][[1]],
                          tbls_ls_type_1[[3]][[1]],
                          tbls_ls_type_1[[4]][[1]],
                          tbls_ls_type_1[[5]][[1]],                         
                          tbls_ls_type_1[[6]][[1]],                          
                          tbls_ls_type_1[[7]][[1]],                         
                          tbls_ls_type_1[[8]][[1]],                          
                          tbls_ls_type_1[[9]][[1]],                          
                          tbls_ls_type_1[[10]][[1]],                          
                          tbls_ls_type_1[[11]][[1]],                          
                          tbls_ls_type_1[[13]][[1]],                          
                          tbls_ls_type_1[[14]][[1]],                          
                          tbls_ls_type_1[[15]][[1]],                          
                          tbls_ls_type_1[[16]][[1]],
                          tbls_ls_type_2[[1]][[1]],
                          tbls_ls_type_2[[2]][[1]],
                          tbls_ls_type_2[[3]][[1]],
                          tbls_ls_type_2[[4]][[1]],
                          tbls_ls_type_2[[5]][[1]],
                          tbls_ls_type_2[[6]][[1]],
                          tbls_ls_type_2[[7]][[1]],
                          tbls_ls_type_2[[8]][[1]],
                          tbls_ls_type_2[[9]][[1]],
                          tbls_ls_type_2[[10]][[1]],
                          tbls_ls_type_2[[11]][[1]],
                          tbls_ls_type_2[[12]][[1]],
                          tbls_ls_type_2[[13]][[1]],
                          tbls_ls_type_2[[14]][[1]],
                          tbls_ls_type_2[[15]][[1]],
                          tbls_ls_type_2[[16]][[1]],
                          tbls_ls_type_2[[17]][[1]],
                          tbls_ls_type_2[[18]][[1]],
                          tbls_ls_type_2[[19]][[1]],
                          tbls_ls_type_2[[20]][[1]],
                          tbls_ls_type_2[[21]][[1]],
                          tbls_ls_type_3[[1]][[1]],
                          tbls_ls_type_3[[2]][[1]],
                          tbls_ls_type_3[[3]][[1]],
                          tbls_ls_type_4[[1]][[1]],
                          tbls_ls_type_4[[2]][[1]],
                          tbls_ls_type_4[[3]][[1]],
                          delaware,
                          illinois,
                          montana,
                          north_carolina)

### === Write the final dataframe to a .csv === ###         
write_csv(data_to_join, "web_scraped_table.csv")
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                                                    