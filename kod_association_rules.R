# Libraries
library(arules)
library(arulesViz)
library(arulesCBA)
library(dplyr)


#### WHAT TYPE OF PEOPLE LIKE LISTENING TO MUSIC ####

# Uploading the data and extracting interesting info 
df <- read.csv("responses.csv")
df[df == ""] <- NA
df_dem <- na.omit(df[, c(1, 141, 145, 147:150)])
for (i in 1:nrow(df_dem)){
  if(df_dem$Music[i] >= 4) {df_dem$Music[i] = "like music"}
  else if (df_dem$Music[i] == 3) {df_dem$Music[i] = "neutral for music"} 
  else {df_dem$Music[i] = "dislike music"}
  if(df_dem$Age[i] <= 18) {df_dem$Age[i] = "15-18"}
  else if (df_dem$Age[i] <= 22) {df_dem$Age[i] = "19-22"} 
  else if (df_dem$Age[i] <= 26) {df_dem$Age[i] = "23-26"}
  else {df_dem$Age[i] = "27-30"}
  if(df_dem$Only.child[i] == "yes") {df_dem$Only.child[i] = "only child"}
  else {df_dem$Only.child[i] = "have siblings"}
}

write.csv(df_dem, "df_dem.csv")
trans_dem <- read.transactions("df_dem.csv", format = "basket", sep = ",")

itemFrequency(trans_dem, type="relative")[which(itemFrequency(trans_dem, type="relative") > 0.00103)]

# What type of young adults like listening to music?
rules.like.music <- apriori(data=trans_dem, parameter=list(supp=0.01, conf=0.1), appearance=list(default="lhs", rhs="like music"), control=list(verbose=F)) 
rules.like.music.conf <- sort(rules.like.music, by="confidence", decreasing=TRUE)
inspect(head(rules.like.music.conf))
# Young adults who like listening to music are mostly teenagers aged 15-18, still in primary school
# and living in villages



#### Genre's likes and dislikes - data transformation ####

df_genre <- na.omit(df[, 3:19])
df_genre_like <- df_genre
df_genre_dislike <- df_genre
df_genre_like[df_genre_like <= 3] <- NA
df_genre_dislike[df_genre_dislike >= 3] <- NA


# Like

for (i in 1:nrow(df_genre_like)){
  for (j in 1:17){
    if (is.na(df_genre_like[i, j]) == FALSE) {df_genre_like[i, j] = paste(colnames(df_genre_like)[j])}
    else {df_genre_like[i, j] = ""}
  }
}

write.table(df_genre_like, file = "df_genre_like.csv", col.names = FALSE, row.names = FALSE, sep = ",")
genre_like <- read.transactions("df_genre_like.csv", sep =",", format("basket"),  rm.duplicates = TRUE)

summary(genre_like)


# Dislike

for (i in 1:nrow(df_genre_dislike)){
  for (j in 1:17){
    if (is.na(df_genre_dislike[i, j]) == FALSE) {df_genre_dislike[i, j] = paste(colnames(df_genre_dislike)[j])}
    else {df_genre_dislike[i, j] = ""}
  }
}

write.table(df_genre_dislike, file = "df_genre_dislike.csv", col.names = FALSE, row.names = FALSE, sep = ",")
genre_dislike <- read.transactions("df_genre_dislike.csv", sep =",", format("basket"),  rm.duplicates = TRUE)

summary(genre_dislike)



#### MUSIC GENRES - LIKE ####


# What are the associations between music genres people like to listen to?

itemFrequencyPlot(genre_like)

genre_like_rules <- apriori(genre_like, parameter = list(support = 0.1, confidence = 0.25, minlen = 2))
summary(genre_like_rules)

plot(genre_like_rules)
plot(genre_like_rules, method="grouped")
plot(genre_like_rules, method="graph")

inspect(sort(genre_like_rules, by = "lift")[1:5])
inspect(sort(genre_like_rules, by = "confidence")[1:5])
inspect(sort(genre_like_rules, by = "count")[1:5])
inspect(sort(genre_like_rules, by = "support")[1:5])

plot(genre_like_rules, method="paracoord", control=list(reorder=TRUE))


# What people are most likely to listen to if listen to Alternative? 

rules_alternative <- apriori(data=genre_like, parameter=list(supp=0.05,conf = 0.01), 
                       appearance=list(default="lhs", rhs="Alternative"), control=list(verbose=F)) 
rules_alternative_conf <- sort(rules_alternative, by="confidence", decreasing=TRUE)
inspect(head(rules_alternative_conf))

plot(rules_alternative, method="paracoord", control=list(reorder=TRUE))




#### MUSIC GENRES - DISLIKE ####

itemFrequencyPlot(genre_dislike)

genre_dislike_rules <- apriori(genre_dislike, parameter = list(support = 0.1, confidence = 0.4, minlen = 2))
summary(genre_dislike_rules)

plot(genre_dislike_rules)
plot(genre_dislike_rules, method="grouped")

inspect(sort(genre_dislike_rules, by = "lift")[1:5])
inspect(sort(genre_dislike_rules, by = "confidence")[1:5])
inspect(sort(genre_dislike_rules, by = "count")[1:5])
inspect(sort(genre_dislike_rules, by = "support")[1:5])

plot(genre_dislike_rules, method="paracoord", control=list(reorder=TRUE))


#### PERSONALITY TRAITS OF PEOPLE WHO LIKE LISTENING TO MUSIC ####

df_like_music <- na.omit(df[, c(1, 77:132)])

df_like_music[is.na(df_like_music) == TRUE] <- ""

for (i in 1:nrow(df_like_music)){
  for (j in 1:1){
    if (df_like_music[i, j] > 3) {df_like_music[i, j] = paste("like", colnames(df_like_music)[j])}
    else if (df_like_music[i, j] < 3) {df_like_music[i, j] = paste("dislike", colnames(df_like_music)[j])}
    else {df_like_music[i, j] = ""}
  }
  for (j in 2:32){
    if (df_like_music[i, j] > 3) {df_like_music[i, j] = paste(colnames(df_like_music)[j])}
    else if (df_like_music[i, j] < 3) {df_like_music[i, j] = paste("not", colnames(df_like_music)[j])}
    else {df_like_music[i, j] = ""}
  }
  for (j in 34:34){
    df_like_music[i, j] = paste("lying", df_like_music[i, j])
  }
  for (j in 35:57){
    if (df_like_music[i, j] > 3) {df_like_music[i, j] = paste(colnames(df_like_music)[j])}
    else if (df_like_music[i, j] < 3) {df_like_music[i, j] = paste("not", colnames(df_like_music)[j])}
    else {df_like_music[i, j] = ""}
  }
}

write.table(df_like_music, file = "df_like_music.csv", col.names = FALSE, row.names = FALSE, sep = ",")
like_music <- read.transactions("df_like_music.csv", sep =",", format("basket"),  rm.duplicates = TRUE)
summary(like_music)
itemFrequencyPlot(like_music, topN = 10)

rules_like_music_personality <- apriori(data=like_music, parameter=list(supp=0.1,conf = 0.25), 
                       appearance=list(default="lhs", rhs="like Music"), control=list(verbose=F)) 
rules_like_music_personality_conf <- sort(rules_like_music_personality, by="confidence", decreasing=TRUE)
inspect(head(rules_like_music_personality_conf))


rules_like_music_personality_rev <- apriori(data=like_music, parameter=list(supp=0.1,conf = 0.25), 
                                        appearance=list(default="rhs", lhs="like Music"), control=list(verbose=F)) 
rules_like_music_personality_rev_conf <- sort(rules_like_music_personality_rev, by="confidence", decreasing=TRUE)
inspect(head(rules_like_music_personality_rev_conf))












#### MUSIC GENRES AND PERSONALITY TRAITS ####

# df_genre_personality <- na.omit(df[, c(3:19, 77:132)])
# 
# df_genre_personality[is.na(df_genre_personality) == TRUE] <- ""
# 
# for (i in 1:nrow(df_genre_personality)){
#   for (j in 1:17){
#     if (df_genre_personality[i, j] > 3) {df_genre_personality[i, j] = paste("like", colnames(df_genre_personality)[j])}
#     else if (df_genre_personality[i, j] < 3) {df_genre_personality[i, j] = paste("dislike", colnames(df_genre_personality)[j])}
#     else {df_genre_personality[i, j] = ""}
#   }
#   for (j in 18:48){
#     if (df_genre_personality[i, j] > 3) {df_genre_personality[i, j] = paste(colnames(df_genre_personality)[j])}
#     else if (df_genre_personality[i, j] < 3) {df_genre_personality[i, j] = paste("not", colnames(df_genre_personality)[j])}
#     else {df_genre_personality[i, j] = ""}
#   }
#   for (j in 51:73){
#     if (df_genre_personality[i, j] > 3) {df_genre_personality[i, j] = paste(colnames(df_genre_personality)[j])}
#     else if (df_genre_personality[i, j] < 3) {df_genre_personality[i, j] = paste("not", colnames(df_genre_personality)[j])}
#     else {df_genre_personality[i, j] = ""}
#   }
# }
# 
# write.table(df_genre_personality, file = "df_genre_personality.csv", col.names = FALSE, row.names = FALSE, sep = ",")
# genre_personality <- read.transactions("df_genre_personality.csv", sep =",", format("basket"),  rm.duplicates = TRUE)
# 
# summary(genre_personality)
# 
# genre_personality_rules <- apriori(genre_personality, parameter = list(support = 0.15, confidence = 0.4, minlen = 2))
# summary(genre_personality_rules)
# 
# inspect(sort(genre_personality_rules, by = "lift")[1:5])
# inspect(sort(genre_personality_rules, by = "confidence")[1:5])
# inspect(sort(genre_personality_rules, by = "count")[1:5])
# inspect(sort(genre_personality_rules, by = "support")[1:5])
