library(cluster)
library(factoextra)
library(textreuse)
library(fpc)
library(plyr)
library(lsa)


uncleaned_df <- read.csv("https://people.sc.fsu.edu/~jburkardt/datasets/hartigan/file46.txt", header = FALSE, sep = "", comment.char = "#")

# Here we clean the data frame to prepare to use it for analysis
uncleaned_df <- uncleaned_df[-c(1:4),]
 
row.names(uncleaned_df) <- uncleaned_df[,1]

cleaned_df <- uncleaned_df[,-1]
#cleaned_df <- scale(cleaned_df[,1:10])

col_names <- c('FI', 'SW', 'DA', 'NO', 'EN', 'GE', 'DU', 'FL', 'FR', 'IT', 'SP', 'PO')
colnames(cleaned_df) <- col_names

# Dendogram for Single Linkage
single_link <- hclust(dist(cleaned_df), method = "single")
fviz_dend(single_link, show_labels = TRUE, main = "Single Linkage")
"Singleton Clusters for Single-Linkage: (Great Britian, Ireland), (West Germany, Austria), (Luxemberg, Switzerland), (France, Belgium), (Denmark, Norway)"

# Dendogram for Complete Linkage
complete_link <- hclust(dist(cleaned_df), method = "complete")
fviz_dend(complete_link, show_labels = TRUE, main = "Complete Linkage")
"Singleton Clusters for Complete-Linkage: (Great Britian, Ireland), (West Germany, Austria), (Luxemberg, Switzerland), (France, Belgium), (Denmark, Norway)"


# Dendogram for Average Linkage
average_link <- hclust(dist(cleaned_df), method = "average")
most_pure = fviz_dend(average_link, show_labels = TRUE, main = "Average Linkage")
most_pure
"Singleton Clusters for Average-Linkage: (Great Britian, Ireland), (West Germany, Austria), (Luxemberg, Switzerland), (France, Belgium), (Denmark, Norway), (Portugal, Spain)"

"Italy should be clustered with the Complete Linkage strategy. In complete linkage, 
Italy is clustered with France and Belgium. All 3 of these countries tend to speak 
roughly 3 languages (where a vast majority speaking a single language) as opposed of a country like Denmark, where 
a multitude of languages are spoken by the population. Because the data set shows that Italians speak Italian with a high majority
and only have a sparse amount of other languages spoken, it makes sense that Italy
should be clustered with other similiar countries such as  Belgium and France"

"Most pure cluster is the Average Link Cluster as it has 6 2-singleton clusters"
fviz_dend(average_link, show_labels = TRUE, main = "Average Linkage", h=125)

"At a height of 125, we see 8 clusters"
# We will now re-cluster with all linkage modes with k = 8

# Dendogram for Single Linkage
single_link_k8 <- eclust(cleaned_df, "hclust", k = 8, hc_method="single")
fviz_dend(single_link_k8, show_labels = TRUE, main = "Single Linkage")
#stats_singlek8 <- cluster.stats(dist(cleaned_df), single_link_k8$cluster, silhouette = TRUE)

# Dendogram for Complete Linkage
complete_link_k8 <- eclust(cleaned_df, "hclust", k = 8, hc_method="complete")
fviz_dend(complete_link_k8, show_labels = TRUE, main = "Complete Linkage")

# Dendogram for Average Linkage
average_link_k8 <- eclust(cleaned_df, "hclust", k = 8, hc_method="average")
fviz_dend(average_link_k8, show_labels = TRUE, main = "Average Linkage")

# Here We will compute the Dunn and Silhouette for each linkage method
# Single Linkage
stats_singlek8 <- cluster.stats(dist(cleaned_df), single_link_k8$cluster, silhouette = TRUE)
stats_singlek8$avg.silwidth
stats_singlek8$dunn

# Complete Linkage
stats_completek8 <- cluster.stats(dist(cleaned_df), complete_link_k8$cluster, silhouette = TRUE)
stats_completek8$avg.silwidth
stats_completek8$dunn

# Average Linkage
stats_averagek8 <- cluster.stats(dist(cleaned_df), average_link_k8$cluster, silhouette = TRUE)
stats_averagek8$avg.silwidth
stats_averagek8$dunn

# Print the average silhouette width
stats_singlek8$avg.silwidth
stats_completek8$avg.silwidth
stats_averagek8$avg.silwidth

# Print the Dunn index
stats_singlek8$dunn
stats_completek8$dunn
stats_averagek8$dunn

"According to the Dunn index, the best cluster is the one produced by
average linkage as it has the highest dunn index value"

"According to the average silhouette metric, the best cluster is the one produced
by complete linkage as it has the highest avg silhouette value"

#-------------------------------------------------------------------------------------
#---------------------------------- LSH ----------------------------------------------
#-------------------------------------------------------------------------------------

files <- list.files("Desktop/fall2017/422/labs/lab4/corpus/", full.names = TRUE)

minhash <- minhash_generator(n = 160,seed = 100)

corpus <- TextReuseCorpus(files, tokenizer = tokenize_ngrams, n = 5,
                          minhash_func = minhash, keep_tokens = TRUE)

# This loop calculates the total number of shingles/tokens for all the documents
i <- 1
sum <- 0
while (i <= 100) {
  
  sum <- sum + length(tokens(corpus[[i]]))
  i = i + 1
  
}

#Sum holds the total number of shingles at 5-shingle tokens
sum

# Dimensions of matrix --> 22033 x 100

# Print the first 5 shingles (or tokens) of the file orig_taske.txt.
orig_taske <- tokens(corpus[["orig_taske"]])
orig_taske[0:5]

# We will fix our signatures (or hashes, or the rows in the signature matrix) at 240. This represents what
# percentage reduction in the size of the problem? 

per_red <-100 - (240 / sum * 100)

sprintf("The percent reduction is %f Precent" , (per_red))

# At 240 signatures (or hashes) we want a probability of 0.888 of getting a candidate pair in at least one band at
# a Jaccard similarity of 0.3 and above. How many bands will you need to get such a probability? 
# Using the formula provided, we need 80 BANDS

# Using the number of bands you determined in (e), run LSH and find candidate pairs. How many candidate
# pairs do you get? 

buckets <- lsh(corpus, b = 80)

lsh_probability(h = 240, b =  80, s = 0.3)
candidate_pairs <- lsh_candidates(buckets)
summary(candidate_pairs)

# here we compute the similarity scores for the candidate pairs with jaccard similarity
candidate_pairs <- lsh_compare(candidate_pairs, corpus, jaccard_similarity)
summary(candidate_pairs)

# Here we order the candidate pairs by decreasing similarity 
# The below line of code orders 
ordered_candidate_pairs_by_index <- order(candidate_pairs$score, decreasing = TRUE)

top_pairs <- candidate_pairs[ordered_candidate_pairs_by_index,]


# (g) Sort the candidate pairs according to their score field, in descending order 
#(i.e., from highest score to lowest score). List the top 5 candidates that are similar
top_pairs[1:5, ]

#-------------------------------------------------------------------------------------
#---------------------------------- Reccomendation Systems ---------------------------
#-------------------------------------------------------------------------------------

u_data <- read.csv("Desktop/fall2017/422/labs/lab4/ml-100k/u.data", sep = "", header = FALSE)
u_items <- read.csv("Desktop/fall2017/422/labs/lab4/ml-100k/u.item", sep = "|", header = FALSE)

u_data_col_names <- c("user id","item id" , "rating", "timestamp")
colnames(u_data) <- u_data_col_names

u_items_col_names <- c("movie id", "movie title", "release date", "video release date", "IMDb URL", "unknown", "Action", "Adventure", "Animation" ,"Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", "Musical", "Mystery", "Romance" ,"Sci-Fi", "Thriller", "War", "Western")
colnames(u_items)<- u_items_col_names

######## PART A ##########

### For user 200 ###

user200 <- c()
for (i in 1:100000) {
  
  if (u_data[i,1] == 200) {
    user200 = c(user200, u_data[i,2])
  }
}

movies200 <- c()

for (i in user200) {
  
  movies200 <- c(movies200, u_items[i,6:24])
}

movies.matrix <- matrix(
  data = as.numeric(movies200),
  nrow = length(user200),
  ncol = 19,
  byrow = TRUE
)
movies.matrix

genre200 <- apply(movies.matrix, 2, mean)
genre200

### For user 50 ###

user50 <- c()
for (i in 1:100000) {
  
  if (u_data[i,1] == 50) {
    user50 = c(user50, u_data[i,2])
  }
}

movies50 <- c()

for (i in user50) {
  
  movies50 <- c(movies50, u_items[i,6:24])
}

movies.matrix50 <- matrix(
  data = as.numeric(movies50),
  nrow = length(user50),
  ncol = 19,
  byrow = TRUE
)

movies.matrix50

genre50 <- apply(movies.matrix50, 2, mean)
genre50

# (i) Compute the user-user similarity of users with ID 200 and user with ID 50

cosine(genre200,genre50)

# (ii) Compute the user-item similarity of movie with ID 127 to user 200.

movie127 <- as.numeric(u_items[127, 6:24 ])

cosine(movie127, genre200)
  
# (iii) Compute the user-item similarity of movie with ID 127 to user 50.

cosine(movie127, genre50)

# (iv) Based on the above two computations, the movie 127 will be recommended to which user?
# The higher the similarity the better...Therefore movie127 will be recommended to user 50

######## PART B ##########

u_ids <- c(1,21,44,59,72,82,102,234,268,409,486)
m_ids <- c(1,2,3,4,5,6)

# Here we create the utility matrix
utility_matrix <- matrix(
  
  c(5,5,4,2,4,4,3,3,3,0,4,3,0,0,0,3,0,2,2,2,0,0,4,0,0,4,0,2,0,0,1,0,2,3,0,0,4,0,0,2,4,4,0,0,3,2,4,0,4,0,3,3,0,0,0,5,0,0,0,0,0,0,0,0,4,4),
  ncol = 11,
  nrow = 6,
  byrow = T
  
)

# generated utility matrix
utility_matrix

# Now we mean-center our utility matrix by subtracting each row by its mean

mean_centered_matrix <- matrix(
  
  ncol = 1,
  nrow = 6
  
)

for (idx in 1:6)
{
  
  mean_centered_matrix[idx,1] <- cosine(utility_matrix[5,],utility_matrix[idx,])
  
}
mean_centered_matrix

cosine(mean_centered_matrix[5, ], mean_centered_matrix[1, ])
cosine(mean_centered_matrix[5, ], mean_centered_matrix[2, ])
cosine(mean_centered_matrix[5, ], mean_centered_matrix[3, ])
cosine(mean_centered_matrix[5, ], mean_centered_matrix[4, ])
cosine(mean_centered_matrix[5, ], mean_centered_matrix[5, ])
cosine(mean_centered_matrix[5, ], mean_centered_matrix[6, ])

x <- mean_centered_matrix[2,]
y <- mean_centered_matrix[1,]
cosine(x,y)

# Top movies are 2, 3, 4

movieRating <- (
  ((cosine(mean_centered_matrix[5, ],
           mean_centered_matrix[2, ]) * 2 ) + (cosine(mean_centered_matrix[5, ], 
          mean_centered_matrix[4, ]) * 4) + (cosine(mean_centered_matrix[5, ],
          mean_centered_matrix[3, ]) * 1 ))/(cosine(mean_centered_matrix[5, ], 
          mean_centered_matrix[2, ]) + cosine(mean_centered_matrix[5, ], 
          mean_centered_matrix[4, ]) + cosine(mean_centered_matrix[5, ], 
          mean_centered_matrix[3, ])))

print(movieRating)






