library(dplyr)
library(readxl)
library(reshape2)
library(coop)
df <- read_excel(
  path="~/Downloads/Online Retail.xlsx", 
  sheet="Online Retail"
)

View(df)

# ignore cancel orders
df <- df[which(df$Quantity > 0),]

# there are 133,361 records with no CustomerID
sum(is.na(df$CustomerID))

# sneak peek at records with no CustomerID
head(df[which(is.na(df$CustomerID)),])

# remove records with NA
df <- na.omit(df)
df %>% distinct()
df %>% distinct(CustomerID, StockCode, .keep_all = TRUE)


customerItemMatrix <- dcast(
  df, CustomerID + InvoiceNo ~ StockCode, value.var="Quantity"
)


# 0-1 encode 
encode_fn <- function(x) {as.integer(x > 0)}

customerItemMatrix <- customerItemMatrix %>% 
  mutate_at(vars(-CustomerID), funs(encode_fn))


# User-to-User Similarity Matrix
userToUserSimMatrix <- cosine(
  as.matrix(
    # excluding CustomerID column
    t(customerItemMatrix[, 2:dim(customerItemMatrix)[2]])
  )
)
colnames(userToUserSimMatrix) <- customerItemMatrix$CustomerID

top10SimilarCustomersTo12350 <- customerItemMatrix$CustomerID[
  order(userToUserSimMatrix[,"12350"], decreasing = TRUE)[1:11]
]



itemsBoughtByA <- customerItemMatrix[
  which(customerItemMatrix$CustomerID == "12350"),
]

itemsBoughtByA <- colnames(customerItemMatrix)[which(itemsBoughtByA != 0)]