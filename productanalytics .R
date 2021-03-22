# install.packages("readxl")
library(readxl)
library(ggplot2)
# install.packages("lubridate")
library(lubridate)
library(dplyr)

#### 1. Load Data ####
df <- read_excel(
  path="~/Downloads/Online Retail.xlsx", 
  sheet="Online Retail"
)

View(df)

# see distibution of the invoices 
ggplot(df, aes(x="", y=Quantity)) + 
  geom_boxplot(outlier.shape = NA) +
  ylim(c(-15, 25))+
  ylab("order quantity") +
  xlab("") +
  ggtitle("Quantity Distribution") +
  theme(plot.title=element_text(hjust=0.5))

# filter out orders with negative quantity (cancel orders)
df <- df[which(df$Quantity > 0),]

timeSeriesNumInvoices <- df %>%
  group_by(InvoiceDate=floor_date(InvoiceDate, "month")) %>%
  summarise(NumOrders=n_distinct(InvoiceNo))

# number of invoices
ggplot(timeSeriesNumInvoices, aes(x=InvoiceDate, y=NumOrders)) +
  geom_line() +
  ylim(c(0, max(timeSeriesNumInvoices$NumOrders) + 1000)) +
  ylab("number of orders") +
  xlab("date") +
  ggtitle("Number of Orders over Time") +
  theme(plot.title=element_text(hjust=0.5))
# 
# get rid of the drop in the last month 
summary(df[which(df$InvoiceDate >= as.Date("2011-12-01")),"InvoiceDate"])
df <- df[which(df$InvoiceDate < as.Date("2011-12-01")),]
df$Sales <- df$Quantity * df$UnitPrice

timeSeriesRevenue <- df %>%
  group_by(InvoiceDate=floor_date(InvoiceDate, "month")) %>%
  summarise(Sales=sum(Sales))


ggplot(timeSeriesRevenue, aes(x=InvoiceDate, y=Sales)) +
  geom_line() +
  ylim(c(0, max(timeSeriesRevenue$Sales) + 10000)) +
  ylab("sales") +
  xlab("date") +
  ggtitle("Revenue over Time") +
  theme(plot.title=element_text(hjust=0.5))

invoiceCustomerDF <- df %>%
  group_by(InvoiceNo, InvoiceDate) %>%
  summarise(CustomerID=max(CustomerID), Sales=sum(Sales))



