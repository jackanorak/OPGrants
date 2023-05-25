library(dplyr)
library(httr)
library(jsonlite)
library(clipr)
library(scales)
library(ggplot2)

# APIKEY <- '[]'
ADDRESS <- '0x0cb199af5f402506963a4df08b11053687e09802'
OP <- '0x4200000000000000000000000000000000000042'


get_distros <- function(x,y){
  
  amounts <- list()
  
  if(missing(y)){
  for (a in x){
    da <- GET(paste0("https://api-optimistic.etherscan.io/api?module=account&action=tokentx&address=",a,
                     "&endblock=999999999&sort=asc&apikey=",
                     APIKEY)
    )
    sc <- fromJSON(rawToChar(da$content), flatten=TRUE)$result 
    
    sc <- sc %>% 
      mutate(value=as.numeric(value)/10^18, # Convert price in numeric
             timeStamp=as.POSIXct(as.numeric(timeStamp), origin="1970-01-01"),
             day = as.Date(timeStamp))  %>% 
      filter(tokenSymbol=='OP') %>%
      select(hash,day,from,to,value) %>% filter(from==a)
    
    amounts <- rbind(amounts, sc)
    
    Sys.sleep(.3)
    }
    } else{
    for (a in x){
      da <- GET(paste0("https://api-optimistic.etherscan.io/api?module=account&action=tokentx&address=",a,
                       "&endblock=999999999&sort=asc&apikey=",
                       APIKEY)
      )
      sc <- fromJSON(rawToChar(da$content), flatten=TRUE)$result 
      
      sc <- sc %>% 
        mutate(value=as.numeric(value)/10^18, # Convert price in numeric
               timeStamp=as.POSIXct(as.numeric(timeStamp), origin="1970-01-01"),
               day = as.Date(timeStamp))  %>% 
        filter(tokenSymbol=='OP') %>%  filter(day > y[which(y$to==a),]$day) %>% 
        select(hash,day,from,to,value) %>% filter(from==a)
      
      amounts <- rbind(amounts, sc)
      
      Sys.sleep(.3)
    }
  }
  return(amounts)
}

# get OP balance

OP_balance <-  GET(paste0("https://api-optimistic.etherscan.io/api?module=account&action=tokenbalance&contractaddress=",OP,
                          "&address=",ADDRESS,
                          "&tag=latest",
                          "&apikey=",
                          APIKEY))

opb <- fromJSON(rawToChar(OP_balance$content), flatten=TRUE)$result   

print(paste0("OP balance: ",as.numeric(opb)/10^18))

  
dg <- d %>% add_count(to) %>% group_by(to) %>% mutate(ttl = sum(value)) %>% ungroup() %>% 
  mutate(pct = ttl/sum(ttl)) %>% arrange(desc(ttl)) %>% mutate(cumpct = cumsum(pct)) %>% 
  mutate(share = as.character(scales::percent(pct, accuracy = 1L)),
         coins=as.character(as.integer(ttl)),
         cumshare = as.character(scales::percent(cumpct, accuracy = 1L))
         )

dg %>% select(to,coins, share, cumshare) %>% slice(1:10) %>% write_clip()


# visualize distro by days
daily <- d %>% group_by(day) %>% summarise(daily_amt = sum(value))
ggplot(daily, aes(x = day, y = daily_amt)) + geom_bar(stat='identity')

payouts <- amounts %>% select(-hash) %>%  add_count(to) %>% group_by(to) %>% 
  summarise(coins = sum(value), second_txs = mean(n), layer2 = unique(from)) %>% rename(layer3 = to) %>%  arrange(desc(coins))

pt <-  amounts %>% select(-hash)

dt <-  dg %>% select(from, to, day, value, n) %>% rename(layer1 = from, layer2 = to, layer1txs = n)

# From GPT: - just speculative

  # Use the Etherscan API to obtain information about the transactions
  # associated with the wallet in question
  transactions <- eth.getTransactionsByAddress(<wallet_address>)

# Create a data frame to store the distribution of the token among the different wallets
distribution <- data.frame(wallet=character(), amount=numeric())

# Iterate over the transactions and keep track of the distribution of the token among the different wallets
for (i in 1:nrow(transactions)) {
  tx <- transactions[i,]
  
  # Update the distribution data frame with the amount of the token transferred
  # in this transaction
  distribution <- rbind(distribution, data.frame(wallet=tx$to, amount=tx$value))
}

# Visualize the distribution of the token among the different wallets
ggplot(distribution, aes(x=wallet, y=amount)) + geom_bar(stat="identity")








## OLD CODE
{
  # df <- read.csv('~Downloads/export-token-0x4200000000000000000000000000000000000042.csv', 
  # colClasses=c('To'='character', 'From'='character','DateTime'='Date')) 
}
