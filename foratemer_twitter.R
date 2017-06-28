##Twitter: Fora Temer

library(rtweet)
library(ggplot2)
library(ggthemes)
library(dplyr)

api_key <- "key"
api_secret <- "secret"
access_token <- "token"
access_token_secret <- "token_secret"
setup_twitter_oauth(api_key,api_secret)


ft1 <- search_tweets("fora temer", n = 18000, include_rts = FALSE)
ft2 <- search_tweets("#ForaTemer", n = 18000, include_rts = FALSE)

ft1$text_plain <- plain_tweets(ft1$text)
ft2$text_plain <- plain_tweets(ft2$text)


agg1 <- ts_filter(ft1, by = "30 mins")
agg2 <- ts_filter(ft2, by = "30 mins")

agg3 <- inner_join(agg2, agg1, by="time")
agg3$foratemer<-agg3$freq.x+agg3$freq.y

agg3$dia<-as.numeric(substr(as.character(agg3$time),9,10))

agg3<-agg3 %>%
  filter(dia>22)

g1<-ggplot(agg3, aes(time, foratemer, color="red")) + geom_line(size=1.3) + xlab("") + ylab("") +  
  theme_fivethirtyeight() + theme(legend.position="none") 
g1 + facet_grid(. ~ agg3$dia, scales="free", space="free")

ggsave(filename="fora_temer.png", last_plot(), dpi=1000)


