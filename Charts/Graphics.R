#Graphics
format = "%d/%m/%Y"

date = as.Date(date_serie, format)

theme_set(theme_bw() + theme(legend.position = "top"))

ggplot(data =  as.data.frame(option_price_1),  aes(x = date, y = option_price_1)) + 
  geom_line(color = "steelblue") +
  labs(title = "K6 Option value over time",
       x = "Date",
       y = "Price") +
  scale_y_continuous(breaks = seq(0, 1.6, by = 0.2)) +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold")) +
  theme(axis.title.x = element_text(hjust = 0.5, size = 12, face = "bold")) +
  theme(axis.title.y = element_text(hjust = 0.5, size = 12, face = "bold"))

delta_hedge_1_graphe <- ggplot(as.data.frame(pf_DeltaReplication), aes(x = date)) + 
  geom_line(aes(y = pf_DeltaReplication, colour = "red")) + 
  geom_line(aes(y = option_price_1, colour = "blue")) +
  scale_color_discrete(name = "Legend", labels = c("Delta-hedged pf", "Option price")) +
  scale_y_continuous(breaks = seq(0, 1.6, 0.2)) +
  labs(title = "Delta Hedging of the K6 Option with constant volatility") + 
  xlab("") + ylab("Value") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold")) +
  theme(axis.title.x = element_text(hjust = 0.5, size = 12, face = "bold")) +
  theme(axis.title.y = element_text(hjust = 0.5, size = 12, face = "bold")) 

delta_hedge_1_graphe <- ggplot(as.data.frame(pf_DeltaReplication), aes(x = date, y = pf_DeltaReplication)) + geom_line(color = "black") + 
  labs(title = "Delta Hedging de Opt K6, sigma = 20 %") + xlab("") + ylab("Value") +
  geom_line(aes(y = option_price_1), color="steelblue")

#Saving and exporting the plots
ggexport(delta_hedge_1_graphe,deltaGamma.hedge_1_graphe, filename = "C:\\Users\\benja\\Downloads\\PartieI.pdf",
         nrow = 2, ncol = 1)
