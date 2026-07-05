############0-Preparation#############
library(tidyverse)
library(ggplot2)
trade <- read_csv("Assignment2_Final/12100012_eng/12100012.csv")

############1-(1)Filter recent years' data and select useful variables#############
trade <- trade %>%
  select(REF_DATE, GEO, Trade, "North American Product Classification System (NAPCS)","Principal trading partners", VALUE) %>%
  rename(trade = Trade, 
         napcs = "North American Product Classification System (NAPCS)", 
         partners = "Principal trading partners", 
         value = VALUE) %>%
  separate(REF_DATE, c("year", "month"), sep = "-") %>%
  mutate(year = as.integer(year), month = as.integer(month)) %>%
  filter(year >= 2010 & year < 2018) %>%
  filter(!str_detect(napcs, "Total"), !str_detect(partners, "All"))

unique(trade$GEO)
unique(trade$trade)



############1-(2)Finding: Re-export only has data in federal level #############
province_data <- trade %>%
  filter(GEO != "Canada")
unique(province_data$GEO)
unique(province_data$trade)

canada_data <- trade %>%
  filter(GEO == "Canada")
unique(canada_data$trade)



############1-(3)Filter out the import and domestic export data in federal level#############
trade <- trade %>%
  filter(GEO != "Canada" | trade == "Re-export")
###no missing values###
is.null(trade$value)



############2.Canadian Average Net Export Value by Industry#############
###Note: Net Export = Total Export - Total Import;
###      Total Export = Domestic Export + Re-export
###Since domestic export values are the sum of 13 provinces' domestic export
###and the re-export only exists in federal level, there is not a repeated addition problem###
trade_type <- function(x){
  mutate(x, trade = case_when(trade == "Re-export" ~ "export",
                              trade == "Domestic export" ~ "export",
                              trade == "Import" ~ "import"))
}


trade_value <- function(x){
  mutate(x, value = case_when(trade == "export" ~ value, trade == "import" ~ value*(-1)))
}


plot1 <- trade_type(trade)
plot1 <- trade_value(plot1)
plot1 <- plot1 %>%
  group_by(year, napcs) %>%
  summarise(value = sum(value)) %>%
  ungroup %>%
  group_by(napcs) %>%
  summarize(mean = mean(value)/1e6) %>%
  arrange(mean) %>%
  mutate(napcs = factor(napcs, levels = napcs))


ggplot(plot1, aes(x = napcs, y = mean)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  coord_flip() + 
  labs(x = "Industry", y = "Average Annual Net Export ($billion)", caption = "Data Source: Statistics Canada") + 
  ggtitle("1.Canadian Average Annual Net Export by Industry (2010 - 2017)" ) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.caption = element_text(vjust = 1, hjust = -0.8)) 


############3.Canadian Total Export Value of Energy Products (2010 - 2017)#############
trade <- trade %>%
  filter(str_detect(napcs, "Energy")) %>%
  select(-napcs)

plot2 <- trade %>%
  filter(str_detect(trade, "export")) %>%
  group_by(year) %>%
  summarize(value = sum(value)/1e6)

ggplot(plot2, aes(x = factor(year), y = value)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  labs(x = "Year", y = "Total Export($billion)",caption = "Data Source: Statistics Canada") + 
  scale_y_continuous(limits = c(0, 150), breaks = seq(min(0), max(150), by = 30)) +
  ggtitle("2.Canadian Total Energy Export Value (2010 - 2017)" ) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.caption = element_text(hjust = -0.1)) 


############4.Find the top 5 energy export market by calculating the average net export value#############
unique(trade$partners)
partner_rank <- trade_type(trade)
partner_rank <- trade_value(partner_rank)
partner_rank <- partner_rank %>%
  mutate(partners_2 = case_when(partners %in% c("Germany", "France", "Belgium", "Italy", "Spain", "Netherlands") ~ "EU",
                                TRUE ~ partners)) %>%
  group_by(year, partners_2) %>%
  summarize(value = sum(value)) %>%
  ungroup %>%
  group_by(partners_2) %>%
  summarize(mean = mean(value)/1e6) %>%
  arrange(-mean) %>%
  slice(1:5) 

asian_four <- partner_rank %>%
  slice(2:5) %>%
  pull(partners_2)

plot3 <- partner_rank %>%
  mutate(partners_2 = factor(partners_2, levels = partners_2)) %>%
  mutate(mean = round(mean, digits = 1))

ggplot(plot3, aes(x = partners_2, y = mean, fill = partners_2, label = mean)) + 
  geom_col()+ geom_text(nudge_y = 3) +  
  labs(x = "", y = "Average Annual Net Export($billion)",fill = "", caption = "Data Source: Statistics Canada") + 
  scale_fill_manual(values = c("red","blue","green","orange","purple")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(min(0), max(100), by = 25)) +
  ggtitle("3.Average Annual Net Export of Energy Products to Top Five Partners (2010-2017)") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.caption = element_text(hjust = -0.1)) +
  theme(legend.position = "")

############5.Annual Energy Export by Partner Type#############
trade <- trade %>%
  mutate(partners_2 = case_when(partners == "United States" ~ "United States",
                                partners %in% asian_four ~ "Asia-4",
                                partners %in% c("Germany", "France", "Belgium", "Italy", "Spain", "Netherlands") ~ "EU-6",
                                TRUE ~ "Others"))

plot4 <- trade %>%
  filter(str_detect(trade, "export")) %>%
  group_by(year, partners_2) %>%
  summarize(value = sum(value)) %>%
  ungroup %>%
  group_by(year) %>%
  mutate(total = sum(value)) %>%
  mutate(percent = value/total) %>%
  mutate(percent = round(percent, digits = 2)) %>%
  mutate(percent = percent * 100)

plot_4_rank <- plot4 %>%
  filter(year==2017) %>%
  arrange(-percent) %>%
  pull(partners_2)

plot4 <- plot4 %>%
  mutate(partners_2 = factor(partners_2, levels = plot_4_rank))


ggplot(plot4, aes(x = factor(year), y = percent, fill = partners_2)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(min(0), max(100), by = 10)) +
  scale_fill_manual(values = c("red","blue","green","orange")) +
  labs(x = "Year", y = "(%)", fill = "", caption = "Data Source: Statistics Canada") + 
  ggtitle("4.The Proportion of Canada's Energy Export by Partner Type (2010-2017)") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.caption = element_text(hjust = -0.1)) 



############6.Annual Energy Export To Four Asian Countries#############
plot5 <- trade %>%
  filter(partners %in% asian_four) %>%
  filter(str_detect(trade, "export")) %>%
  group_by(year, partners) %>%
  summarize(value = sum(value)/1e6) %>%
  mutate(partners = factor(partners, levels = asian_four))

ggplot(plot5, aes(x = factor(year), y = value, fill = partners)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_continuous(limits = c(0, 2.5), breaks = seq(min(0), max(2.5), by = 0.5)) +
  scale_fill_manual(values = c("red","blue","green","orange")) +
  labs(x = "Year", y = "Energy Export Value ($billion)", fill = "", caption = "Data Source: Statistics Canada") + 
  ggtitle("5.Canadian Total Energy Export to Top Four Asian Partners (2010-2017)") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.caption = element_text(hjust = -0.1)) 


############7. Annual Growth Rate of Energy Export to Four Asian Countries#############
plot6 <- trade %>%
  filter(partners %in% asian_four) %>%
  filter(str_detect(trade, "export")) %>%
  group_by(year,partners) %>%
  summarize(value = sum(value)) %>%
  mutate(val = "val") %>%
  unite(val_year, val, year, sep = "_") %>%
  spread(val_year, value)

growth_rate <- function(x){
  mutate(x, inc2011 = ((val_2011-val_2010)/val_2010),
         inc2012 = ((val_2012-val_2011)/val_2011), 
         inc2013 = ((val_2013-val_2012)/val_2012),
         inc2014 = ((val_2014-val_2013)/val_2013),
         inc2015 = ((val_2015-val_2014)/val_2014),
         inc2016 = ((val_2016-val_2015)/val_2015),
         inc2017 = ((val_2017-val_2016)/val_2016))
}

plot6 <- growth_rate(plot6)
plot6 <- plot6 %>%
  select(partners, starts_with("inc")) %>%
  gather(year, growth_rate, starts_with("inc")) %>%
  mutate(year = str_replace(year, "inc", "")) %>%
  mutate(year = as.integer(year)) %>%
  mutate(growth_rate = growth_rate * 100)


ggplot(plot6, aes(x = factor(year), y = growth_rate)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  scale_y_continuous(limits = c(-50, 130), breaks = seq(min(-50), max(130), by = 30)) +
  facet_wrap(~ partners) +
  labs(x = "Year", y = "Growth Rate (%)", caption = "Data Source: Statistics Canada") + 
  ggtitle("6.Annual Growth Rate of Energy Export to Top Four Asian Partners (2011-2017)") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.caption = element_text(hjust = -0.1)) 


############8. Average Annual Growth Rate of Energy Export to Four Asian Countries and U.S#############
partner_rank <- partner_rank %>%
  rename(partners = partners_2)
plot7 <- semi_join(trade, partner_rank, by = "partners") %>%
  filter(str_detect(trade, "export")) %>%
  group_by(year,partners) %>%
  summarize(value = sum(value)) %>%
  mutate(val = "val") %>%
  unite(val_year, val, year, sep = "_") %>%
  spread(val_year, value)

plot7 <- growth_rate(plot7)
plot7 <- plot7 %>%
  select(partners, starts_with("inc")) %>%
  gather(year, growth_rate, starts_with("inc")) %>%
  mutate(year = str_replace(year, "inc", "")) %>%
  mutate(year = as.integer(year)) %>%
  group_by(partners) %>%
  summarize(avg_g_r = mean(growth_rate)) %>%
  arrange(avg_g_r) %>%
  mutate(partners = factor(partners, levels = partners)) %>%
  mutate(avg_g_r = avg_g_r * 100)


ggplot(plot7, aes(x = partners, y = avg_g_r, fill = partners)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("purple","orange","green","blue","red")) +
  labs(x = "", y = "Average Annual Growth Rate (%)", fill = "", caption = "Data Source: Statistics Canada") + 
  ggtitle("7.Average Annual Growth Rate of Energy Export to The US and Top Four Asian Partners (2011-2017)") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.caption = element_text(hjust = -0.05)) +
  theme(legend.position = "")


############9. Annual Growth Rate of Energy Export to Four Asian Countries and U.S#############
plot8 <- trade %>%
  filter(str_detect(trade, "export"), partners %in% asian_four | partners == "United States") %>%
  group_by(year, partners_2) %>%
  summarize(value = sum(value)) %>%
  mutate(val = "val") %>%
  unite(val_year, val, year, sep = "_") %>%
  spread(val_year, value)

plot8 <- growth_rate(plot8)

plot8 <- plot8 %>%
  select(partners_2, starts_with("inc")) %>%
  gather(year, growth_rate, starts_with("inc")) %>%
  mutate(year = str_replace(year, "inc", "")) %>%
  mutate(year = as.integer(year)) %>%
  mutate(growth_rate = growth_rate * 100)

ggplot(plot8, aes(x = factor(year), y = growth_rate, fill = partners_2)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("red","blue")) +
  labs(x = "Year", y = "Growth Rate (%)", fill = "", caption = "Data Source: Statistics Canada") + 
  ggtitle("8.The Growth Rate of Energy Export to The US and Top Four Asian Partners (2011-2017)") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.caption = element_text(hjust = -0.1)) 




############10. Find out main energy export provinces and create a graph of average annual net export value for these provinces#############
plot9 <- trade %>%
  filter(GEO != "Canada") %>%
  select(-partners_2) 

plot9 <- trade_type(plot9)

plot9 <- trade_value(plot9)

plot9 <- plot9 %>%
  group_by(year,GEO) %>%
  summarize(value = sum(value)) %>%
  ungroup %>%
  group_by(GEO) %>%
  summarize(mean = mean(value)/1e6) %>%
  arrange(-mean) %>% 
  slice(1:4) %>%
  mutate(GEO = factor(GEO, levels = GEO))


ggplot(plot9, aes(x = GEO, y = mean, fill = GEO)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("red", "blue", "green", "orange")) +
  labs(x = "", y = "Average Annual Net Export ($billion)", fill = "", caption = "Data Source: Statistics Canada") + 
  ggtitle("9.Average Annual Net Export of Energy Products From Top Four Provinces (2010-2017)") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.caption = element_text(hjust = -0.05)) +
  theme(legend.position = "")




