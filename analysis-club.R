library("data.table")
library("bit64")
library("plyr")
library("dplyr")
library("ggplot2")
library("ggmap")
library("stringr")
library("maps")
library("zipcode")
library("MASS")

## ------------ data_clean ---------
# import data sets
configuration_data = fread("configuration.csv", 
                           colClasses = list(character = c("k_uuid")))
leads_data = fread("leads.csv",
                   colClasses = list(character = c("lead_id",  
                                                   "dealer_location_id", 
                                                   "dealer_zip")))
shopping_data = fread("shopping.csv")
transactions_data = fread("transactions.csv", 
                          colClasses = list(character = c("dealer_location_id", 
                                                          "zip_bought")))
visitors_data = fread("visitor.csv", colClasses = 
                        list(character = c("zip", "age_range")), integer64 = "numeric")
state_zip = fread("state_coordinates.csv")

NE = c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA", "DC")
MW = c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD")
S = c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "WV", "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX")
W = c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR", "WA")

NE_with = matrix(c(rep("NE", length(NE)), NE), ncol = 2)
MW_with = matrix(c(rep("MW", length(MW)), MW), ncol = 2)
S_with = matrix(c(rep("S", length(S)), S), ncol = 2)
W_with = matrix(c(rep("W", length(W)), W), ncol = 2)

state_region = as.data.table(rbind(NE_with, MW_with, S_with, W_with))
setnames(state_region, str_c("V", 1:2, sep = ""), c("region", "state"))

# how many configurations do each visitor make?
setkey(configuration_data, visitor_key)
setkey(transactions_data, visitor_key)
setkey(transactions_data, dma_bought)

configuration_data[, vehicle_make := toupper(vehicle_make)]

configuration_data[, .(num_config = .N), by = visitor_key]

price_data = transactions_data[, .(mean_price = mean(price_bought, na.rm = T)), 
                               by = dma_bought]
setkey(price_data, mean_price)

transactions_data[.("TULSA"), .(state_bought, price_bought, dma_bought)]

# where are transaction happening?
data(zipcode)
zipcode_data = as.data.table(zipcode)
setnames(zipcode_data, "zip", "zip_bought")
setkey(zipcode_data, zip_bought)

transaction_with_coor = merge(transactions_data, zipcode_data, 
                              by = "zip_bought", 
                              by.x = T, by.y = F)[, .(latitude, 
                                                      longitude, price_bought)]
setnames(transaction_with_coor, c("latitude", "longitude"), c("lat", "lon"))
map_country = get_googlemap(center = c(lon = -97, lat = 35), zoom = 4, scale = 2)
all_states = map_data("state")
state_maps = ggplot() + geom_polygon(data = all_states, aes(x = long, y = lat, 
                                                            group = group), 
                                     colour = "white", fill = "grey10")
ggmap(map_country, extent = "device") + 
  geom_point(data = transaction_with_coor, 
             aes(x = lon, y = lat, colour = price_bought), 
             position = "jitter", alpha = 0.2) + 
  scale_colour_gradient(low = "red")

# merge lead and transaction
setkey(transactions_data, visitor_key)
transaction_with_lead = merge(unique(transactions_data), leads_data, 
                              by = "visitor_key", by.x = T, by.y = F)
leads_num = transaction_with_lead[, .(num_leads = .N), by = visitor_key]
setkey(leads_num, num_leads)
nrow(leads_num[.(1), ]) / nrow(leads_num)

# count of road test and comparison
# comparator lt_rd_test_nmydp comp_testrev_nmydp used_cars_tmv_appraiser
test_counts = visitors_data[, .(lt_rd_test_nmydp, comp_testrev_nmydp, used_cars_tmv_appraiser)]

table(test_counts$lt_rd_test_nmydp)
numeric_column = laply(visitors_data, is.numeric)
visitors_numeric = visitors_data[, numeric_column, with = FALSE]
visitor_numeric_princomp = princomp(visitors_numeric)

# rank leads by dealer and compute their 
dealer_info = leads_data[, .(dealer_location_id, dealer_zip, dealer_distance)]
setkey(dealer_info, dealer_location_id)
dealer_leads = dealer_info[, .(num_leads = .N, 
                               mean_dist = median(dealer_distance, na.rm = T)), 
                           by = key(dealer_info)]
ggplot(dealer_leads[mean_dist < 500 & num_leads < 4000], aes(x = mean_dist, y = num_leads)) + geom_point()


# get the engine sizes
leads_data[, engine_size := str_extract(style, 
                                        "[[:digit:]](\\.)[[:digit:]]L")]
leads_data[, engine_size := as.numeric(str_replace_all(engine_size, "L", ""))]
str_match(foo_1, pattern = "([[:digit:]])(\\.)[[:digit:]]L")
leads_data[, engine_type := cut(engine_size, 
                                breaks = c(0.9, 2.0, 3.5, 5, 8.5), 
                                right = T, 
                                labels = c("<= 2.0", "<= 3.5", "<= 5", "> 5"))]
leads_data[, lead_date_r := as.Date(lead_date)]
leads_data[, lead_ymon := format(lead_date_r, format = "%Y, %m")]

engine_data = leads_data[, .(num_leads = .N, 
                        time = parse_date_time(lead_ymon, "%Y, %m")), 
                    by = .(lead_ymon, engine_type)]
engine_data[, engine_type := ordered(engine_type)]

ggplot(engine_data[!is.na(engine_type) & year(time) >= 2014, ], 
       aes(x = time, y = num_leads, fill = engine_type)) + 
  geom_bar(stat = "identity", position = "fill")

# lead and comparator
setkey(leads_data, visitor_key)
setkey(visitors_data, visitor_key)
leads_with_visitor = merge(leads_data, visitors_data, by = "visitor_key", 
                           all.x = T, all.y = F, allow.cartesian = T)
table(leads_with_visitor[, session_count])

num_leads = leads_data[, .(num_leads = .N), by = visitor_key]
nrow(visitors_data[unique(leads_with_visitor)])

# num_leads by brand
leads_data[, make := toupper(make)]
brand_names = names(sort(table(leads_data$make), decreasing = T))[1:5]
setkey(leads_data, make)
brand_sales = leads_data[, .(num_leads = .N), by = .(lead_ymon, make)]
brand_sales[, time := parse_date_time(lead_ymon, "%Y, %m")]
setkey(brand_sales, make)
ggplot(brand_sales[make %in% brand_names & year(time) >= 2014, ], 
       aes(x = time, y = num_leads, colour = make)) + geom_line() 

# num_transaction
transactions_data[, make_bought := toupper(make_bought)]
transactions_data[, date_sold := as.Date(date_sold)]
transactions_data[, ymon := format(date_sold, format = "%Y, %m")]
brand_name_transac = names(sort(table(transactions_data$make_bought), 
                                decreasing = T))[1:5]
transaction_brand = transactions_data[make_bought %in% brand_name_transac, 
                                      .(num_transac = .N), 
                                      by = .(ymon, make_bought)]
transaction_brand[, time := parse_date_time(ymon, "%Y, %m")]
ggplot(transaction_brand, aes(x = time, y = num_transac, colour = make_bought)) + geom_line()

# median price by brand
brand_price = transactions_data[make_bought %in% brand_name_transac, 
                                .(med_price = median(price_bought, na.rm = T)), 
                                by = .(ymon, make_bought)]
brand_price[, time := parse_date_time(ymon, "%Y %m")]
ggplot(brand_price, aes(x = time, y = med_price, colour = make_bought)) + geom_smooth(se = F)

# engine sizes
engine_region = leads_data[!is.na(engine_type), .(dealer_zip, engine_type, engine_size, lead_ymon)]
engine_region_zip = merge(engine_region, zipcode_data, by = "dealer_zip")
engine_region_zip[, year := year(parse_date_time(lead_ymon, "%Y, %m"))]
median_engine_zip = engine_region_zip[year == 2014, 
                                      .(med_engine = median(engine_size, na.rm = T), 
                                        latitude, longitude), by = .(dealer_zip)]
median_engine_zip[, med_engine_type := cut(med_engine, 
                                          breaks = c(0.9, 2.0, 3.5, 5, 8.5), 
                                          right = T, 
                                          labels = c("<= 2.0", "<= 3.5", 
                                                     "<= 5", "> 5"))]
ggmap(map_country, "device") + geom_point(data = median_engine_zip, 
                                mapping = aes(x = longitude, y = latitude, 
                                              colour = med_engine_type))

# leads to transaciton rates

configuration_data[, to_purchase := visitor_key %in% transactions_data$visitor_key]
leads_data[, to_purchase := visitor_key %in% transactions_data$visitor_key]

model_data = merge(leads_data, visitors_data, by = "visitor_key")
model_data[, make := toupper(make)]
index = model_data[, which(make == "")]

set(model_data, as.integer(index), 5L, NA)
training_index = sample.int(nrow(model_data), nrow(model_data) %/% 2, replace = F)
training_data = model_data[training_index, ]
testing_data = model_data[!training_index, ]

models = list()
models[[1]] = glm(to_purchase ~ price_promise_flag, binomial(), 
              training_data)
models[[2]] = update(models[[1]], formula. = . ~ . + configuration_count)
models[[3]] = update(models[[2]], . ~ . + tot_dwell_time)
models[[4]] = update(models[[3]], . ~ . + dealer_distance)


# regional differences in purchasing behaviour
# price
price_region = transactions_data[, .(zip = zip_bought[1], 
                                     med_price = median(price_bought, 
                                                        na.rm = T)), 
                                 by = state_bought]
setnames(zipcode_data, "zip_bought", "zip")
price_region_zip = merge(price_region, zipcode_data, by = "zip")
ggmap(map_country) + 
  geom_point(data = price_region_zip, 
             aes(x = longitude, y = latitude, size = med_price, 
                 label = med_price)) + 
  geom_text(data = price_region_zip, 
            aes(x = longitude, y = latitude, size = med_price, 
                label = med_price), hjust = 0.5, vjust = 1.8, colour = "red")

# used and new car
new_rate = transactions_data[, .(prop = 
                                  length(which(new_or_used_bought == "N")) / .N, 
                                 zip = zip_bought[1]), 
                             by = "state_bought"]
new_rate_zip = merge(new_rate, zipcode_data, by = "zip")
ggmap(map_country, "device") + 
  geom_point(data = new_rate_zip, aes(x = longitude, y = latitude, 
                                      size = prop, colour = prop)) + 
  geom_text(data = new_rate_zip, aes(x = longitude, y = latitude, 
                                     label = round(prop, 3)), vjust = 1.7) + 
  scale_colour_gradient(low = "red")

# discount rate
discount = transactions_data[, 
                             .(disc = median((msrp_bought - price_bought) / 
                                               msrp_bought, na.rm = T), 
                               zip = zip_bought[1]), by = state_bought]
discount_zip = merge(discount, zipcode_data, by = "zip")
ggmap(map_country, "device") + 
  geom_point(data = discount_zip, 
             aes(x = longitude, y = latitude, size = disc)) + 
  geom_text(data = discount_zip, aes(x = longitude, y = latitude, 
                                     label = round(disc, 3)), vjust = 1.7)

# merge leads and visitor
leads_visitor = merge(leads_data, visitors_data, by = "visitor_key")
body_type = ignore.case("(2 Door$)|(Wagon)|(Sedan)|(Convertible)|(SUV)|(Truck)|(Pickup)|(Coupe)|(Van)|(Hatchback)")

transactions_data[, type := toupper(str_extract(bodytype_bought, body_type))]

# type of cars
type_region = transactions_data[, .(num = .N), by = .(state_bought, type)]
ggplot(type_region[state_bought != ""], aes(x = state_bought, fill = type)) + 
  geom_bar(position = "fill")

# platform
platform_region = leads_visitor[, .(num = .N), by = .(dealer_state, first_platform_type)]
ggplot(platform_region[first_platform_type != "", ], aes(x = dealer_state, fill = first_platform_type)) + geom_bar(position = "fill")

# buyer rate
buyer_region = leads_visitor[, .(buyer_rate = length(which(to_purchase)) / .N, 
                                 zip = dealer_zip[1]), by = dealer_state]
buyer_zip = merge(buyer_region, zipcode_data, by = "zip")
ggmap(map_country, "device") + 
  geom_point(data = buyer_zip, aes(x = longitude, y = latitude, 
                                   size = buyer_rate)) + 
  geom_text(data = buyer_zip, aes(x = longitude, y = latitude, 
                                  label = round(buyer_rate, 3), vjust = 1.7))

ggplot(buyer_region, aes(x = dealer_state, y = buyer_rate)) + geom_bar(stat = "identity")

# buyer rate
buyer_click = leads_visitor[, 
                            .(buyer_rate = length(which(to_purchase)) / .N, 
                              config_count = median(as.numeric(
                                configuration_count) + nci_count, na.rm = T)), by = dealer_state]
setnames(state_zip, "state", "dealer_state")
buyer_click_zip = merge(buyer_click, state_zip, by = "dealer_state")

state_maps + 
  geom_point(data = buyer_click_zip[longitude > -140,], aes(x = longitude, 
                                         y = latitude, size = buyer_rate, 
                                         colour = config_count)) + 
  scale_colour_gradient(name = "Purchase Interest", 
                        low = "yellow", high = "red") +
  scale_size_continuous(name = "Purchase Rate", range = c(2, 7)) + 
  ggtitle("State Differences in Purchase Interest and Purchase Rate") + 
  theme(legend.position = "bottom", 
        legend.direction = "horizontal", 
        legend.background = element_blank(), 
        axis.line = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        panel.border = element_blank())
ggsave("state_map.png", width = 10, height = 6)

# look at the behaviours
impression_sum = rowSums(select(visitors_data, starts_with("imp")))
visitors_data[, impression_sum := impression_sum]
toyota_honda = rowSums(select(visitors_data, imp_toyota, imp_honda))
toyota_honda_prop = toyota_honda / impression_sum
toyota_honda_prop[is.nan(toyota_honda_prop)] = 0
toyota_honda_click = rowSums(select(visitors_data, clk_toyota, clk_honda))
visitors_data[, `:=`(th_prop = toyota_honda_prop, th_click = toyota_honda_click)]

visitor_search = merge(configuration_data, visitors_data, by = "visitor_key")

visitor_search[, shown_th := ifelse((vehicle_make != "HONDA" & 
                                       vehicle_make != "TOYOTA") & 
                                      th_prop > 0, TRUE, FALSE)]
visitor_search[, directed := ifelse(vehicle_make != "HONDA" & 
                                      vehicle_make != "TOYOTA" & 
                                      th_click > 0, TRUE, FALSE)]

# transaction visitor
transaction_visitor = merge(transactions_data, visitors_data, by = "visitor_key")
setkey(configuration_data, visitor_key)
transaction_visitor_config = merge(transaction_visitor, 
                                   unique(configuration_data), by = "visitor_key")

# sale by make
transactions_data[, make_bought := toupper(make_bought)]
setnames(state_region, "state", "state_bought")
transactions_data = merge(transactions_data, state_region, by = "state_bought")
toyota_honda_transac = transactions_data[, .(th_rate = length(which(make_bought == "HONDA" | make_bought == "TOYOTA")) / .N), by = region]

setnames(state_zip, "dealer_state", "state_bought")
toyota_honda_zip = merge(toyota_honda_transac, state_zip, by = "state_bought")
toyota_honda_zip

toyota_honda_zip[, state_bought := reorder(toyota_honda_zip$state_bought, 
                                           X = toyota_honda_zip$th_rate, 
                                           FUN = function(x) -x)]
ggplot(toyota_honda_zip, aes(x = state_bought, y = th_rate, fill = state_bought)) + 
  geom_bar(stat = "identity") + 
  theme(legend.position = "bottom", axis.ticks = element_blank(), 
        axis.text = element_blank(), axis.title.y = element_blank()) + 
  guides(fill = guide_legend(nrow = 6)) + 
  labs(x = "States", 
       title = "Proportion of Honda and Toyota Transactions
       Over States") + coord_flip()

#
state_maps + geom_point(data = toyota_honda_zip, 
                        aes(x = longitude, y = latitude, size = th_rate), 
                        colour = "orange") + 
  theme(legend.position = "bottom", axis.title = element_blank(), 
        axis.ticks = element_blank(), axis.text = element_blank()) + 
  scale_size_continuous(name = "Proportions", range = c(3, 8))

#
toyota_honda_transac[, complement := 1 - th_rate]
toyota_honda_transac_long = melt(toyota_honda_transac, 
                                 variable.name = "Type", value.name = "rate")

#
index = sample.int(nrow(leads_visitor), 
                   size = nrow(leads_visitor) %/% 2, replace = F)
training_transaction = leads_visitor[index, ]
validation_transaction = leads_visitor[!index, ]
model_train_data = training_transaction[, 
                                        .(rate = length(which(to_purchase)) / 
                                            .N, 
                                          config_count = 
                                            median(as.numeric(
                                              configuration_count), na.rm = T), 
                                          dealer_state = dealer_state[1]), 
                                        by = dealer_dma]
foo = lm(rate ~ config_count + dealer_state + , data = model_train_data)

# predict with number of configurations
lda_model = lda(to_purchase ~ configuration_count, training_transaction)

prediction_qda = predict(lda_model, newdata = validation_transaction)

# 
model_2 = lm(buyer_rate ~ poly(config_count, 2, raw = T), data = buyer_click_zip)

boot_fun = function(data, index) {
  model_result = lm(buyer_rate ~ poly(config_count, 2, raw = T), 
                    data = data, subset = index)
  return (coef(model_result))
}

boot_result = boot(buyer_click_zip, boot_fun, 10^3)

#
leads_data = merge(leads_data, state_region, by = "dealer_state")
state_engine = leads_data[, .(med_size = mean(engine_size, na.rm = T)), by = dealer_state]

ggplot(state_engine, aes(x = dealer_state, y = med_size)) + geom_bar(stat = "identity")

state_engine = merge(state_engine, state_zip, by = "dealer_state")

state_maps + geom_point(data = state_engine[longitude > -140, ], aes(x = longitude, y = latitude, size = med_size), colour = "yellow")

region_promise = leads_data[, .(promise_rate = length(which(price_promise_flag == "Y")) / .N), 
                            by = region]

#
ggplot(region_promise, aes(x = region, y = promise_rate)) + geom_bar(stat = "identity")

hybrid_keyword = ignore.case("(hybrid)|(elec)")
leads_data[, is_hybrid := str_detect(fuel, hybrid_keyword)]

# first shopping
setkey(shopping_data, visitor_key)
shopping_transaction = merge(unique(shopping_data, fromLast = T), 
                             transactions_data, by = "visitor_key")
with(shopping_transaction, length(which(make_name == make_bought)))

#
buyer_click_zip_region = merge(buyer_click_zip, state_region, by = "dealer_state", 
                               all.x = T)
model_1 = lm(buyer_rate ~ config_count + region, 
             buyer_click_zip_region[dealer_state != "DC"])
summary(model_1)

prediction_model = predict(model_1)
buyer_click_zip_region[, predictions := prediction_model]

ggplot(buyer_click_zip_region[dealer_state != "DC"], 
       aes(x = config_count, y = buyer_rate)) + 
  annotate("rect", xmin = 0, xmax = 22, ymin = 0, ymax = 0.09, fill = "antiquewhite", alpha = 0.5) + 
  annotate("rect", xmin = 22, xmax = 51, ymin = 0.09, ymax = 0.23, fill = "pink", alpha = 0.5) +
  geom_point(aes(colour = region), size = 5) + 
  stat_smooth(method = "lm", se = F, size = 2) + 
  labs(x = "Purchase Interest", y = "Purchase Rate", 
       title = "Purchase Rate vs. Purchase Interest") + 
  theme(legend.position = "bottom") + scale_colour_discrete(name = "US Regions") +
  annotate("text", x = 5, y = 0.07, label = "Low", colour = "#999999", size = 10) + 
  annotate("text", x = 43, y = 0.17, label = "High", colour = "#999999", size = 10) + 
  annotate("text", x = 8, y = 0.15, label = "ND") + 
  annotate("text", x = 17.5, y = 0.2, label = "VT")
ggsave("region_regression.png", width = 16, height = 10)
  

# by dma
dma_purchase_rate = leads_visitor[dma_name != "", .(config_count = median(as.numeric(configuration_count) + nci_count, na.rm = T), 
                                purchase_rate = length(which(to_purchase)) / .N, dealer_state = dealer_state[1]), by = dma_name]

ggplot(dma_purchase_rate[purchase_rate > 0 & config_count < 40, ], 
       aes(x = config_count, y = purchase_rate)) + 
  geom_point() + stat_smooth(method = "lm") + theme(legend.position = "bottom")

