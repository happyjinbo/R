# =================================================
# title:hw02
# description: this is a scrip about data of hw02.
# input(s): csv files about the nba players.
# output(s): text file about the nba players.
# =================================================

# ==================
# data preparation
# ==================
# read csv file
iguodala <- read.csv('../data/andre-iguodala.csv',
                           stringsAsFactors = FALSE)
green <- read.csv('../data/draymond-green.csv',
                          stringsAsFactors = FALSE)
durant <- read.csv('../data/kevin-durant.csv',
                         stringsAsFactors = FALSE)
thompson <- read.csv('../data/klay-thompson.csv',
                          stringsAsFactors = FALSE)
curry <- read.csv('../data/stephen-curry.csv',
                  stringsAsFactors = FALSE)

# ============================
# data preparation
# ==========================
# add column name
ig <- cbind(iguodala,'Andre Iguodala')
gr <- cbind(green,'Graymond Green')
dr <- cbind(durant,'Kevin Durant')
tp <- cbind(thompson,'Klay Thompson')
cr <- cbind(curry,'Stephen Curry')
colnames(ig)[14] <- 'name'
colnames(gr)[14] <- 'name'
colnames(dr)[14] <- 'name'
colnames(tp)[14] <- 'name'
colnames(cr)[14] <- 'name'


# change 'n' with 'missed shot' and 'y' with 'made shot'
ig$shot_made_flag [ig$shot_made_flag == 'n']='missed shot'
ig$shot_made_flag [ig$shot_made_flag == 'y']='made shot'

gr$shot_made_flag [gr$shot_made_flag == 'n']='missed shot'
gr$shot_made_flag [gr$shot_made_flag == 'y']='made shot'

dr$shot_made_flag [dr$shot_made_flag == 'n']='missed shot'
dr$shot_made_flag [dr$shot_made_flag == 'y']='made shot'

tp$shot_made_flag [tp$shot_made_flag == 'n']='missed shot'
tp$shot_made_flag [tp$shot_made_flag == 'y']='made shot'

cr$shot_made_flag [cr$shot_made_flag == 'n']='missed shot'
cr$shot_made_flag [cr$shot_made_flag == 'y']='made shot'

# add column minute
ig <- mutate(ig,minute = period*12 - minutes_remaining)
gr <- mutate(gr,minute = period*12 - minutes_remaining)
dr <- mutate(dr,minute = period*12 - minutes_remaining)
tp <- mutate(tp,minute = period*12 - minutes_remaining)
cr <- mutate(cr,minute = period*12 - minutes_remaining)

# sink()
sink(file = '../output/andre-iguodala-summary.txt')
summary(ig)
sink()

sink(file = '../output/graymond-green-summary.txt')
summary(gr)
sink()

sink(file = '../output/kevin-durant-summary.txt')
summary(dr)
sink()

sink(file = '../output/klay-thompson-summary.txt')
summary(tp)
sink()

sink(file = '../output/stephen-curry-summary.txt')
summary(cr)
sink()

# binding into a single data frame
bin_dat <- rbind(ig,gr,dr,tp,cr)
bin_dat$shot_made_flag [bin_dat$shot_made_flag == 'n']='missed shot'
bin_dat$shot_made_flag [bin_dat$shot_made_flag == 'y']='made shot'
write.csv(bin_dat,file = '../data/shot-data.csv')
sink(file = '../output/shot-data-summary.txt')
summary(bin_dat)
sink()






















