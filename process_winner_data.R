misc_data <- read_table('http://users.stat.ufl.edu/~winner/data/clinton1.dat', col_names=F)
desc <- read_lines('http://users.stat.ufl.edu/~winner/data/clinton1.txt', skip=9)
names <- str_split_fixed(desc, pattern="\\s\\W", n=2)
names <- names[,1] 
names <- str_replace_all(names, pattern=c("\\s" = "_"))
names <- str_to_lower(names)
colnames(misc_data) <- names
write_csv(misc_data, 'winner_dataset.csv', col_names=T)
