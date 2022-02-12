library(dplyr);
library(readr);
library(keras);
library(caret);
library(fastDummies);
library(class);
library(ROCR);
library(magrittr)

setwd("/Users/jaredthacker/STAT_Courses/multi_vars/project")
full_dataset <- read_csv("full_dataset.csv", col_names = T)

clinton <- full_dataset %>% filter(clinton_win == TRUE)
bush <- full_dataset %>% filter(clinton_win == FALSE)

### --- EDA --- ###
for_clint_by_region <- full_dataset %>% dplyr::group_by(region) %>%
  dplyr::summarise(mean_for_clint=mean(percent_for_clinton));

ggplot(for_clint_by_region, aes(x=region, y=mean_for_clint)) +
  scale_y_continuous(breaks=xticks, labels=xticks) +
  geom_bar(stat="identity", position="dodge", fill="orange") +
  geom_text(aes(label= paste(format(mean_for_clint, digits=2, nsmall=2), "%", sep="")), vjust=-0.5,
            position = position_dodge(0.9)) +
  scale_fill_manual(values=c("red", "blue")) +
  theme_minimal() +
  labs(x=NULL, y=NULL, title="Mean Percentage for Clinton by Region and Color") +
  theme(plot.title = element_text(hjust=0.5))

for_clint_by_state <- full_dataset %>% dplyr::group_by(state, color) %>%
  dplyr::summarise(mean_for_clint=mean(percent_for_clinton));

ggplot(for_clint_by_state, aes(x=state, y=mean_for_clint, fill=color)) +
  scale_fill_manual(values=c("red", "blue")) +
  geom_bar(stat="identity") + 
  theme_minimal() + 
  labs(title="Vote Percentage for Clinton by State",
       y="Percentage for Clinton",
       x="State") +
  theme(plot.title = element_text(hjust=0.5)) +
  coord_flip()

yticks <- pretty(full_dataset$percent_veterans);
yticks_format <- paste(yticks, "%", sep="");
bp1 <- ggplot(full_dataset, aes(y=percent_veterans, x=clinton_win, fill=clinton_win)) +
  scale_y_continuous(breaks=yticks, labels=yticks_format) +
  scale_x_discrete(labels=NULL) +
  scale_fill_manual(values=c("red","blue")) +
  geom_boxplot() +
  labs(x=NULL, y="Percentage for Veterans", title="Percent Veterans by Clinton Win") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

bp2 <- ggplot(full_dataset, aes(y=crime_index_PC, x=clinton_win, fill=clinton_win)) +
  scale_x_discrete(labels=NULL) +
  scale_fill_manual(values=c("red","blue")) +
  geom_boxplot() +
  labs(x=NULL, y="Crime Index (Per Capita)", title="Crime Index by Clinton Win") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

bp3 <- ggplot(full_dataset, aes(y=median_age, x=clinton_win, fill=clinton_win)) +
  scale_x_discrete(labels=NULL) +
  scale_fill_manual(values=c("red","blue")) +
  geom_boxplot() +
  labs(x=NULL, y="Median Age", title="Median Age by Clinton Win") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

yticks <- pretty(unlist(full_dataset[,"mean_savings"]));
yticks_format <- format(yticks, big.mark = ",", scientific=F)
bp4 <- ggplot(full_dataset, aes(y=mean_savings, x=clinton_win, fill=clinton_win)) +
  scale_y_continuous(breaks=yticks, labels=yticks_format) +
  scale_x_discrete(labels=NULL) +
  scale_fill_manual(values=c("red","blue")) +
  geom_boxplot() +
  labs(x=NULL, y="Mean Savings ($)", title="Mean Savings ($) by Clinton Win") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

bp5 <- ggplot(full_dataset, aes(y=percent_in_poverty, x=clinton_win, fill=clinton_win)) +
  scale_x_discrete(labels=NULL) +
  scale_fill_manual(values=c("red","blue")) +
  geom_boxplot() +
  labs(x=NULL, y="Percent In Poverty (%)", title="Percent In Poverty (%) by Clinton Win") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

yticks <- pretty(full_dataset$percent_female, n=3);
yticks_format <- paste(yticks, "%", sep="")
bp6 <- ggplot(full_dataset, aes(y=percent_female, x=clinton_win, fill=clinton_win)) +
  scale_y_continuous(breaks=yticks, labels=yticks_format) +
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(breaks=yticks, labels=yticks_format) +
  scale_fill_manual(values=c("red","blue")) +
  geom_boxplot() +
  labs(x=NULL, y="Percent Female (%)", title="Percent Female (%) by Clinton Win") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

yticks <- pretty(full_dataset$per_capita_income);
yticks_format <- format(yticks, big.mark=",", scientific=T)
bp7 <- ggplot(full_dataset, aes(y=per_capita_income, x=clinton_win, fill=clinton_win)) +
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(breaks=yticks, labels=yticks_format) +
  scale_fill_manual(values=c("red","blue")) +
  geom_boxplot() +
  labs(x=NULL, y="Per Capita Income ($)", title="Per Capita Income ($) by Clinton Win") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

yticks <- pretty(full_dataset$population_density);
yticks_format <- format(yticks, big.mark=",", scientific=F)
bp8 <- ggplot(full_dataset, aes(y=population_density, x=clinton_win, fill=clinton_win)) +
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(breaks=yticks, labels=yticks_format, limits=c(0, 500)) +
  scale_fill_manual(values=c("red","blue")) +
  geom_boxplot() +
  labs(x=NULL, y="Population Density", title="Population Density by Clinton Win") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))


bp1 + bp2 + bp3 + bp4 + bp5 + bp6 + bp7 + guide_area() + plot_layout(ncol=2, guides="collect")

ggplot(full_dataset, aes(x=percent_in_poverty, y=percent_female, color=clinton_win)) +
  scale_color_manual(values=c("red", "blue")) +
  geom_point(alpha=1/5) +
  labs(x="Percent in Poverty (%)", y="Percent Female (%)", title="Percent Female vs. Percent in Poverty",
       color="Clinton Victory") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5))

### --- Prepare for MANOVA and test for assumptions --- ###
 qq1 <- ggplot(full_dataset, aes(sample=sqrt(crime_index_PC), color=clinton_win)) +
   stat_qq() + stat_qq_line() +
   scale_color_manual(values=c("red", "blue")) +
   labs(x=NULL, y=NULL, title=expression(paste("QQ-Plot of ", sqrt("Crime Index Per Capita"), sep=" "))) +
   theme_classic() +
   theme(plot.title = element_text(hjust=0.5))

 qq2 <- ggplot(full_dataset, aes(sample=median_age, color=clinton_win)) +
   stat_qq() + stat_qq_line() +
   scale_color_manual(values=c("red", "blue")) +
   labs(x=NULL, y=NULL, title=expression(paste("QQ-Plot of ", "Median Age", sep=" "))) +
   theme_classic() +
   theme(plot.title = element_text(hjust=0.5))
 
 qq3 <- ggplot(full_dataset, aes(sample=log(mean_savings), color=clinton_win)) +
   stat_qq() + stat_qq_line() +
   scale_color_manual(values=c("red", "blue")) +
   labs(x=NULL, y=NULL, title=expression(paste("QQ-Plot of ", log("Mean Savings"), sep=" "))) +
   theme_classic() +
   theme(plot.title = element_text(hjust=0.5))
 
 qq4 <- ggplot(full_dataset, aes(sample=log(per_capita_income), color=clinton_win)) +
   stat_qq() + stat_qq_line() +
   scale_color_manual(values=c("red", "blue")) +
   labs(x=NULL, y=NULL, title=expression(paste("QQ-Plot of ", log("Per Capita Income"), sep=" "))) +
   theme_classic() +
   theme(plot.title = element_text(hjust=0.5))
 
 qq5 <- ggplot(full_dataset, aes(sample=log(percent_in_poverty), color=clinton_win)) +
   stat_qq() + stat_qq_line() +
   scale_color_manual(values=c("red", "blue")) +
   labs(x=NULL, y=NULL, title=expression(paste("QQ-Plot of ", log("Percent In Poverty"), sep=" "))) +
   theme_classic() +
   theme(plot.title = element_text(hjust=0.5))
 
 qq6 <- ggplot(full_dataset, aes(sample=percent_veterans, color=clinton_win)) +
   stat_qq() + stat_qq_line() +
   scale_color_manual(values=c("red", "blue")) +
   labs(x=NULL, y=NULL, title=expression(paste("QQ-Plot of ", sqrt("Crime Index Per Capita"), sep=" "))) +
   theme_classic() +
   theme(plot.title = element_text(hjust=0.5))
 
 qq7 <- ggplot(full_dataset, aes(sample=percent_female, color=clinton_win)) +
   stat_qq() + stat_qq_line() +
   scale_color_manual(values=c("red", "blue")) +
   labs(x=NULL, y=NULL, title=expression(paste("QQ-Plot of ", "Percent Female", sep=" "))) +
   theme_classic() +
   theme(plot.title = element_text(hjust=0.5))
 
 qq8 <- ggplot(full_dataset, aes(sample=log(population_density), color=clinton_win)) +
   stat_qq() + stat_qq_line() +
   scale_color_manual(values=c("red", "blue")) +
   labs(x=NULL, y=NULL, title=expression(paste("QQ-Plot of ", log("Population Density"), sep=" "))) +
   theme_classic() +
   theme(plot.title = element_text(hjust=0.5))
 
 qq9 <- ggplot(full_dataset, aes(sample=log(percent_in_nursing_homes), color=clinton_win)) +
   stat_qq() + stat_qq_line() +
   scale_color_manual(values=c("red", "blue")) +
 labs(x=NULL, y=NULL, title=expression(paste("QQ-Plot of ", log("Percent In Nursing Homes"), sep=" "))) +
   theme_classic() +
   theme(plot.title = element_text(hjust=0.5))
 
 qq1 + qq2 + qq3 + qq4 + qq5 + qq6 + qq7 + qq8 + qq9 + guide_area() + plot_layout(ncol=2, guides="collect");

 
 qq1 <- ggplot(full_dataset, aes(sample=crime_index_PC, color=clinton_win)) +
   stat_qq() + stat_qq_line() +
   scale_color_manual(values=c("red", "blue")) +
   labs(x=NULL, y=NULL, title=expression(paste("QQ-Plot of ", "Crime Index Per Capita", sep=" "))) +
   theme_classic() +
   theme(plot.title = element_text(hjust=0.5))
 
 qq2 <- ggplot(full_dataset, aes(sample=median_age, color=clinton_win)) +
   stat_qq() + stat_qq_line() +
   scale_color_manual(values=c("red", "blue")) +
   labs(x=NULL, y=NULL, title=expression(paste("QQ-Plot of ", "Median Age", sep=" "))) +
   theme_classic() +
   theme(plot.title = element_text(hjust=0.5))
 
 qq3 <- ggplot(full_dataset, aes(sample=mean_savings, color=clinton_win)) +
   stat_qq() + stat_qq_line() +
   scale_color_manual(values=c("red", "blue")) +
   labs(x=NULL, y=NULL, title=expression(paste("QQ-Plot of ", "Mean Savings", sep=" "))) +
   theme_classic() +
   theme(plot.title = element_text(hjust=0.5))
 
 qq4 <- ggplot(full_dataset, aes(sample=per_capita_income, color=clinton_win)) +
   stat_qq() + stat_qq_line() +
   scale_color_manual(values=c("red", "blue")) +
   labs(x=NULL, y=NULL, title=expression(paste("QQ-Plot of ", "Per Capita Income", sep=" "))) +
   theme_classic() +
   theme(plot.title = element_text(hjust=0.5))
 
 qq5 <- ggplot(full_dataset, aes(sample=percent_in_poverty, color=clinton_win)) +
   stat_qq() + stat_qq_line() +
   scale_color_manual(values=c("red", "blue")) +
   labs(x=NULL, y=NULL, title=expression(paste("QQ-Plot of ", "Percent In Poverty", sep=" "))) +
   theme_classic() +
   theme(plot.title = element_text(hjust=0.5))
 
 qq6 <- ggplot(full_dataset, aes(sample=percent_veterans, color=clinton_win)) +
   stat_qq() + stat_qq_line() +
   scale_color_manual(values=c("red", "blue")) +
   labs(x=NULL, y=NULL, title=expression(paste("QQ-Plot of ", "Crime Index Per Capita", sep=" "))) +
   theme_classic() +
   theme(plot.title = element_text(hjust=0.5))
 
 qq7 <- ggplot(full_dataset, aes(sample=percent_female, color=clinton_win)) +
   stat_qq() + stat_qq_line() +
   scale_color_manual(values=c("red", "blue")) +
   labs(x=NULL, y=NULL, title=expression(paste("QQ-Plot of ", "Percent Female", sep=" "))) +
   theme_classic() +
   theme(plot.title = element_text(hjust=0.5))
 
 qq8 <- ggplot(full_dataset, aes(sample=population_density, color=clinton_win)) +
   stat_qq() + stat_qq_line() +
   scale_color_manual(values=c("red", "blue")) +
   labs(x=NULL, y=NULL, title=expression(paste("QQ-Plot of ", "Population Density", sep=" "))) +
   theme_classic() +
   theme(plot.title = element_text(hjust=0.5))
 
 qq9 <- ggplot(full_dataset, aes(sample=percent_in_nursing_homes, color=clinton_win)) +
   stat_qq() + stat_qq_line() +
   scale_color_manual(values=c("red", "blue")) +
   labs(x=NULL, y=NULL, title=expression(paste("QQ-Plot of ", "Percent In Nursing Homes", sep=" "))) +
   theme_classic() +
   theme(plot.title = element_text(hjust=0.5))
 
 qq1 + qq2 + qq3 + qq4 + qq5 + qq6 + qq7 + qq8 + qq9 + guide_area() + plot_layout(ncol=2, guides="collect");
 
 ggpairs(full_dataset[,-c(1,2, 12:17)])
 
 xyticks <- colnames(full_dataset[,-c(1:2, 12:17)])
 xyticks_parts <- str_split_fixed(xyticks, pattern="_", n=4)
 xyticks_formatted <- str_replace_all(str_to_title(str_trim(paste(xyticks_parts[,1],
                                     xyticks_parts[,2],
                                     xyticks_parts[,3],
                                     xyticks_parts[,4],
                                     sep=" "))),
                                     pattern=c("Pc"="PC"));
 cor.full <- cor(full_dataset[,-c(1:2, 12:17)])
 cor.full.melt <- melt(cor.full) 
 ggplot(cor.full.melt, aes(x=Var1, y=Var2, fill=value)) +
   scale_x_discrete(breaks=xyticks, labels=xyticks_formatted) +
   scale_y_discrete(breaks=xyticks, labels=xyticks_formatted) +
   scale_fill_gradient2(low="blue", mid="white", high="red", limits=c(-1,1)) +
   labs(x=NULL, y=NULL, title="Correlation Heatmap for the Whole Dataset") +
   geom_tile() +
   geom_text(aes(label=round(value, digits=2))) +
     theme_minimal() +
     theme(plot.title = element_text(hjust=0.5),
           axis.text.x = element_text(angle=90))
 
 
full_dataset_clinton_win <- full_dataset %>% filter(clinton_win == TRUE);
full_dataset_clinton_lose <- full_dataset %>% filter(clinton_win == FALSE); 
colnames(full_dataset_clinton_win) <- paste(colnames(full_dataset_clinton_win),
                                            "_win", sep=""); 
colnames(full_dataset_clinton_lose) <- paste(colnames(full_dataset_clinton_lose),
                                             "_lose", sep="");
clinton_win_clinton_lost <- bind_cols(full_dataset_clinton_win,
                                      full_dataset_clinton_lose[1:nrow(full_dataset_clinton_win),]);
 
cor.between.sample <- cor(clinton_win_clinton_lost[,-c(1,12:14,17, 18, 29:31, 34)]);
cor.between.sample.melt <- melt(cor.between.sample)

xyticks <- colnames(cor.between.sample);
xyticks_parts <- str_split_fixed(xyticks, n=5, patter="_");
xyticks_format <- str_replace_all(str_to_title(str_trim(paste(xyticks_parts[,1], xyticks_parts[,2],
                        xyticks_parts[,3], xyticks_parts[,4],
                        xyticks_parts[,5], sep=" "))), pattern=c("Pc" = "PC"))
ggplot(cor.between.sample.melt, aes(x=Var1, y=Var2, fill=value)) +
  scale_x_discrete(breaks=xyticks, labels=xyticks_format) +
  scale_y_discrete(breaks=xyticks, labels=xyticks_format) +
  scale_fill_gradient2(low="blue", mid="white", high="red", limits=c(-1, 1)) +
  geom_tile() +
  geom_label(x="bush_perc_lose", y="bush_perc_lose", label = "I", fill="green") +
  geom_label(x="median_age_win", y="bush_perc_lose", label = "II", fill="green") +
  geom_label(x="median_age_win", y="median_age_win", label = "III", fill="green") +
  geom_label(x="bush_perc_lose", y="median_age_win", label = "IV", fill="green") +
  labs(x=NULL, y=NULL, title="Correlation Heatmap Between Subpopulations",
       fill="Correlation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(angle=90))
 
res.man <- as_tibble(residuals(man.full.data))
obs.tibble <- tibble(observation_num=1:nrow(res.man))
residuals.obs <- dplyr::bind_cols(obs.tibble, res.man)

xticks <- pretty(residuals.obs$observation_num, 6)
xticks_format <- format(xticks, big.mark = ",", scientific=F);

ggplot(residuals.obs, aes(x=observation_num, y=median_age)) +
  scale_x_continuous(breaks=xticks, labels=xticks_format) +
  geom_point(alpha=1/2) +
  geom_hline(yintercept=0.0, lty="longdash", color="orange", size=2) +
  labs(x="Observation Number", y="Median Age",
       title="Residual Plot for Median Age") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))
  
ggplot(residuals.obs, aes(x=observation_num, y=percent_female)) +
  scale_x_continuous(breaks=xticks, labels=xticks_format) +
  geom_point(alpha=1/2) +
  geom_hline(yintercept=0.0, lty="longdash", color="orange", size=2) +
  labs(x="Observation Number", y="Median Age",
       title="Residual Plot for Percent Female") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))

man.full.data <- manova(cbind(median_age, mean_savings, per_capita_income, percent_in_poverty, percent_veterans,
                              percent_female, population_density, percent_in_nursing_homes, crime_index_PC) ~ as.factor(clinton_win),
                        data=full_dataset)
summary(man.full.data, test="Wilks")
summary.aov(man.full.data)

yticks <- pretty(full_dataset$percent_female, n=3);
yticks_format <- paste(yticks, "%", sep="")
bp6 <- ggplot(full_dataset, aes(y=percent_female, x=clinton_win, fill=clinton_win)) +
  scale_y_continuous(breaks=yticks, labels=yticks_format) +
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(breaks=yticks, labels=yticks_format) +
  scale_fill_manual(values=c("red","blue")) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=20, size=8, color="green", fill="black") +
  labs(x=NULL, y="Percent Female (%)", title="Percent Female (%) by Clinton Win") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

yticks <- pretty(full_dataset$per_capita_income);
yticks_format <- format(yticks, big.mark=",", scientific=T)
bp7 <- ggplot(full_dataset, aes(y=per_capita_income, x=clinton_win, fill=clinton_win)) +
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(breaks=yticks, labels=yticks_format) +
  scale_fill_manual(values=c("red","blue")) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=20, size=8, color="green", fill="black") +
  labs(x=NULL, y="Per Capita Income ($)", title="Per Capita Income ($) by Clinton Win") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

yticks <- pretty(0:100, by=4);
yticks_format <- format(yticks, big.mark=",", scientific=F)
bp8 <- ggplot(full_dataset, aes(y=population_density, x=clinton_win, fill=clinton_win)) +
  scale_x_discrete(labels=NULL) +
  scale_y_continuous(breaks=yticks, labels=yticks_format, limits=c(0, 100)) +
  scale_fill_manual(values=c("red","blue")) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=20, size=8, color="green", fill="black") +
  labs(x=NULL, y="Population Density", title="Population Density by Clinton Win") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5))

bp6 + bp7 + bp8 + guide_area() + plot_layout(ncol=2, guides="collect")
### --- Simultaneous Confidence Intervals --- ###
   cov.full.dataset <- cov(full_dataset[,-c(1:2,12:14,17)]);
   vars.full.dataset <- diag(cov.full.dataset);
   alpha <- 0.05;
   p <- 11;
   n <- nrow(full_dataset);
   crit.value <- qchisq(1-alpha,p);
   full.dataset.means <- full_dataset[,-c(1:2,12:14,17)] %>% summarise_all(mean)
   variable_names <- colnames(full.dataset.means);
   variable_names_parts <- str_split_fixed(variable_names, n=5, pattern="_");
   variable_names_format <- str_replace_all(str_to_title(str_trim(paste(variable_names_parts[,1], variable_names_parts[,2],
                                  variable_names_parts[,3], variable_names_parts[,4],
                                  variable_names_parts[,5]))), pattern=c("Pc" = "PC"))
   
   lower <- c();
   upper <- c();
   for (i in 1:p) {
     lower[i] <- full.dataset.means[i] - sqrt(crit.value) * sqrt(vars.full.dataset[i] / n);
     upper[i] <- full.dataset.means[i] + sqrt(crit.value) * sqrt(vars.full.dataset[i] / n);
     print(paste("The confidence interval for ", variable_names_format[i], "is:"))
     cat(paste("\t(", format(lower[i], digits=2, nsmall=2, big.mark=","), ", " , format(upper[i], digits=2, nsmall=2, big.mark=","), ")\n\n", sep=""))
   }   
   

### --- Principal Components Analysis --- ###
   predictors.full.dataset <- full_dataset %>% select(-c(county_name, state, color, region, percent_for_clinton,
                                                       bush_perc, clinton_perc, clinton_win))
   labels <- full_dataset %>% select(clinton_win)

   pca.cor <- princomp(predictors.full.dataset, cor=T);

  n_data <- nrow(pca.cor$scores)
   pca.eigs <- pca.cor$sdev^2
   pca.tot <- sum(pca.cor$sdev^2)
   pca.cumsum <- cumsum(pca.eigs)
   pca.var.explained <- pca.cumsum / pca.tot
   crit.vals.ci <- qnorm(mean=0, sd=1, p=1-alpha/2);
   moe.lower <- 1 + crit.vals.ci * sqrt(2 / n);
   moe.upper <- 1 - crit.vals.ci * sqrt(2/ n);
   pca.stats <- tibble(n=1:length(pca.eigs),
                       eigenvalues=pca.eigs,
                       variance_explained=pca.var.explained,
                       moe.lb = moe.lower,
                       moe.up = moe.upper);

   xticks <- pretty(pca.stats$n,4);
   ggplot(pca.stats, aes(x=n, y=eigenvalues, ymin=eigenvalues / moe.lb, ymax=eigenvalues / moe.up)) +
     scale_x_continuous(breaks=xticks, labels=xticks) +
     geom_vline(xintercept=4, linetype="longdash", color="green", size=2) +
     geom_label(x=3.5, y=0.8, label="Elbow", fill="beige") +
     geom_point() +
     geom_pointrange() +
     geom_line() +
     labs(x="Component #", y="Eigenvalue", title="Screeplot for PCA with Confidence Intervals") +
     theme_classic() +
     theme(plot.title=element_text(hjust=0.5))

   ggplot(pca.stats, aes(x=n, y=variance_explained)) +
     scale_x_continuous(breaks=xticks, labels=xticks) +
     geom_hline(yintercept=0.6, lty="longdash", color="green", size=2) +
     geom_hline(yintercept = 0.8, lty="longdash", color="magenta", size=2) +
     geom_hline(yintercept=0.7, lty="longdash", size=2, color="cyan") +
     geom_label(x=6, y=0.58, label="60% Explained", fill="green") +
     geom_label(x=6, y=0.68, label="70% Explained", fill="cyan") +
     geom_label(x=6, y=0.78, label="80% Explained", fill="magenta") +
     geom_label(x=3, y=0.74, label="74% Explained", fill="orange") +
     annotate(geom="point", x=4, y=0.7382881, color="orange", size=4) +
     geom_point() +
     geom_line() +
     labs(x="# of Components", y="Variance Explained",
          title="Cumulative Variance vs. # of Components") +
     theme_classic() +
     theme(plot.title=element_text(hjust=0.5))

     pca.cor$loadings   
     y.score <- pca.cor[["scores"]][which(pca.cor[["scores"]][,2] < -20),2]
     x.score <- pca.cor[["scores"]][which(pca.cor[["scores"]][,2] < -20),1]
     full_dataset[which(pca.cor[["scores"]][,2] < -20),"county_name"]
     full_dataset[which(pca.cor[["scores"]][,2] < -20),"state"]
     
     pca.scores <- as_tibble(pca.cor[["scores"]]);
     clint_win.labels <- tibble(clinton_win=full_dataset$clinton_win);
     pca.scores.clinton.win <- bind_cols(clint_win.labels, pca.scores)
     ggplot(pca.scores.clinton.win, aes(x=Comp.1, y=Comp.2, color=clinton_win)) +
       scale_color_manual(values=c("red", "blue")) +
       annotate(geom = "point", x=x.score, y=y.score, size=5, color="green") +
       geom_point() +
       labs(x="Component 1", y="Component 2",
            title="Score Plot for Components 1 and 2") +
       theme_classic() +
       theme(plot.title=element_text(hjust=0.5))
### --- NN with Keras --- ###
     ### -- Prepare the data for the NN --- ###
     labels <- fastDummies::dummy_cols(labels, remove_first_dummy = T);
     dataset.NN <- bind_cols(predictors.full.dataset, labels[,"clinton_win_TRUE"])   
     train.indices <- caret::createDataPartition(dataset.NN$clinton_win_TRUE, p=0.7, list=F);   
     train.set <- dataset.NN[train.indices,];
     test.set <- dataset.NN[-train.indices,];

     train.set.predictors <- train.set %>% select(-clinton_win_TRUE) %>% scale();     
     labels.train <- keras::to_categorical(train.set$clinton_win_TRUE);

      test.set.predictors <- test.set %>% select(-clinton_win_TRUE) %>% scale();
     labels.test <-to_categorical(test.set$clinton_win_TRUE);
     
     dnn.model <- keras_model_sequential();
     
     dnn.model %>%
       layer_dense(units=32, activation="relu", input_shape=ncol(train.set.predictors)) %>%
       layer_dropout(rate=0.1) %>%
       layer_dense(units=64, activation = "relu") %>%
       layer_dropout(rate=0.1) %>%
       layer_dense(units=128, activation = "relu") %>%
       layer_dropout(rate=0.1) %>%
       layer_dense(units=64, activation = "relu") %>%
       layer_dropout(rate=0.1) %>%
       layer_dense(units=32, activation = "relu") %>%
       layer_dropout(rate=0.1) %>%
       layer_dense(units=2, activation='sigmoid')

     history <- dnn.model %>% compile(
       loss = "binary_crossentropy",
       optimizer = "RMSprop",
       metrics = c("accuracy")
     );

    dnn.model %>% fit(
      train.set.predictors, labels.train,
      epochs = 100,
      batch_size = 5,
      validation_split=0.2
    )     
     
    dnn.model.mets <- dnn.model %>% evaluate(test.set.predictors, labels.test)     
    dnn.model.acc <- dnn.model.mets$acc
    
    
    predictions <- dnn.model %>% predict_classes(test.set.predictors);
    table(factor(predictions, levels=min(test.set$clinton_win_TRUE):max(test.set$clinton_win_TRUE)),
                 factor((test.set$clinton_win_TRUE), levels=min(test.set$clinton_win_TRUE):max(test.set$clinton_win_TRUE)))     
     
     
### --- KNN model --- ###
   knn.mod <- class::knn(train=train.set, test=test.set, cl=train.set$clinton_win_TRUE, k=5);
   knn.acc <- round(mean(knn.mod != test.set$clinton_win_TRUE), digits=2)
   knn.err <- 1 - knn.acc;
   
### --- Random Forest Model --- ###
   i <- 49
   set.seed(i)
   train.indices.rf <- caret::createDataPartition(full_dataset$clinton_win, p=0.8, list=F);
   train.set.rf <- full_dataset[train.indices.rf,c(3:11, 17)]
   test.set.rf <- full_dataset[-train.indices.rf,c(3:11, 17)]
   train.set.rf$clinton_win <- factor(as.character(train.set.rf$clinton_win));
   test.set.rf$clinton_win <- factor(as.character(test.set.rf$clinton_win));
   contrasts(test.set.rf$clinton_win);
   contrasts(train.set.rf$clinton_win);
   set.seed(i)
   rf_clinton <- randomForest::randomForest(clinton_win ~ ., data=train.set.rf, ntree=150,
                                            mtry=1, importance=T);
     rf_clinton
   preds <- predict(rf_clinton, test.set.rf[,-10])     
   conf.matrix <- table(observed=test.set.rf$clinton_win, predicted=preds)
   accuracy.rf <- sum(diag(conf.matrix)) / sum(conf.matrix)
   error.rate.rf <- 1 - accuracy.rf;
   
  #accuracy
   
  preds.roc <- predict(rf_clinton, newdata=test.set.rf[,-10], type="prob")
  forest.pred <- ROCR::prediction(preds.roc[,2], test.set.rf$clinton_win);
  forest.perf <- ROCR::performance(forest.pred, "tpr", "fpr");
  plot(forest.perf, main="ROC Curve for the Random Forest", colorize=F);
  lines(x=seq(0, 1, by=0.01), y=seq(0, 1, by=0.01), lty=2, col="red")
  classes <- levels(test.set.rf$clinton_win)
  true.values <- ifelse(test.set.rf[,10] == classes[1], 1, 0);
  pred.auc <- ROCR::prediction(preds.roc[,1], true.values)
  auc <- ROCR::performance(pred.auc, measure="auc")
  auc@y.values
  ytick <- colnames(train.set.rf)[-10]
  label <-c("Median Age", "Percent In Nursing Homes", "Crime Index PC", "Mean Savings",
             "Percent Veterans", "Percent Female", "PC Income", "Percent In Poverty",
             "Population Density")
  randomForest::varImpPlot(rf_clinton, type=2, main="Variable Importance Plot from Random Forest")
  axis(side=2, at=ytick, labels = label)
  
  acc.tibble <- tibble(classifier=c("Deep-Learning Neural Network", "KNN", "Random Forest"),
         acc=c(100 - dnn.model.acc * 100, 100 - knn.acc * 100, 100 - accuracy.rf * 100))
  
  ### --- Plot the performance of all the classifier models --- ###
  ggplot(acc.tibble, aes(x=classifier, y=acc)) +
    geom_bar(stat="identity", fill="orange") +
    geom_text(aes(label=paste(format(acc, digits=2, nsmall=2), "%", sep=""),
                  vjust=-1.0)) +
    labs(x=NULL, y="ER (%)", title="Model ER vs. Classifier Type") +
    theme_minimal() +
    theme(plot.title=element_text(hjust=0.5))
   
  