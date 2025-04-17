########################################################################################################################
#Lab 11
library(tidyverse)
library(pwr)

#######################################################
# Step 1: Conduct a Power Analysis
(pwr.t.test(d = 0.65, # large effect
           power = 0.80,
           sig.level = 0.05,
           alternative = "two.sided",
           type = "one.sample"))

#######################################################
# Step 2: Collect Source Data for Figure 2
bird.data <- read_csv("bird-data.csv") |>
  mutate(difference = closer_vals - further_vals)

# To collect the data for Figure2(g), we had to download the Excel file from the link
# and then isolate the two specific Excel sheets we wanted.
# Then, we combined the two Excel sheets and saved them to a csv
# which we then read with read_csv.
# Finally, we used mutate to create a difference column

#######################################################
#Step 3a: Summarize the Further Data


library(e1071)
bird.data|>
  summarize(mean_further       = mean(further_vals),
            sd_further         = sd(further_vals),
            median_further     = median(further_vals),
            IQR_further        = IQR(further_vals),
            skewness_further   = skewness(further_vals),
            exkurtosis_further = kurtosis(further_vals))

# Graph

ggplot(data = bird.data, aes(y = further_vals, x = "")) +
  geom_boxplot(fill = "lightblue", color = "black", alpha = 0.8, width = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +
  labs(title = "Boxplot of Further Values",
       y = "Further Values",
       x = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#######################################################
#Step 3b: Summarize the Closer Data

bird.data|>
  summarize(mean_closer       = mean(closer_vals),
            sd_closer         = sd(closer_vals),
            median_closer     = median(closer_vals),
            IQR_closer        = IQR(closer_vals),
            skewness_closer   = skewness(closer_vals),
            exkurtosis_closer = kurtosis(closer_vals))

# Graph

ggplot(data = bird.data, aes(y = closer_vals, x = "")) +
  geom_boxplot(fill = "lightblue", color = "black", alpha = 0.8, width = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +
  labs(title = "Boxplot of Closer Values",
       y = "Closer Values",
       x = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#######################################################
#Step 3b: Summarize the Difference Data

bird.data|>
  summarize(mean_difference       = mean(difference),
            sd_difference         = sd(difference),
            median_difference    = median(difference),
            IQR_difference        = IQR(difference),
            skewness_difference   = skewness(difference),
            exkurtosis_difference = kurtosis(difference))

# Graph

ggplot(data = bird.data, aes(y = difference, x = "")) +
  geom_boxplot(fill = "lightblue", color = "black", alpha = 0.8, width = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +
  labs(title = "Boxplot of Difference Values",
       y = "Difference Values",
       x = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


#######################################################
# Step 4a: Conduct Inferences Done In Paper: Closer

t_closer <- t.test(bird.data$closer_vals, mu = 0, 
        conf.level = 0.95, alternative = "greater")
t.close <- t_closer[[1]][[1]]
p.close <- t_closer[[3]]
library(effectsize)
g.close <- interpret_hedges_g(hedges_g(x = t_closer, mu = 0, alternative = "greater"))


#######################################################

# Step 4b: Conduct Inferences Done In Paper: Further
t_further <- t.test(bird.data$further_vals, mu = 0, 
        conf.level = 0.95, alternative = "less")
t.far <- t_further[[1]][[1]]
p.far <- t_further[[3]]
g.far <- interpret_hedges_g(hedges_g(x = t_further, mu = 0, alternative = "less"))


#######################################################

# Step 4c: Conduct Inferences Done In Paper: Difference
t_difference <- t.test(bird.data$difference, mu = 0, 
        conf.level = 0.95, alternative = "two.sided")
t.diff <- t_difference[[1]][[1]]
p.diff <- t_difference[[3]]
g.difference <- interpret_hedges_g(hedges_g(x = t_difference, mu = 0, alternative = "two.sided"))


#######################################################

# Step 5a: 

##########################################################
# Compute the test statistic
##########################################################
mu0 <- 0
x <- bird.data$closer_vals
(xbar <- mean(x))
(s <- sd(x))
(n <- length(x))
any(is.na(x)) # no missing data
(t.statx <- (xbar - mu0)/(s/sqrt(n)))

y <- bird.data$further_vals
(xbar <- mean(y))
(s <- sd(y))
(n <- length(y))
any(is.na(y)) # no missing data
(t.staty <- (xbar - mu0)/(s/sqrt(n)))

z <- bird.data$difference
(xbar <- mean(z))
(s <- sd(z))
(n <- length(z))
any(is.na(y)) # no missing data
(t.statz <- (xbar - mu0)/(s/sqrt(n)))





##########################################################
# Plot Closer
##########################################################
n=25

# For plotting the null distribution
ggdat.t <- tibble(t=seq(-5,5,length.out=1000))|>
  mutate(pdf.null = dt(t, df=n-1))


# For plotting the observed point
ggdat.obs <- tibble(close.t    = t.statx, 
                    far.t = t.staty,
                    diff.t = t.statz,
                    y    = 0)

# Resampling to approximate the sampling distribution 
# on the data
R <- 1000
resamples <- tibble(close.t=numeric(R))
for(i in 1:R){
  curr.sample <- sample(x=bird.data$closer_vals,
                        size=n,
                        replace=T)
  resamples$t[i] = (mean(curr.sample)-mu0)/(sd(curr.sample)/sqrt(n))
}

t.breaks <- c(-5, qt(0.025, df = n-1), # rejection region (left)
              0, 
              qt(0.975, df = n-1), 5,  # rejection region (right)
              t.statx)                  # t-statistic observed
xbar.breaks <- t.breaks * s/(sqrt(n)) + mu0

# Create Plot
ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null))+
  geom_hline(yintercept=0)+
  # rejection regions
  geom_ribbon(data=subset(ggdat.t, t<=qt(0.025, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  geom_ribbon(data=subset(ggdat.t, t>=qt(0.975, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  # plot p-value (not visible)
  geom_ribbon(data=subset(ggdat.t, t>=t.statx), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="reg", alpha=0.25)+
  # plot observation point
  geom_point(data=ggdat.obs, aes(x=close.t, y=y), color="red")+
  # Resampling Distribution
  stat_density(data=resamples, 
               aes(x=t),
               geom="line", color="grey")+
  # clean up aesthetics
  theme_bw()+
  scale_x_continuous("t",
                     breaks = round(t.breaks,2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks,
                                         labels = round(xbar.breaks,2)))+
  ylab("Density")+
  ggtitle("T-Test for Zebra Finches Closer Data",
          subtitle=bquote(H[0]==0*";"~H[a]!=0))

##########################################################
# Plot Further
##########################################################

# Resampling to approximate the sampling distribution 
# on the data
R <- 1000
resamples <- tibble(far.t=numeric(R))
for(i in 1:R){
  curr.sample <- sample(x=bird.data$further_vals,
                        size=n,
                        replace=T)
  resamples$t[i] = (mean(curr.sample)-mu0)/(sd(curr.sample)/sqrt(n))
}

t.breaks <- c(-5, qt(0.025, df = n-1), # rejection region (left)
              0, 
              qt(0.975, df = n-1), 5,  # rejection region (right)
              t.statx)                  # t-statistic observed
xbar.breaks <- t.breaks * s/(sqrt(n)) + mu0

# Create Plot
ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null))+
  geom_hline(yintercept=0)+
  # rejection regions
  geom_ribbon(data=subset(ggdat.t, t<=qt(0.025, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  geom_ribbon(data=subset(ggdat.t, t>=qt(0.975, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  # plot p-value (not visible)
  geom_ribbon(data=subset(ggdat.t, t>=t.staty), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="NA", alpha=0.25)+
  # plot observation point
  geom_point(data=ggdat.obs, aes(x=far.t, y=y), color="red")+
  # Resampling Distribution
  stat_density(data=resamples, 
               aes(x=t),
               geom="line", color="grey")+
  # clean up aesthetics
  theme_bw()+
  scale_x_continuous("t",
                     breaks = round(t.breaks,2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks,
                                         labels = round(xbar.breaks,2)))+
  ylab("Density")+
  ggtitle("T-Test for Zebra Finches Further Data",
          subtitle=bquote(H[0]==0*";"~H[a]!=0))

##########################################################
# Plot Difference
##########################################################

# Resampling to approximate the sampling distribution 
# on the data
R <- 1000
resamples <- tibble(diff.t=numeric(R))
for(i in 1:R){
  curr.sample <- sample(x=bird.data$difference,
                        size=n,
                        replace=T)
  resamples$t[i] = (mean(curr.sample)-mu0)/(sd(curr.sample)/sqrt(n))
}

t.breaks <- c(-5, qt(0.025, df = n-1), # rejection region (left)
              0, 
              qt(0.975, df = n-1), 5,  # rejection region (right)
              t.statx)                  # t-statistic observed
xbar.breaks <- t.breaks * s/(sqrt(n)) + mu0

# Create Plot
ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null))+
  geom_hline(yintercept=0)+
  # rejection regions
  geom_ribbon(data=subset(ggdat.t, t<=qt(0.025, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  geom_ribbon(data=subset(ggdat.t, t>=qt(0.975, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  # plot p-value (not visible)
  geom_ribbon(data=subset(ggdat.t, t>=t.statz), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="NA", alpha=0.25)+
  # plot observation point
  geom_point(data=ggdat.obs, aes(x=diff.t, y=y), color="red")+
  # Resampling Distribution
  stat_density(data=resamples, 
               aes(x=t),
               geom="line", color="grey")+
  # clean up aesthetics
  theme_bw()+
  scale_x_continuous("t",
                     breaks = round(t.breaks,2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks,
                                         labels = round(xbar.breaks,2)))+
  ylab("Density")+
  ggtitle("T-Test for Zebra Finches Difference Data",
          subtitle=bquote(H[0]==0*";"~H[a]!=0))

