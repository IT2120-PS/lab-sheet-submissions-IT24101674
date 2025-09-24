setwd("C:\\Users\\USER\\OneDrive - Sri Lanka Institute of Information Technology\\Desktop\\SLIIT CAMPUS\\Year_2_Semester_1\\PS\\Labs\\Lab08")

data<-read.table("Data - Lab 8.txt", header = TRUE)
fix(data)
attach(data)

#Q1
#calculate population mean an variance
popmn<-mean(Nicotine)
popvar<-var(Nicotine)

#Q2
#calculate the sample mean and variance
#step 1 : create null vector to store sample data sets.
sample<-c()
n<-c()

for(i in 1:30){
  s<-sample(Nicotine, 5, replace = TRUE)
  sample<-cbind(sample,s)
  n<-c(n,paste('S',i))
}

#assign coloumn names o each sample created.
colnames(sample) = n

#calculate sample mean
s.mean<-apply(sample,2,mean)
s.mean
#calculate sample variance
s.var<-apply(sample,2,var)
s.var

#Q3
samplemean<-mean(s.mean)
samplevars<-var(s.mean)

#Q4
popmn
samplemean

#Q5
truevar=popvar/5
samplevars

#exersice
# Read as a data frame
data <- read.table("Exercise - LaptopsWeights.txt", header = TRUE)

# Extract the weight column
weights <- data$Weight


# Population statistics
pop_mean <- mean(weights)
pop_sd <- sd(weights)

cat("Population Mean:", pop_mean, "\n")
cat("Population SD:", pop_sd, "\n")

set.seed(123)  # For reproducibility

sample_means <- numeric(25)
sample_sds <- numeric(25)

for (i in 1:25) {
  sample <- sample(weights, size = 6)
  sample_means[i] <- mean(sample)
  sample_sds[i] <- sd(sample)
}

# Display sample means and SDs
data.frame(Sample = 1:25, Mean = sample_means, SD = sample_sds)

mean_of_sample_means <- mean(sample_means)
sd_of_sample_means <- sd(sample_means)

cat("Mean of Sample Means:", mean_of_sample_means, "\n")
cat("SD of Sample Means:", sd_of_sample_means, "\n")

theoretical_se <- pop_sd / sqrt(6)
cat("Theoretical Standard Error:", theoretical_se, "\n")

hist(sample_means, main = "Distribution of Sample Means", xlab = "Sample Mean", col = "skyblue", border = "white")
abline(v = pop_mean, col = "red", lwd = 2, lty = 2)

