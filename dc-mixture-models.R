# Mixture Models in R
# October 2018, Victor Medina

expectation <- function(data, means, proportions, sds){
  # Estimate the probabilities
  exp_data <- data %>% 
    mutate(prob_from_cluster1 = proportions[1] * dnorm(x, mean = means[1], sd = sds[1]),
           prob_from_cluster2 = proportions[2] * dnorm(x, mean = means[2], sd = sds[2]),
           prob_cluster1 = prob_from_cluster1/(prob_from_cluster1 + prob_from_cluster2),
           prob_cluster2 = prob_from_cluster2/(prob_from_cluster1 + prob_from_cluster2)) %>% 
    select(x, prob_cluster1, prob_cluster2)
  
  # Return data with probabilities
  return(exp_data)
}

maximization <- function(data_with_probs){
  
  means_estimates <- data_with_probs %>%
    summarise(mean_1 = sum(x * prob_cluster1) / sum(prob_cluster1),
              mean_2 = sum(x * prob_cluster2) / sum(prob_cluster2)) %>% 
    as.numeric()
  
  props_estimates <- data_with_probs %>% 
    summarise(proportion_1 = mean(prob_cluster1),
              proportion_2 = 1 - proportion_1) %>% 
    as.numeric()
  list(means_estimates, props_estimates)   
}

means_init <- c(0, 100)
props_init <- c(0.5, 0.5)

# Iterative process
for(i in 1:10){
  new_values <- maximization(expectation(gaussian_sample, means_init, props_init, c(10, 10)))
  means_init <- new_values[[1]]
  props_init <- new_values[[2]]
  cat(c(i, means_init, props_init), "\n")
}

gaussian_sample %>% 
  ggplot() + geom_histogram(aes(x = x, y = ..density..), bins = 200) +
  stat_function(geom = "line", fun = fun_gaussian,
                args = list(mean = means_iter10[1], proportion = props_iter10[1])) +
  stat_function(geom = "line", fun = fun_gaussian,
                args = list(mean = means_iter10[2], proportion = props_iter10[2]))


# Univariate Gaussian Mixture Models with flexmix (using EM) --------------

library(flexmix)
set.seed(1515)
fit_mix_example <- flexmix(x ~ 1, 
                           data = mix_example,  # data with one column of x and one column of assignments
                           k = 3,  # number of clusters
                           model = FLXMCnorm1(),  # univariate gaussian
                           control = list(tolerance = 1e-15, verbose = 1, iter = 1e4))

proportions <- prior(fit_mix_example)  # proportions of each cluster
comp_1 <- parameters(fit_mix_example, component = 1)  # parameters for each cluster
comp_2 <- parameters(fit_mix_example, component = 2)
comp_3 <- parameters(fit_mix_example, component = 3)

ggplot(mix_example) + geom_histogram(aes(x = x, y = ..density..)) + 
  stat_function(geom = "line", fun = fun_prop, 
                args = list(mean = comp_1[1], sd = comp_1[2], 
                            proportion = proportions[1])) +
  stat_function(geom = "line", fun = fun_prop, 
                args = list(mean = comp_2[1], sd = comp_2[2], 
                            proportion = proportions[2]))+
  stat_function(geom = "line", fun = fun_prop, 
                args = list(mean = comp_3[1], sd = comp_3[2], 
                            proportion = proportions[3]))

# Explore the first assignments
head(clusters(fit_mix_example))

# Explore the first real labels
head(mix_example$assignment)

# Create frequency table
table(mix_example$assignment, clusters(fit_mix_example))


# Bivariate Gaussian Mixture Models with flexmix --------------------------

set.seed(1313)
fit_with_covariance <- flexmix(cbind(Weight,BMI) ~ 1,
                               data = gender,
                               k = 2, 
                               model = FLXMCmvnorm(diag = FALSE),  # false includes covariances
                               control = list(tolerance = 1e-15, iter.max = 1000))

# Get the parameters
comp_1 <- parameters(fit_with_covariance, component = 1)
comp_2 <- parameters(fit_with_covariance, component = 2)

# The means
mean_comp_1 <- comp_1[1:2]
mean_comp_1
mean_comp_2 <- comp_2[1:2]
mean_comp_2

# The covariance matrices
covariance_comp_1 <- matrix(comp_1[3:6], nrow = 2)
covariance_comp_1
covariance_comp_2 <- matrix(comp_2[3:6], nrow = 2)
covariance_comp_2

# Create ellipse curve 1
library(ellipse)
ellipse_comp_1 <- ellipse(x = covariance_comp_1, 
                          centre = mean_comp_1,
                          npoints = nrow(gender))
head(ellipse_comp_1)

# Create ellipse curve 2
ellipse_comp_2 <- ellipse(x = covariance_comp_2, 
                          centre = mean_comp_2,
                          npoints = nrow(gender))
head(ellipse_comp_2)

# Plot the ellipses
gender %>% 
  ggplot(aes(x = Weight, y = BMI)) + geom_point()+
  geom_path(data = data.frame(ellipse_comp_1), aes(x=x,y=y), col = "red") +
  geom_path(data = data.frame(ellipse_comp_2), aes(x=x,y=y), col = "blue")
# Check the assignments
table(gender$Gender, clusters(fit_with_cov))


# Bernoulli Mixture Models (discrete case) --------------------------------

set.seed(1513)
# Fit Bernoulli mixture model
bernoulli_mix_model <- flexmix(digits_sample_2 ~ 1,
                               k = 3,
                               model = FLXMCmvbinary(),
                               control = list(tolerance = 1e-15, iter.max = 1000))
# Check the proportions
prior(bernoulli_mix_model)

# Extract the parameters for each cluster
param_comp_1 <- parameters(bernoulli_mix_model, component = 1)
param_comp_2 <- parameters(bernoulli_mix_model, component = 2)
param_comp_3 <- parameters(bernoulli_mix_model, component = 3)

# Visualize the clusters
show_digit(param_comp_1)
show_digit(param_comp_2)
show_digit(param_comp_3)


# Poisson Mixture Models (discrete case) ----------------------------------

# Transform into a matrix, without `community`
matrix_crimes <- crimes %>%
  select(-community) %>%
  as.matrix()

set.seed(2017)
# Fit the Poisson mixture model
poisson_mm <- stepFlexmix(matrix_crimes ~ 1, 
                          k = 1:15, 
                          nrep = 5, 
                          model = FLXMCmvpois(),
                          control = list(tolerance = 1e-15, iter.max = 1000))

# Select the model that minimize the BIC
best_poisson_mm <- getModel(poisson_mm, which = "BIC")

# Get the parameters into a data frame
params_lambdas <- data.frame(parameters(best_poisson_mm))

# Add the column with the type of crime
params_lambdas_crime <- params_lambdas %>% 
  mutate(crime = colnames(matrix_crimes))

# Plot the clusters with their lambdas
params_lambdas_crime %>% 
  gather(cluster, lambdas, -crime) %>% 
  ggplot(aes(x = crime, y = lambdas, fill = crime)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ cluster) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "none")

# Add the cluster assignments
crimes_with_clusters <- crimes %>% 
  mutate(cluster = factor(clusters(best_poisson_mm)))

# Enumerate the cluster's elements
crimes_with_clusters <- crimes_with_clusters %>% 
  group_by(cluster) %>% 
  mutate(number = row_number()) 

# Plot the clusters with the communities
crimes_with_clusters %>% 
  ggplot(aes(x = cluster, y = number, col = cluster)) + 
  geom_text(aes(label = community), size = 2.3) +
  theme(legend.position="none")