# Data Mining: 7142
# Author: Christopher Peters
# Teacher: Dr. Kevin McCarter

points <- c(1, 7, 2, 8, 3, 10, 4, 6, 5, 2, 6, 3, 7, 5, 8, 9, 9, 1, 10, 4)

points <- as.data.frame(matrix(points, 10, byrow = TRUE))
  names(points) <- c("x", "y")

# Mark response
points$y_target <- ifelse(points$y >= 8, 1, 0)

# Create factors
points$y_target <- factor(points$y_target, levels = c(1, 0))

# Get Chi-square values
points$chi_pvals <- 1
  
for(i in 1:9){
 points$chi_pvals[i] <- chisq.test(table(points$y_target, points$x > points$x[i]), simulate.p.value = TRUE, B = 10000)$p.value
}

# Worth
points$worth <- 1 / points$chi_pvals

# Log-worth
points$log.worth <- -log(points$chi_pvals)

# Gini
points$gini <- (points$chi_pvals^2) + (1 - points$chi_pvals)^2

# Entropy
points$entropy <- with(points, -((chi_pvals * log2(chi_pvals)) + ((1 - chi_pvals) * log2(1 - chi_pvals))))


# > points
# x  y y_target chi_pvals    worth log.worth      gini   entropy
# 1   1  7        0 1.0000000 1.000000 0.0000000 1.0000000       NaN
# 2   2  8        1 1.0000000 1.000000 0.0000000 1.0000000       NaN
# 3   3 10        1 0.1940806 5.152499 1.6394818 0.6871734 0.7099301
# 4   4  6        0 0.4982502 2.007024 0.6966530 0.5000061 0.9999912
# 5   5  2        0 1.0000000 1.000000 0.0000000 1.0000000       NaN
# 6   6  3        0 1.0000000 1.000000 0.0000000 1.0000000       NaN
# 7   7  5        0 1.0000000 1.000000 0.0000000 1.0000000       NaN
# 8   8  9        1 0.5342466 1.871795 0.6268978 0.5023457 0.9966133
# 9   9  1        0 1.0000000 1.000000 0.0000000 1.0000000       NaN
# 10 10  4        0 1.0000000 1.000000 0.0000000 1.0000000       NaN

# Worth plot
ggplot(points, aes(x = x, y = worth)) + 
  geom_line() +
  ggtitle("Worth")

# Log-Worth plot
ggplot(points, aes(x = x, y = log.worth)) + 
  geom_line() +
  ggtitle("Log-Worth")

# Worth plot
ggplot(points, aes(x = x, y = gini)) + 
  geom_line() +
  ggtitle("Gini")

# Worth plot
ggplot(points, aes(x = x, y = entropy)) + 
  geom_line() +
  ggtitle("Entropy")