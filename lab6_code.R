group_A <- c(78, 85, 82, 90, 88, 79, 81, 86, 84, 89, 83, 87)
group_B <- c(72, 75, 78, 74, 80, 71, 76, 73, 79, 77, 74, 72)

mean_A <- mean(group_A) ; sd_A <- sd(group_A)
median_A <- median(group_A)

mean_B <- mean(group_B); sd_B <- sd(group_B)
median_B <- median(group_B)
print(mean_A)
print(sd_A)
print(median_A)
print(mean_B)
print(sd_B)
print(median_B)


par(mfrow = c(1, 2))
boxplot(group_A, group_B, names = c("A", "B"),
        col = c("blue", "green"),
        main = "Оноо",
        ylab = "Шалгалтын оноо")

hist(group_A, breaks = seq(70, 92, by = 2),
     col = rgb(0, 0, 1, 0.4),
     xlim = c(70, 92),
     ylim = c(0, 5),
     main = "Оноо",
     xlab = "Шалгалтын оноо",
     ylab = "Давтамж")
hist(group_B, breaks = seq(70, 92, by = 2),
     col = rgb(0, 1, 0, 0.4),
     add = TRUE)


shapiro.test(group_A)
shapiro.test(group_B)

var.test(group_A, group_B)





ttest_result <- t.test( group_A, group_B, var.equal = TRUE)
print(ttest_result)

