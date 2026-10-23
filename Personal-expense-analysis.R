# ==========================================
# MINI PROJECT: PERSONAL EXPENSE ANALYSIS
# ==========================================

# EXPERIMENT 1: Create Expense Dataset

expenses <- data.frame(
  Expense_ID = 1:20,
  Category = c(
    "Food", "Travel", "Shopping", "Bills", "Food",
    "Travel", "Entertainment", "Bills", "Shopping", "Food",
    "Travel", "Food", "Bills", "Entertainment", "Shopping",
    "Food", "Travel", "Bills", "Entertainment", "Shopping"
  ),
  Amount = c(
    150, 80, 500, 1200, 200,
    100, 300, 800, 700, 250,
    120, 180, 900, 350, 450,
    220, 150, 600, 250, 550
  )
)

cat("===== PERSONAL EXPENSE DATASET =====\n")
print(expenses)

# Arithmetic Operations
expenses$Tax <- expenses$Amount * 0.05
expenses$Total_Amount <- expenses$Amount + expenses$Tax

cat("\nTotal Expenses:", sum(expenses$Amount), "\n")
cat("Average Expense:", round(mean(expenses$Amount), 2), "\n")
cat("Highest Expense:", max(expenses$Amount), "\n")
cat("Lowest Expense:", min(expenses$Amount), "\n")

# Mathematical Functions
cat("Rounded Average:", round(mean(expenses$Amount), 2), "\n")
cat("Square Root of 144:", sqrt(144), "\n")
cat("Absolute Value of -500:", abs(-500), "\n")
cat("5 Raised to Power 2:", 5^2, "\n")


# EXPERIMENT 2: Objects, dim() and Budget Analysis

monthly_budget <- 10000
total_spent <- sum(expenses$Amount)
remaining_budget <- monthly_budget - total_spent

budget_used <- (total_spent / monthly_budget) * 100
budget_remaining_percent <- (remaining_budget / monthly_budget) * 100

cat("\n===== BUDGET ANALYSIS =====\n")
cat("Monthly Budget: Rs.", monthly_budget, "\n")
cat("Total Spent: Rs.", total_spent, "\n")
cat("Remaining Budget: Rs.", remaining_budget, "\n")
cat("Budget Used:", round(budget_used, 2), "%\n")
cat("Budget Remaining:", round(budget_remaining_percent, 2), "%\n")

cat("\nDataset Dimensions:\n")
print(dim(expenses))


# EXPERIMENT 3: Data Frame Structure

cat("\n===== DATA FRAME STRUCTURE =====\n")
str(expenses)

cat("\nColumn Names:\n")
print(names(expenses))

cat("\nSummary of Expense Data:\n")
print(summary(expenses))


# EXPERIMENT 4: Expense Classification Using Loops

expenses$Expense_Level <- ""

for (i in 1:nrow(expenses)) {
  if (expenses$Amount[i] >= 1000) {
    expenses$Expense_Level[i] <- "High"
  } else if (expenses$Amount[i] >= 300) {
    expenses$Expense_Level[i] <- "Medium"
  } else {
    expenses$Expense_Level[i] <- "Low"
  }
}

cat("\n===== CLASSIFIED EXPENSES =====\n")
print(expenses)

# Count Expenses by Level

cat("\n===== EXPENSE LEVEL SUMMARY =====\n")

levels <- c("High", "Medium", "Low")

for (level in levels) {
  count <- sum(expenses$Expense_Level == level)
  cat(level, "Expenses:", count, "\n")
}


# Category-Wise Expense Analysis

categories <- unique(expenses$Category)

category_totals <- numeric(length(categories))

for (i in 1:length(categories)) {
  category_totals[i] <- sum(
    expenses$Amount[expenses$Category == categories[i]]
  )
}

category_analysis <- data.frame(
  Category = categories,
  Total_Spending = category_totals
)

category_analysis$Percentage <- round(
  category_analysis$Total_Spending / total_spent * 100, 2
)

cat("\n===== CATEGORY-WISE ANALYSIS =====\n")
print(category_analysis)

# Highest and Lowest Spending Categories

highest_category <- category_analysis$Category[
  which.max(category_analysis$Total_Spending)
]

lowest_category <- category_analysis$Category[
  which.min(category_analysis$Total_Spending)
]

cat("\nHighest Spending Category:", highest_category, "\n")
cat("Lowest Spending Category:", lowest_category, "\n")


# EXPERIMENT 5: Histogram

hist(
  expenses$Amount,
  main = "Personal Expense Distribution",
  xlab = "Expense Amount (Rs.)",
  ylab = "Number of Expenses",
  col = "skyblue",
  border = "black",
  breaks = 6
)


# ADDITIONAL GRAPH 1: Category-Wise Bar Chart

barplot(
  category_analysis$Total_Spending,
  names.arg = category_analysis$Category,
  main = "Category-Wise Spending Analysis",
  xlab = "Expense Category",
  ylab = "Total Spending (Rs.)",
  col = "lightgreen",
  border = "black",
  las = 2
)


# ADDITIONAL GRAPH 2: Category-Wise Pie Chart

pie(
  category_analysis$Total_Spending,
  labels = paste(
    category_analysis$Category,
    category_analysis$Percentage,
    "%",
    sep = " "
  ),
  main = "Percentage of Total Expenses",
  col = rainbow(length(categories))
)


# ADDITIONAL GRAPH 3: Budget Comparison

budget_values <- c(total_spent, remaining_budget)

barplot(
  budget_values,
  names.arg = c("Total Spent", "Remaining Budget"),
  main = "Monthly Budget Utilization",
  xlab = "Budget Status",
  ylab = "Amount (Rs.)",
  col = c("tomato", "lightblue"),
  border = "black"
)


# FINAL SUMMARY AND AUTOMATIC INSIGHTS

cat("\n====================================\n")
cat("       MONTHLY EXPENSE REPORT\n")
cat("====================================\n")

cat("Total Transactions:", nrow(expenses), "\n")
cat("Monthly Budget: Rs.", monthly_budget, "\n")
cat("Total Spending: Rs.", total_spent, "\n")
cat("Remaining Budget: Rs.", remaining_budget, "\n")
cat("Average Expense: Rs.", round(mean(expenses$Amount), 2), "\n")
cat("Maximum Expense: Rs.", max(expenses$Amount), "\n")
cat("Minimum Expense: Rs.", min(expenses$Amount), "\n")
cat("Budget Utilization:", round(budget_used, 2), "%\n")

cat("\n===== SPENDING INSIGHTS =====\n")

cat("Highest Spending Category:", highest_category, "\n")
cat("Lowest Spending Category:", lowest_category, "\n")

if (remaining_budget >= 0) {
  cat("Budget Status: Within Budget\n")
  cat("You have Rs.", remaining_budget, "remaining.\n")
} else {
  cat("Budget Status: Budget Exceeded\n")
  cat("You exceeded your budget by Rs.",
      abs(remaining_budget), "\n")
}

if (budget_used >= 80 && budget_used <= 100) {
  cat("Warning: You have used at least 80% of your budget.\n")
} else if (budget_used > 100) {
  cat("Suggestion: Reduce expenses to control your budget.\n")
} else {
  cat("Good: Your spending is within a comfortable range.\n")
}

cat("\n===== RECOMMENDATIONS =====\n")

if (highest_category == "Food") {
  cat("Track food purchases and reduce unnecessary spending.\n")
} else if (highest_category == "Shopping") {
  cat("Plan purchases and avoid unnecessary shopping.\n")
} else if (highest_category == "Travel") {
  cat("Consider economical transport options.\n")
} else if (highest_category == "Bills") {
  cat("Review bills and look for ways to save utilities.\n")
} else {
  cat("Review entertainment spending to identify savings.\n")
}

cat("\nPersonal Expense Analysis Completed Successfully!\n")