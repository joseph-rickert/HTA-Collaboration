# Load required package
# Load required package
if (!require(stringr)) install.packages("stringr")
library(stringr)

# Function to translate and decompose a Julia-style expression
process_expression <- function(expr_str) {
  # Step 1: Replace λij → qij
  expr_str <- str_replace_all(expr_str, "λ(\\d)(\\d)", "q\\1\\2")
  
  # Step 2: Extract LHS (P[i,j]) and RHS
  lhs <- str_match(expr_str, "^\\s*(P\\[\\d,\\d\\])\\s*=")[,2]
  
  rhs <- str_match(expr_str, "^\\s*P\\[\\d,\\d\\]\\s*=\\s*(.*)$")[,2]
  
  # Step 3: Match all subterms in the form (1//n)*(...)
  pattern <- "\\(1//(\\d+)\\)\\*\\((.*?)\\)"
  order_matches <- str_match_all(rhs, pattern)[[1]]
  
  # Step 4: Remove those matched pieces from RHS to isolate order-1 remainder
  remainder <- rhs
  if (nrow(order_matches) > 0) {
    for (i in seq_len(nrow(order_matches))) {
      remainder <- str_replace(remainder, fixed(order_matches[i,1]), "")
    }
    remainder <- str_trim(remainder)
  }
  
  # Step 5: Build output structure
  result <- list(variable = lhs)
  if (remainder != "") result[["order_1"]] <- remainder
  for (i in seq_len(nrow(order_matches))) {
    order_label <- paste0("order_", order_matches[i,2])
    result[[order_label]] <- str_trim(order_matches[i,3])
  }
  
  return(result)
}

# 🧪 Try it with your example
example <- "P[1,1] = 1 - u*λ12 + (1//2)*((u^2)*(λ12^2) + (u^2)*λ12*λ21) + (1//3)*(-(1//2)*u*((u^2)*(λ12^2) + (u^2)*λ12*λ21)*λ12 + (1//2)*u*(-(u^2)*λ12*λ21 + (u^2)*(-λ21 - λ23 - λ24)*λ21)*λ12)"

# 🔍 Run and print breakdown
translated <- process_expression(example)
cat("🔍 Expression Breakdown for", translated$variable, "\n\n")
for (name in names(translated)[-1]) {
  cat("•", name, ":\n", translated[[name]], "\n\n")
}
