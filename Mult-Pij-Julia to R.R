# Load required package
if (!require(stringr)) install.packages("stringr")
library(stringr)

# Define function to process one Julia-style expression
process_expression <- function(expr_str) {
  expr_str <- str_replace_all(expr_str, "λ(\\d)(\\d)", "q\\1\\2")
  lhs <- str_match(expr_str, "^\\s*(P\\[\\d,\\d\\])\\s*=")[,2]
  rhs <- str_match(expr_str, "^\\s*P\\[\\d,\\d\\]\\s*=\\s*(.*)$")[,2]
  pattern <- "\\(1//(\\d+)\\)\\*\\((.*?)\\)"
  order_matches <- str_match_all(rhs, pattern)[[1]]
  
  remainder <- rhs
  if (nrow(order_matches) > 0) {
    for (i in seq_len(nrow(order_matches))) {
      remainder <- str_replace(remainder, fixed(order_matches[i,1]), "")
    }
    remainder <- str_trim(remainder)
  }
  
  result <- list(variable = lhs)
  if (remainder != "") result[["order_1"]] <- remainder
  for (i in seq_len(nrow(order_matches))) {
    label <- paste0("order_", order_matches[i,2])
    result[[label]] <- str_trim(order_matches[i,3])
  }
  
  return(result)
}

# 🔄 Read expressions from file
input_file <- "expressions.txt"
lines <- readLines(input_file)

# 🧾 Parse each expression
parsed_matrix <- lapply(lines, function(line) {
  if (str_detect(line, "^\\s*P\

\[\\d,\\d\\]

")) {
    process_expression(line)
  } else {
    NULL
  }
})
parsed_matrix <- Filter(Negate(is.null), parsed_matrix)

# 📝 Write results to output.txt
output_file <- "output.txt"
writeLines("", output_file)  # Clear file if exists

for (expr in parsed_matrix) {
  cat("🔍 Expression Breakdown for", expr$variable, "\n\n")
  write(paste0("Expression Breakdown for ", expr$variable), output_file, append = TRUE)
  for (name in names(expr)[-1]) {
    cat("•", name, ":\n", expr[[name]], "\n\n")
    write(paste("•", name, ":", expr[[name]]), output_file, append = TRUE)
  }
  write("\n", output_file, append = TRUE)
}
