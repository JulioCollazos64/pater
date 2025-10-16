reserved <- c(
  "(",
  ")",
  "[",
  "]",
  "+",
  "?",
  "!"
)
groups <- c(
  "{",
  "}"
)

SIMPLE_TOKENS <- c(
  reserved,
  groups
)

names(SIMPLE_TOKENS) <- c(reserved, groups)

tokenType <- c(
  "{",
  "}",
  "wildcard",
  "param",
  "char",
  "escape",
  "end",
  "(",
  ")",
  "[",
  "]",
  "+",
  "?",
  "!"
)
