# md_literal(): signals useful errors

    Code
      md_literal(heading_node) <- "Hello"
    Condition
      Error in `md_literal<-`:
      ! `x` must be a leaf node, not a node of type <heading>.

# md_heading_level(): signals useful errors

    Code
      md_heading_level(text_node) <- 1
    Condition
      Error in `md_heading_level<-`:
      ! `x` must be a heading node, not a node of type <text>.

# md_list_type(): signals useful errors

    Code
      md_list_type(text_node) <- "bullet"
    Condition
      Error in `md_list_type<-`:
      ! `x` must be a list node, not a node of type <item>.

---

    Code
      md_list_type(list_node) <- "asdfgh"
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "bullet", "ordered"

# md_list_delim(): signals useful errors

    Code
      md_list_delim(text_node) <- "period"
    Condition
      Error in `md_list_delim<-`:
      ! `x` must be a list node, not a node of type <item>.

---

    Code
      md_list_delim(list_node) <- "asdfgh"
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "period", "paren"

# md_list_start(): signals useful errors

    Code
      md_list_start(text_node) <- "bullet"
    Condition
      Error in `md_list_start<-`:
      ! `x` must be a list node, not a node of type <item>.

