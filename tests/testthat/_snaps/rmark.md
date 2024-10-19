# list type: signals useful errors

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

# list delim: signals useful errors

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

# list start: signals useful errors

    Code
      md_list_start(text_node) <- "bullet"
    Condition
      Error in `md_list_start<-`:
      ! `x` must be a list node, not a node of type <item>.

