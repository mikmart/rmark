describe("list type", {
  list_node <- md_first_child(parse_md("* Hello"))
  it("matches text content", {
    expect_equal(md_list_type(list_node), "bullet")
  })
  it("can be set", {
    md_list_type(list_node) <- "ordered"
    expect_equal(md_list_type(list_node), "ordered")
  })
  it("signals useful errors", {
    text_node <- md_first_child(list_node)
    expect_snapshot(md_list_type(text_node) <- "bullet", error = TRUE)
    expect_snapshot(md_list_type(list_node) <- "asdfgh", error = TRUE)
  })
})

describe("list delim", {
  list_node <- md_first_child(parse_md("1. Hello"))
  it("matches text content", {
    expect_equal(md_list_delim(list_node), "period")
  })
  it("can be set", {
    md_list_delim(list_node) <- "paren"
    expect_equal(md_list_delim(list_node), "paren")
  })
  it("signals useful errors", {
    text_node <- md_first_child(list_node)
    expect_snapshot(md_list_delim(text_node) <- "period", error = TRUE)
    expect_snapshot(md_list_delim(list_node) <- "asdfgh", error = TRUE)
  })
})

describe("list start", {
  list_node <- md_first_child(parse_md("1. Hello"))
  it("matches text content", {
    expect_equal(md_list_start(list_node), 1)
  })
  it("can be set", {
    md_list_start(list_node) <- 2
    expect_equal(md_list_start(list_node), 2)
    expect_error(md_list_start(list_node) <- NA)
  })
  it("signals useful errors", {
    text_node <- md_first_child(list_node)
    expect_snapshot(md_list_start(text_node) <- "bullet", error = TRUE)
  })
})

describe("list tight", {
  list_node <- md_first_child(parse_md("1. Hello"))
  it("matches text content", {
    expect_equal(md_list_tight(list_node), TRUE)
  })
  it("can be set", {
    md_list_tight(list_node) <- FALSE
    expect_equal(md_list_tight(list_node), FALSE)
    expect_error(md_list_tight(list_node) <- NA)
  })
  it("signals useful errors", {
    text_node <- md_first_child(list_node)
    expect_error(md_list_tight(text_node) <- TRUE)
  })
})
