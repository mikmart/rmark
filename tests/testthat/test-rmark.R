describe("md_literal()", {
  heading_node <- md_first_child(parse_md("# Hello"))
  text_node <- md_first_child(heading_node)
  it("matches source", {
    expect_equal(md_literal(text_node), "Hello")
  })
  it("can be set", {
    md_literal(text_node) <- "World"
    expect_equal(md_literal(text_node), "World")
    expect_error(md_literal(text_node) <- NA)
  })
  it("signals useful errors", {
    expect_snapshot(md_literal(heading_node) <- "Hello", error = TRUE)
  })
  it("is missing for inapplicable nodes", {
    expect_equal(md_literal(heading_node), NA_character_)
  })
})


describe("md_heading_level()", {
  heading_node <- md_first_child(parse_md("# Hello"))
  text_node <- md_first_child(heading_node)
  it("matches source", {
    expect_equal(md_heading_level(heading_node), 1)
  })
  it("can be set", {
    md_heading_level(heading_node) <- 2
    expect_equal(md_heading_level(heading_node), 2)
    expect_error(md_heading_level(heading_node) <- NA)
  })
  it("signals useful errors", {
    expect_snapshot(md_heading_level(text_node) <- 1, error = TRUE)
  })
  it("is missing for inapplicable nodes", {
    expect_equal(md_heading_level(text_node), NA_integer_)
  })
})


describe("md_list_type()", {
  list_node <- md_first_child(parse_md("* Hello"))
  it("matches source", {
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


describe("md_list_delim()", {
  list_node <- md_first_child(parse_md("1. Hello"))
  it("matches source", {
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


describe("md_list_start()", {
  list_node <- md_first_child(parse_md("1. Hello"))
  it("matches source", {
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


describe("md_list_tight()", {
  list_node <- md_first_child(parse_md("1. Hello"))
  it("matches source", {
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
