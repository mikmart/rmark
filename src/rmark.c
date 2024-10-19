#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <cmark.h>

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Visibility.h>
#include <R_ext/Connections.h>

SEXP rmark_node_symbol; // Initialised on package load.

#define STOPIFNOT(x) \
    do { if (!(x)) Rf_error("Internal error: "#x" is not true."); } while (0)
#define CHECK_TYPEOF(x, type) STOPIFNOT(TYPEOF(x) == (type))

#define Rf_mkStringUTF8(x) rmark_make_utf8_strsxp(x)

SEXP rmark_make_utf8_strsxp(const char *string) {
    SEXP charsxp = Rf_mkCharCE(string, CE_UTF8);
    PROTECT(charsxp);
    SEXP strsxp = Rf_ScalarString(charsxp);
    UNPROTECT(1);
    return strsxp;
}

#define NODE(x) rmark_node_get_cmark_node(x)

cmark_node *rmark_node_get_cmark_node(SEXP x) {
    x = Rf_getAttrib(x, Rf_install("ptr"));
    CHECK_TYPEOF(x, EXTPTRSXP);
    cmark_node *node = R_ExternalPtrAddr(x);
    if (!node) {
        Rf_error("External pointer is invalid.");
    }
    return node;
}

void rmark_finalize_root_node_ptr(SEXP x) {
    CHECK_TYPEOF(x, EXTPTRSXP);
    cmark_node *node = R_ExternalPtrAddr(x);
    if (node) {
        cmark_node_free(node);
        R_ClearExternalPtr(x);
    }
}

SEXP make_r_node(cmark_node *node, SEXP parent) {
    SEXP ptr = PROTECT(R_MakeExternalPtr(node, rmark_node_symbol, R_NilValue));
    if (Rf_isNull(parent)) {
        R_RegisterCFinalizer(ptr, &rmark_finalize_root_node_ptr);
    }

    SEXP out = PROTECT(Rf_allocVector(VECSXP, 0));
    Rf_setAttrib(out, R_ClassSymbol, Rf_mkString("rmark_node"));
    Rf_setAttrib(out, Rf_install("ptr"), ptr);

    if (!Rf_isNull(parent)) {
        // Keep a reference to the parent pointer to stop it from being collected early.
        SEXP parent_ptr = Rf_getAttrib(parent, Rf_install("ptr"));
        CHECK_TYPEOF(parent_ptr, EXTPTRSXP);
        Rf_setAttrib(out, Rf_install("parent"), parent_ptr);
    }

    UNPROTECT(2);
    return out;
}

#define make_root_r_node(x) make_r_node(x, R_NilValue)

/** Classification */

SEXP rmark_node_is_block(SEXP x) {
    return Rf_ScalarLogical(cmark_node_is_block(NODE(x)));
}

SEXP rmark_node_is_inline(SEXP x) {
    return Rf_ScalarLogical(cmark_node_is_inline(NODE(x)));
}

SEXP rmark_node_is_leaf(SEXP x) {
    return Rf_ScalarLogical(cmark_node_is_leaf(NODE(x)));
}

/** Tree Traversal */

// FIXME: Assign parents properly for all traversal nodes.

SEXP rmark_node_next(SEXP x) {
    cmark_node *node = cmark_node_next(NODE(x));
    return (node) ? make_r_node(node, x) : R_NilValue;
}

SEXP rmark_node_previous(SEXP x) {
    cmark_node *node = cmark_node_previous(NODE(x));
    return (node) ? make_r_node(node, x) : R_NilValue;
}

SEXP rmark_node_parent(SEXP x) {
    cmark_node *node = cmark_node_parent(NODE(x));
    return (node) ? make_r_node(node, x) : R_NilValue;
}

SEXP rmark_node_first_child(SEXP x) {
    cmark_node *node = cmark_node_first_child(NODE(x));
    return (node) ? make_r_node(node, x) : R_NilValue;
}

SEXP rmark_node_last_child(SEXP x) {
    cmark_node *node = cmark_node_last_child(NODE(x));
    return (node) ? make_r_node(node, x) : R_NilValue;
}

/** Iteration */

// TODO: Expose full iterator API to R?

void rmark_iter_free(SEXP x) {
    CHECK_TYPEOF(x, EXTPTRSXP);
    cmark_iter *iter = R_ExternalPtrAddr(x);
    if (iter) {
        cmark_iter_free(iter);
        R_ClearExternalPtr(x);
    }
}

const char *rmark_cmark_event_type_string(cmark_event_type event) {
    switch (event) {
        case CMARK_EVENT_NONE:  return "none";
        case CMARK_EVENT_ENTER: return "enter";
        case CMARK_EVENT_EXIT:  return "exit";
        case CMARK_EVENT_DONE:  return "done";
    }
    return "";
}

SEXP rmark_iterate(SEXP x, SEXP callback, SEXP envir) {
    if (!Rf_isFunction(callback))
        Rf_error("`callback` must be a function.");
    if (!Rf_isEnvironment(envir))
        Rf_error("`envir` must be an environment.");

    cmark_iter *iter = cmark_iter_new(NODE(x));
    cmark_event_type event = {0};

    // Make sure iterator gets released even if R code errors.
    SEXP ptr = PROTECT(R_MakeExternalPtr(iter, R_NilValue, R_NilValue));
    R_RegisterCFinalizer(ptr, &rmark_iter_free);

    while ((event = cmark_iter_next(iter)) != CMARK_EVENT_DONE) {
        const char *event_string = rmark_cmark_event_type_string(event);
        cmark_node *node = cmark_iter_get_node(iter);
        SEXP r_node = PROTECT(make_r_node(node, x));
        SEXP r_event = PROTECT(Rf_mkString(event_string));
        SEXP r_call = PROTECT(Rf_lang3(callback, r_node, r_event));
        Rf_eval(r_call, envir);
        UNPROTECT(3);
    }

    UNPROTECT(1);
    return R_NilValue;
}

/** Accessors */

// User data is not supported; We already have attributes in R.

SEXP rmark_node_get_type_string(SEXP x) {
    return Rf_mkStringUTF8(cmark_node_get_type_string(NODE(x)));
}

SEXP rmark_node_get_literal(SEXP x) {
    const char *content = cmark_node_get_literal(NODE(x));
    return (content) ? Rf_mkStringUTF8(content) : Rf_ScalarString(NA_STRING);
}

SEXP rmark_node_set_literal(SEXP x, SEXP value) {
    const char *content = Rf_translateCharUTF8(STRING_ELT(value, 0));
    if (!cmark_node_set_literal(NODE(x), content)) {
        Rf_error("Failed to set node literal to \"%s\".", content);
    };
    return x;
}

SEXP rmark_node_get_heading_level(SEXP x) {
    int heading_level = cmark_node_get_heading_level(NODE(x));
    return Rf_ScalarInteger((heading_level) ? heading_level : NA_INTEGER);
}

SEXP rmark_node_set_heading_level(SEXP x, SEXP value) {
    int heading_level = INTEGER(value)[0];
    if (!cmark_node_set_heading_level(NODE(x), heading_level)) {
        Rf_error("Failed to set heading level to \"%d\".", heading_level);
    };
    return x;
}

SEXP rmark_node_get_list_type(SEXP x) {
    cmark_list_type list_type = cmark_node_get_list_type(NODE(x));
    return Rf_ScalarInteger((list_type) ? list_type : NA_INTEGER);
}

SEXP rmark_node_set_list_type(SEXP x, SEXP value) {
    cmark_list_type list_type = INTEGER(value)[0];
    if (!cmark_node_set_list_type(NODE(x), list_type)) {
        Rf_error("Failed to set list type to \"%d\".", list_type);
    };
    return x;
}

SEXP rmark_node_get_list_delim(SEXP x) {
    cmark_delim_type list_delim = cmark_node_get_list_delim(NODE(x));
    return Rf_ScalarInteger((list_delim) ? list_delim : NA_INTEGER);
}

SEXP rmark_node_set_list_delim(SEXP x, SEXP value) {
    cmark_delim_type list_delim = INTEGER(value)[0];
    if (!cmark_node_set_list_delim(NODE(x), list_delim)) {
        Rf_error("Failed to set list delim to \"%d\".", list_delim);
    };
    return x;
}

SEXP rmark_node_get_list_start(SEXP x) {
    int list_start = cmark_node_get_list_start(NODE(x));
    return Rf_ScalarInteger((list_start) ? list_start : NA_INTEGER);
}

SEXP rmark_node_set_list_start(SEXP x, SEXP value) {
    int list_start = INTEGER(value)[0];
    if (!cmark_node_set_list_start(NODE(x), list_start)) {
        Rf_error("Failed to set list start to \"%d\".", list_start);
    };
    return x;
}

SEXP rmark_read_md(SEXP x) {
    static_assert(R_CONNECTIONS_VERSION == 1);
    Rconnection conn = R_GetConnection(x);
    size_t bytes_read = 0;
    char buf[BUFSIZ] = {0};

    int options = CMARK_OPT_DEFAULT;
    cmark_parser *parser = cmark_parser_new(options);
    while ((bytes_read = R_ReadConnection(conn, buf, BUFSIZ))) {
        cmark_parser_feed(parser, buf, bytes_read);
    }
    cmark_node *root = cmark_parser_finish(parser);
    cmark_parser_free(parser);

    return make_root_r_node(root);
}

SEXP rmark_parse_md(SEXP x) {
    int options = CMARK_OPT_DEFAULT;
    const char *input = Rf_translateCharUTF8(STRING_ELT(x, 0));
    cmark_node *root = cmark_parse_document(input, strlen(input), options);
    return make_root_r_node(root);
}

SEXP rmark_render_md(SEXP x, SEXP width) {
    int options = CMARK_OPT_DEFAULT;
    char *output = cmark_render_commonmark(NODE(x), options, INTEGER(width)[0]);
    SEXP result = PROTECT(Rf_mkStringUTF8(output));
    free(output);
    UNPROTECT(1);
    return result;
}

attribute_visible void R_init_rmark(DllInfo *dll_info) {
    rmark_node_symbol = Rf_install("rmark_node");
}
