#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <cmark.h>

#include <Rinternals.h>
#include <R_ext/Visibility.h>
#include <R_ext/Connections.h>

SEXP rmark_node_symbol; // Initialised on package load.

#define NODE(x) rmark_node_get_cmark_node(x)

#define CHECK_RMARK_EXTPR(x)                                                                       \
    do {                                                                                           \
    if (TYPEOF(x) != EXTPTRSXP || R_ExternalPtrTag(x) != rmark_node_symbol) {                      \
        Rf_error("Expected `" #x "` to be an external pointer, got %s.", Rf_type2char(TYPEOF(x))); \
    }                                                                                              \
    } while(0)

cmark_node *rmark_node_get_cmark_node(SEXP x) {
    x = Rf_getAttrib(x, Rf_install("ptr"));
    CHECK_RMARK_EXTPR(x);
    cmark_node *node = R_ExternalPtrAddr(x);
    if (!node) {
        Rf_error("External pointer is invalid.");
    }
    return node;
}

void rmark_finalize_root_node_ptr(SEXP x) {
    CHECK_RMARK_EXTPR(x);
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
        CHECK_RMARK_EXTPR(parent_ptr);
        Rf_setAttrib(out, Rf_install("parent"), parent_ptr);
    }

    UNPROTECT(2);
    return out;
}

#define make_root_r_node(x) make_r_node(x, R_NilValue)

SEXP rmark_node_is_block(SEXP x) {
    return Rf_ScalarLogical(cmark_node_is_block(NODE(x)));
}

SEXP rmark_node_is_inline(SEXP x) {
    return Rf_ScalarLogical(cmark_node_is_inline(NODE(x)));
}

SEXP rmark_node_is_leaf(SEXP x) {
    return Rf_ScalarLogical(cmark_node_is_leaf(NODE(x)));
}

SEXP rmark_node_type(SEXP x) {
    return Rf_mkString(cmark_node_get_type_string(NODE(x)));
}

SEXP rmark_node_first_child(SEXP x) {
    cmark_node *child = cmark_node_first_child(NODE(x));
    return (child) ? make_r_node(child, x) : R_NilValue;
}

SEXP rmark_node_get_literal(SEXP x) {
    const char *content = cmark_node_get_literal(NODE(x));
    return (content) ? Rf_mkString(content) : Rf_ScalarString(NA_STRING);
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
    SEXP result = PROTECT(Rf_mkString(output));
    free(output);
    UNPROTECT(1);
    return result;
}

attribute_visible void R_init_rmark(DllInfo *dll_info) {
    rmark_node_symbol = Rf_install("rmark_node");
}
