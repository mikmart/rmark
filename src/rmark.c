#include "cmark.h"

#include <Rinternals.h>
#include <R_ext/Visibility.h>

SEXP rmark_node_symbol; // Initialised on package load.

#define NODE(x) rmark_get_cmark_node(x)

cmark_node *rmark_get_cmark_node(SEXP x) {
    if (TYPEOF(x) != EXTPTRSXP || R_ExternalPtrTag(x) != rmark_node_symbol) { 
        Rf_error("`x` must be an rmark_node object.");                        
    }                                                                         
    cmark_node *node = R_ExternalPtrAddr(x);                                  
    if (!node) {                                                              
        Rf_error("`x` pointer is invalid.");                                  
    }                                                                                                                                                 
    return node;
}

void rmark_finalize_rmark_node(SEXP x) {
    cmark_node *node = NODE(x);
    if (node) {
        cmark_node_free(node);
        R_ClearExternalPtr(x);
    }
}

SEXP make_md_node(cmark_node *node) {
    SEXP ptr = PROTECT(R_MakeExternalPtr(node, rmark_node_symbol, R_NilValue));
    R_RegisterCFinalizer(ptr, &rmark_finalize_rmark_node);
    // TODO: WRE says to only use as attribute or list element, so we're gonna find out.
    Rf_setAttrib(ptr, R_ClassSymbol, Rf_mkString("rmark_node"));
    UNPROTECT(1);
    return ptr;
}

attribute_visible
SEXP rmark_md_node_type(SEXP x) {
    return Rf_mkString(cmark_node_get_type_string(NODE(x)));
}

attribute_visible
SEXP rmark_parse_md(SEXP x) {
    int options = CMARK_OPT_DEFAULT;
    const char *input = CHAR(STRING_ELT(x, 0));
    size_t input_len = LENGTH(STRING_ELT(x, 0));
    cmark_parser *parser = cmark_parser_new(options);
    cmark_parser_feed(parser, input, input_len);
    cmark_node *root = cmark_parser_finish(parser);
    cmark_parser_free(parser);
    return make_md_node(root);
}

attribute_visible
void R_init_rmark(DllInfo *dll_info) {
    rmark_node_symbol = Rf_install("rmark_node");
}
