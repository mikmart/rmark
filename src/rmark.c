#include <stdlib.h>
#include <string.h>

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

#define PTR(x) rmark_r_node_get_ptr(x)

SEXP rmark_r_node_get_ptr(SEXP x) {
    SEXP ptr = Rf_getAttrib(x, Rf_install("ptr"));
    if (TYPEOF(ptr) != EXTPTRSXP || R_ExternalPtrTag(ptr) != rmark_node_symbol) {
        Rf_error("`x` must be a Markdown node.");
    }
    return ptr;
}

#define NODE(x) ((cmark_node *)R_ExternalPtrAddr(PTR(x)))

void rmark_finalize_node_ptr(SEXP x) {
    SEXP root = R_ExternalPtrProtected(x);
    if (Rf_isNull(root)) {
        cmark_node_free(R_ExternalPtrAddr(x));
    }
}

SEXP make_r_node(cmark_node *node, SEXP root) {
    SEXP ptr = PROTECT(R_MakeExternalPtr(node, rmark_node_symbol, root));
    R_RegisterCFinalizer(ptr, &rmark_finalize_node_ptr);

    SEXP out = PROTECT(Rf_allocVector(VECSXP, 0));
    Rf_setAttrib(out, R_ClassSymbol, Rf_mkString("rmark_node"));
    Rf_setAttrib(out, Rf_install("ptr"), ptr);

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

/** Creating Nodes */

SEXP rmark_node_new(SEXP type) {
    cmark_node_type node_type = INTEGER(type)[0];
    cmark_node *node = cmark_node_new(node_type);
    return make_root_r_node(node);
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

const char *rmark_cmark_event_type_string(cmark_event_type event) {
    switch (event) {
        case CMARK_EVENT_NONE:  return "none";
        case CMARK_EVENT_ENTER: return "enter";
        case CMARK_EVENT_EXIT:  return "exit";
        case CMARK_EVENT_DONE:  return "done";
    }
    return "";
}

void rmark_finalize_iter_ptr(SEXP x) {
    cmark_iter_free(R_ExternalPtrAddr(x));
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
    R_RegisterCFinalizer(ptr, &rmark_finalize_iter_ptr);

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

SEXP rmark_node_get_list_tight(SEXP x) {
    int list_tight = cmark_node_get_list_tight(NODE(x));
    return Rf_ScalarLogical(list_tight);
}

SEXP rmark_node_set_list_tight(SEXP x, SEXP value) {
    int list_tight = LOGICAL(value)[0];
    if (!cmark_node_set_list_tight(NODE(x), list_tight)) {
        Rf_error("Failed to set list tight to \"%d\".", list_tight);
    };
    return x;
}

SEXP rmark_node_get_fence_info(SEXP x) {
    const char *info = cmark_node_get_fence_info(NODE(x));
    return (info) ? Rf_mkStringUTF8(info) : Rf_ScalarString(NA_STRING);
}

SEXP rmark_node_set_fence_info(SEXP x, SEXP value) {
    const char *info = Rf_translateCharUTF8(STRING_ELT(value, 0));
    if (!cmark_node_set_fence_info(NODE(x), info)) {
        Rf_error("Failed to set node fence info to \"%s\".", info);
    };
    return x;
}

SEXP rmark_node_get_url(SEXP x) {
    const char *url = cmark_node_get_url(NODE(x));
    return (url) ? Rf_mkStringUTF8(url) : Rf_ScalarString(NA_STRING);
}

SEXP rmark_node_set_url(SEXP x, SEXP value) {
    const char *url = Rf_translateCharUTF8(STRING_ELT(value, 0));
    if (!cmark_node_set_url(NODE(x), url)) {
        Rf_error("Failed to set node url to \"%s\".", url);
    };
    return x;
}

SEXP rmark_node_get_title(SEXP x) {
    const char *title = cmark_node_get_title(NODE(x));
    return (title) ? Rf_mkStringUTF8(title) : Rf_ScalarString(NA_STRING);
}

SEXP rmark_node_set_title(SEXP x, SEXP value) {
    const char *title = Rf_translateCharUTF8(STRING_ELT(value, 0));
    if (!cmark_node_set_title(NODE(x), title)) {
        Rf_error("Failed to set node title to \"%s\".", title);
    };
    return x;
}

// TODO: on_enter and on_exit for custom nodes not supported.

SEXP rmark_node_get_start_line(SEXP x) {
    return Rf_ScalarInteger(cmark_node_get_start_line(NODE(x)));
}

SEXP rmark_node_get_start_column(SEXP x) {
    return Rf_ScalarInteger(cmark_node_get_start_column(NODE(x)));
}

SEXP rmark_node_get_end_line(SEXP x) {
    return Rf_ScalarInteger(cmark_node_get_end_line(NODE(x)));
}

SEXP rmark_node_get_end_column(SEXP x) {
    return Rf_ScalarInteger(cmark_node_get_end_column(NODE(x)));
}

/** Tree Manipulation */

// TODO: Handle memory management.

SEXP rmark_node_unlink(SEXP x) {
    cmark_node_unlink(NODE(x));
    return R_NilValue;
}

SEXP rmark_node_insert_before(SEXP x, SEXP new) {
    int ret = cmark_node_insert_before(NODE(x), NODE(new));
    return Rf_ScalarLogical(ret);
}

SEXP rmark_node_insert_after(SEXP x, SEXP new) {
    int ret = cmark_node_insert_after(NODE(x), NODE(new));
    return Rf_ScalarLogical(ret);
}

SEXP rmark_node_replace(SEXP x, SEXP new) {
    int ret = cmark_node_replace(NODE(x), NODE(new));
    return Rf_ScalarLogical(ret);
}

SEXP rmark_node_prepend_child(SEXP x, SEXP new) {
    int ret = cmark_node_prepend_child(NODE(x), NODE(new));
    return Rf_ScalarLogical(ret);
}

SEXP rmark_node_append_child(SEXP x, SEXP new) {
    int ret = cmark_node_append_child(NODE(x), NODE(new));
    return Rf_ScalarLogical(ret);
}

// TODO: Support consolidate text nodes? What if we hold external pointers to them?

/** Parsing */

void rmark_finalize_parser_ptr(SEXP x) {
    cmark_parser_free(R_ExternalPtrAddr(x));
}

SEXP rmark_read_md(SEXP x) {
#if R_CONNECTIONS_VERSION > 1
    Rf_error("rmark was built with an unsupported version of the R connections API.");
#else
    int options = CMARK_OPT_DEFAULT;
    Rconnection conn = R_GetConnection(x);
    cmark_parser *parser = cmark_parser_new(options);

    // Make sure parser is free'd if there's an error reading from the connection.
    SEXP ptr = PROTECT(R_MakeExternalPtr(parser, R_NilValue, R_NilValue));
    R_RegisterCFinalizer(ptr, &rmark_finalize_parser_ptr);

    size_t bytes_read = 0;
    char *buf = R_alloc(BUFSIZ, sizeof(char));
    while ((bytes_read = R_ReadConnection(conn, buf, BUFSIZ))) {
        R_CheckUserInterrupt();
        cmark_parser_feed(parser, buf, bytes_read);
    }
    cmark_node *root = cmark_parser_finish(parser);

    UNPROTECT(1);
    return make_root_r_node(root);
#endif // R_CONNECTIONS_VERSION
}

SEXP rmark_parse_md(SEXP x) {
    int options = CMARK_OPT_DEFAULT;
    const char *input = Rf_translateCharUTF8(STRING_ELT(x, 0));
    cmark_node *root = cmark_parse_document(input, strlen(input), options);
    return make_root_r_node(root);
}

/** Rendering */

// TODO: Allow customizing rendering options from R.

typedef enum {
    RMARK_OUTPUT_NONE,
    RMARK_OUTPUT_COMMONMARK,
    RMARK_OUTPUT_HTML,
    RMARK_OUTPUT_LATEX,
    RMARK_OUTPUT_MAN,
    RMARK_OUTPUT_XML,
} rmark_output_format;

SEXP rmark_render(SEXP x, SEXP output_format, SEXP output_width) {
    cmark_node *root = NODE(x);
    int options = CMARK_OPT_DEFAULT;
    int width = INTEGER(output_width)[0];
    rmark_output_format format = INTEGER(output_format)[0];

    char *output = NULL;
    switch (format) {
        case RMARK_OUTPUT_COMMONMARK: {
            output = cmark_render_commonmark(root, options, width);
        } break;
        case RMARK_OUTPUT_HTML: {
            output = cmark_render_html(root, options);
        } break;
        case RMARK_OUTPUT_LATEX: {
            output = cmark_render_latex(root, options, width);
        } break;
        case RMARK_OUTPUT_MAN: {
            output = cmark_render_man(root, options, width);
        } break;
        case RMARK_OUTPUT_XML: {
            output = cmark_render_xml(root, options);
        } break;
        default:
            Rf_error("Unknown output format: %d.", format);
    }
    if (!output) {
        Rf_error("Failed to render document.");
    }

    SEXP result = PROTECT(Rf_mkStringUTF8(output));
    free(output);
    UNPROTECT(1);
    return result;
}

/** Version Info */

SEXP rmark_cmark_version_string() {
    return Rf_mkStringUTF8(cmark_version_string());
}

/** R Package Initialization */

attribute_visible void R_init_rmark(DllInfo *dll_info) {
    rmark_node_symbol = Rf_install("rmark_node");
}
