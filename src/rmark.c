#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include <cmark.h>

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Visibility.h>
#include <R_ext/Connections.h>

// Initialised on package load.
SEXP rmark_root_symbol;
SEXP rmark_node_symbol;

#define HAS_RMARK_TAG(x) \
    (R_ExternalPtrTag(x) == rmark_node_symbol || R_ExternalPtrTag(x) == rmark_root_symbol)

SEXP rmark_r_node_new(SEXP node) {
    if (TYPEOF(node) != EXTPTRSXP || !HAS_RMARK_TAG(node)) {
        Rf_error("`node` must be an rmark external pointer, not <%s>.", Rf_type2char(TYPEOF(node)));
    }
    
    SEXP out = PROTECT(Rf_allocVector(VECSXP, 0));
    Rf_setAttrib(out, R_ClassSymbol, Rf_mkString("rmark_node"));
    Rf_setAttrib(out, Rf_install("xptr"), node);
    UNPROTECT(1);
    
    return out;
}

#define PTR(x) rmark_r_node_get_ptr(x)
#define NODE(x) ((cmark_node *)R_ExternalPtrAddr(PTR(x)))
#define ROOT(x) rmark_node_get_root(PTR(x))

SEXP rmark_r_node_get_ptr(SEXP x) {
    SEXP node = Rf_getAttrib(x, Rf_install("xptr"));
    if (TYPEOF(node) != EXTPTRSXP || !HAS_RMARK_TAG(node)) {
        Rf_error("`x` must be a Markdown node, not <%s>.", Rf_type2char(TYPEOF(x)));
    }
    return node;
}

SEXP rmark_node_get_root(SEXP x) {
    SEXP tag = R_ExternalPtrTag(x);
    if (tag == rmark_root_symbol) {
        return x;
    } else {
        return R_ExternalPtrProtected(x);
    }
}

void rmark_finalize_node_ptr(SEXP x) {
    SEXP tag = R_ExternalPtrTag(x);
    if (tag == rmark_root_symbol) {
        cmark_node *node = R_ExternalPtrAddr(x);
        if (node) {
            cmark_node_free(node);
            R_ClearExternalPtr(x);
        }
    }
}

SEXP rmark_tree_new(cmark_node *node) {
    SEXP ptr = PROTECT(R_MakeExternalPtr(node, rmark_root_symbol, R_NilValue));
    R_RegisterCFinalizer(ptr, &rmark_finalize_node_ptr);

    // Keep a reference to self to move if we get adopted into another tree.
    SEXP ref = PROTECT(R_MakeWeakRef(ptr, R_NilValue, R_NilValue, FALSE));
    R_SetExternalPtrProtected(ptr, CONS(ref, R_NilValue));

    UNPROTECT(2);
    return ptr;
}

SEXP rmark_tree_make_node(SEXP root, cmark_node *node) {
    // Protect root to ensure we don't get deallocated while we live.
    SEXP ptr = PROTECT(R_MakeExternalPtr(node, rmark_node_symbol, root));
    R_RegisterCFinalizer(ptr, &rmark_finalize_node_ptr);

    // Add weak reference to root so that we can be moved when unlinked.
    SEXP ref = PROTECT(R_MakeWeakRef(ptr, R_NilValue, R_NilValue, FALSE));
    SEXP refs = R_ExternalPtrProtected(root);
    while (refs != R_NilValue && R_WeakRefKey(CAR(refs)) == R_NilValue)
        refs = CDR(refs); // Quick clean-up of some invalidated refs.
    R_SetExternalPtrProtected(root, CONS(ref, refs));

    UNPROTECT(2);
    return ptr;
}

// Debugging purposes only.
SEXP rmark_r_node_list_root_refs(SEXP r_node) {
    SEXP refs = R_ExternalPtrProtected(ROOT(r_node));
    SEXP list = R_NilValue;
    for (SEXP cell = refs; cell != R_NilValue; cell = CDR(cell))
        list = CONS(R_WeakRefKey(CAR(cell)), list);
    return Rf_PairToVectorList(list);
}

#define make_r_root(node) rmark_r_node_new(rmark_tree_new(node))
#define make_r_node(root, node) rmark_r_node_new(rmark_tree_make_node(root, node))

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
    return make_r_root(node);
}

/** Tree Traversal */

SEXP rmark_node_next(SEXP x) {
    cmark_node *node = cmark_node_next(NODE(x));
    return (node) ? make_r_node(ROOT(x), node) : R_NilValue;
}

SEXP rmark_node_previous(SEXP x) {
    cmark_node *node = cmark_node_previous(NODE(x));
    return (node) ? make_r_node(ROOT(x), node) : R_NilValue;
}

SEXP rmark_node_parent(SEXP x) {
    cmark_node *node = cmark_node_parent(NODE(x));
    return (node) ? make_r_node(ROOT(x), node) : R_NilValue;
}

SEXP rmark_node_first_child(SEXP x) {
    cmark_node *node = cmark_node_first_child(NODE(x));
    return (node) ? make_r_node(ROOT(x), node) : R_NilValue;
}

SEXP rmark_node_last_child(SEXP x) {
    cmark_node *node = cmark_node_last_child(NODE(x));
    return (node) ? make_r_node(ROOT(x), node) : R_NilValue;
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
        SEXP r_node = PROTECT(make_r_node(ROOT(x), node));
        SEXP r_event = PROTECT(Rf_mkString(event_string));
        SEXP r_call = PROTECT(Rf_lang3(callback, r_node, r_event));
        Rf_eval(r_call, envir);
        UNPROTECT(3);
    }

    UNPROTECT(1);
    return R_NilValue;
}

/** Accessors */

#define Rf_mkStringUTF8(x) rmark_make_utf8_strsxp(x)

SEXP rmark_make_utf8_strsxp(const char *string) {
    SEXP charsxp = Rf_mkCharCE(string, CE_UTF8);
    PROTECT(charsxp);
    SEXP strsxp = Rf_ScalarString(charsxp);
    UNPROTECT(1);
    return strsxp;
}

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

// Move a node and all its descendents to a new root. This only moves the R
// references to ensure GC protection. Used after changing cmark node links.
SEXP rmark_tree_adopt_node(SEXP root, SEXP node) {
    // root == node is used to promote a node to a tree while moving refs from the old root.
    if (R_ExternalPtrTag(root) != rmark_root_symbol && root != node)
        Rf_error("internal error: invalid root");
    if (R_ExternalPtrTag(node) != rmark_root_symbol && R_ExternalPtrTag(node) != rmark_node_symbol)
        Rf_error("internal error: invalid node");

    SEXP old_root = rmark_node_get_root(node);
    if (old_root == root) {
        return R_NilValue;
    }

    // Iterate old references and move any descendants of node to root.
    SEXP old_root_refs = R_ExternalPtrProtected(old_root);
    SEXP adopted_refs = R_NilValue; // Empty pairlist.
    cmark_node *adopted_node = R_ExternalPtrAddr(node);

    // Also prune stale refs from old root since we have to go through the list anyway.
    for (SEXP prev = NULL, cell = old_root_refs; cell != R_NilValue; cell = CDR(cell)) {
        SEXP ref = CAR(cell);
        SEXP ptr = R_WeakRefKey(ref);
        bool should_unlink_cell = false;

        if (ptr == R_NilValue) {
            should_unlink_cell = true; // Stale reference.
        } else {
            // Follow parent chain to check if ancestor is being moved.
            cmark_node *ancestor_node = R_ExternalPtrAddr(ptr);
            if (ancestor_node) {
                do {
                    if (ancestor_node == adopted_node) {
                        // Link reference to new root.
                        R_SetExternalPtrProtected(ptr, root);
                        adopted_refs = CONS(ref, adopted_refs);
                        should_unlink_cell = true;
                        break;
                    }
                    ancestor_node = cmark_node_parent(ancestor_node);
                } while(ancestor_node);
            }
        }

        if (should_unlink_cell) {
            if (prev) {
                SETCDR(prev, CDR(cell));
            } else {
                R_SetExternalPtrProtected(old_root, CDR(cell));
            }
        } else {
            prev = cell;
        }
    }

    if (root == node) {
        R_SetExternalPtrTag(root, rmark_root_symbol);
        R_SetExternalPtrProtected(root, adopted_refs);
    } else {
        // Add adopted references to root refs.
        SEXP refs = R_ExternalPtrProtected(root);
        while (refs != R_NilValue && R_WeakRefKey(CAR(refs)) == R_NilValue)
            refs = CDR(refs); // Quick clean-up of some invalidated refs.
        for (SEXP cell = adopted_refs; cell != R_NilValue; cell = CDR(cell))
            refs = CONS(CAR(cell), refs);
        R_SetExternalPtrProtected(root, refs);
        R_SetExternalPtrProtected(node, root);
        R_SetExternalPtrTag(node, rmark_node_symbol);
    }

    return R_NilValue;
}

SEXP rmark_node_promote_to_tree(SEXP node) {
    return rmark_tree_adopt_node(node, node);
}

SEXP rmark_node_unlink(SEXP x) {
    cmark_node_unlink(NODE(x));
    rmark_node_promote_to_tree(PTR(x));
    return R_NilValue;
}

SEXP rmark_node_insert_before(SEXP x, SEXP new) {
    int ok = cmark_node_insert_before(NODE(x), NODE(new));
    if (ok) {
        rmark_tree_adopt_node(ROOT(x), PTR(new));
    }
    return Rf_ScalarLogical(ok);
}

SEXP rmark_node_insert_after(SEXP x, SEXP new) {
    int ok = cmark_node_insert_after(NODE(x), NODE(new));
    if (ok) {
        rmark_tree_adopt_node(ROOT(x), PTR(new));
    }
    return Rf_ScalarLogical(ok);
}

SEXP rmark_node_replace(SEXP x, SEXP new) {
    int ok = cmark_node_replace(NODE(x), NODE(new));
    if (ok) {
        rmark_tree_adopt_node(ROOT(x), PTR(new));
    }
    return Rf_ScalarLogical(ok);
}

SEXP rmark_node_prepend_child(SEXP x, SEXP new) {
    int ok = cmark_node_prepend_child(NODE(x), NODE(new));
    if (ok) {
        rmark_tree_adopt_node(ROOT(x), PTR(new));
    }
    return Rf_ScalarLogical(ok);
}

SEXP rmark_node_append_child(SEXP x, SEXP new) {
    int ok = cmark_node_append_child(NODE(x), NODE(new));
    if (ok) {
        rmark_tree_adopt_node(ROOT(x), PTR(new));
    }
    return Rf_ScalarLogical(ok);
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
    return make_r_root(root);
#endif // R_CONNECTIONS_VERSION
}

SEXP rmark_parse_md(SEXP x) {
    int options = CMARK_OPT_DEFAULT;
    const char *input = Rf_translateCharUTF8(STRING_ELT(x, 0));
    cmark_node *root = cmark_parse_document(input, strlen(input), options);
    return make_r_root(root);
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

R_CallMethodDef call_method_defs[] = {
    { "rmark_cmark_version_string",   (DL_FUNC) &rmark_cmark_version_string,   0 },
    { "rmark_read_md",                (DL_FUNC) &rmark_read_md,                1 },
    { "rmark_parse_md",               (DL_FUNC) &rmark_parse_md,               1 },
    { "rmark_render",                 (DL_FUNC) &rmark_render,                 3 },
    { "rmark_node_is_block",          (DL_FUNC) &rmark_node_is_block,          1 },
    { "rmark_node_is_inline",         (DL_FUNC) &rmark_node_is_inline,         1 },
    { "rmark_node_is_leaf",           (DL_FUNC) &rmark_node_is_leaf,           1 },
    { "rmark_node_new",               (DL_FUNC) &rmark_node_new,               1 },
    { "rmark_node_next",              (DL_FUNC) &rmark_node_next,              1 },
    { "rmark_node_previous",          (DL_FUNC) &rmark_node_previous,          1 },
    { "rmark_node_parent",            (DL_FUNC) &rmark_node_parent,            1 },
    { "rmark_node_first_child",       (DL_FUNC) &rmark_node_first_child,       1 },
    { "rmark_node_last_child",        (DL_FUNC) &rmark_node_last_child,        1 },
    { "rmark_iterate",                (DL_FUNC) &rmark_iterate,                3 },
    { "rmark_node_get_type_string",   (DL_FUNC) &rmark_node_get_type_string,   1 },
    { "rmark_node_get_literal",       (DL_FUNC) &rmark_node_get_literal,       1 },
    { "rmark_node_set_literal",       (DL_FUNC) &rmark_node_set_literal,       2 },
    { "rmark_node_get_heading_level", (DL_FUNC) &rmark_node_get_heading_level, 1 },
    { "rmark_node_set_heading_level", (DL_FUNC) &rmark_node_set_heading_level, 2 },
    { "rmark_node_get_list_type",     (DL_FUNC) &rmark_node_get_list_type,     1 },
    { "rmark_node_set_list_type",     (DL_FUNC) &rmark_node_set_list_type,     2 },
    { "rmark_node_get_list_delim",    (DL_FUNC) &rmark_node_get_list_delim,    1 },
    { "rmark_node_set_list_delim",    (DL_FUNC) &rmark_node_set_list_delim,    2 },
    { "rmark_node_get_list_start",    (DL_FUNC) &rmark_node_get_list_start,    1 },
    { "rmark_node_set_list_start",    (DL_FUNC) &rmark_node_set_list_start,    2 },
    { "rmark_node_get_list_tight",    (DL_FUNC) &rmark_node_get_list_tight,    1 },
    { "rmark_node_set_list_tight",    (DL_FUNC) &rmark_node_set_list_tight,    2 },
    { "rmark_node_get_fence_info",    (DL_FUNC) &rmark_node_get_fence_info,    1 },
    { "rmark_node_set_fence_info",    (DL_FUNC) &rmark_node_set_fence_info,    2 },
    { "rmark_node_get_url",           (DL_FUNC) &rmark_node_get_url,           1 },
    { "rmark_node_set_url",           (DL_FUNC) &rmark_node_set_url,           2 },
    { "rmark_node_get_title",         (DL_FUNC) &rmark_node_get_title,         1 },
    { "rmark_node_set_title",         (DL_FUNC) &rmark_node_set_title,         2 },
    { "rmark_node_get_start_line",    (DL_FUNC) &rmark_node_get_start_line,    1 },
    { "rmark_node_get_start_column",  (DL_FUNC) &rmark_node_get_start_column,  1 },
    { "rmark_node_get_end_line",      (DL_FUNC) &rmark_node_get_end_line,      1 },
    { "rmark_node_get_end_column",    (DL_FUNC) &rmark_node_get_end_column,    1 },
    { "rmark_node_unlink",            (DL_FUNC) &rmark_node_unlink,            1 },
    { "rmark_node_insert_before",     (DL_FUNC) &rmark_node_insert_before,     2 },
    { "rmark_node_insert_after",      (DL_FUNC) &rmark_node_insert_after,      2 },
    { "rmark_node_replace",           (DL_FUNC) &rmark_node_replace,           2 },
    { "rmark_node_prepend_child",     (DL_FUNC) &rmark_node_prepend_child,     2 },
    { "rmark_node_append_child",      (DL_FUNC) &rmark_node_append_child,      2 },
};

attribute_visible void R_init_rmark(DllInfo *dll_info) {
    rmark_root_symbol = Rf_install("rmark_root");
    rmark_node_symbol = Rf_install("rmark_node");
    R_registerRoutines(dll_info, NULL, call_method_defs, NULL, NULL);
}
