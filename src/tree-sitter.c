#define R_NO_REMAP
#include "R.h"
#include "Rinternals.h"
#include "cleancall.h"

#include "tree_sitter/api.h"
extern const TSLanguage* tree_sitter_r(void);

static void r_free(void *data) {
  free(data);
}

bool pred_ok_eq(const TSQuery *query, const TSQueryMatch *match,
                uint32_t pattern_index, const TSQueryPredicateStep *preds,
                const char *text, uint32_t length,
                uint32_t *capture_map) {
  // first one is a capture, get that
  if (preds->type != TSQueryPredicateStepTypeCapture) {
    Rf_error("First argument of an #eq? predicate must be a capture");
  }
  uint32_t first_id = preds->value_id;
  uint32_t first_idx = capture_map[first_id];
  TSNode first_node = match->captures[first_idx].node;
  uint32_t first_start = ts_node_start_byte(first_node);
  uint32_t first_length = ts_node_end_byte(first_node) - first_start;
  // TODO: this capture may match multiple nodes, if quantified
  preds++;

  // now compare the rest to that
  while (preds->type != TSQueryPredicateStepTypeDone) {
    switch (preds->type) {
      case TSQueryPredicateStepTypeCapture: {
        uint32_t next_id = preds->value_id;
        uint32_t cnl;
        const char *cn = ts_query_capture_name_for_id(
          query,
          next_id,
          &cnl
        );
        TSQuantifier next_quant = ts_query_capture_quantifier_for_id(
          query,
          pattern_index,
          next_id
        );
        REprintf("CAPTURE %u (%u): %s\n", next_id, next_quant, cn);
        uint32_t next_idx = capture_map[next_id];
        TSNode next_node = match->captures[next_idx].node;
        uint32_t next_start = ts_node_start_byte(next_node);
        uint32_t next_length = ts_node_end_byte(next_node) - next_start;
        // TODO: this capture may match multiple nodes, if quantified
        if (first_length != next_length) {
          return false;
        }
        if (strncmp(text + first_start, text + next_start, first_length)) {
          return false;
        }
        break;
      }
      case TSQueryPredicateStepTypeString: {
        uint32_t next_length;
        const char *str = ts_query_string_value_for_id(
          query,
          preds->value_id,
          &next_length
        );
        if (first_length != next_length) {
          return false;
        }
        if (strncmp(text + first_start, str, first_length)) {
          return false;
        }
        break;
      }
      default:
        Rf_error("Unknown predicate step, this should not happen");
        break;
    }
    preds++;
  }

  return true;
}

bool pred_ok_not_eq(const TSQuery *query, const TSQueryMatch *match,
                    const TSQueryPredicateStep *preds,
                    const char *text, uint32_t length,
                    uint32_t *capture_map) {
  if (preds->type != TSQueryPredicateStepTypeCapture) {
    Rf_error("First argument of an #eq? predicate must be a capture");
  }
  return true;
}

bool pred_ok_any_eq(const TSQuery *query, const TSQueryMatch *match,
                    const TSQueryPredicateStep *preds,
                    const char *text, uint32_t length,
                    uint32_t *capture_map) {
  if (preds[0].type != TSQueryPredicateStepTypeCapture) {
    Rf_error("First argument of an #eq? predicate must be a capture");
  }
  return true;
}

bool pred_ok_any_not_eq(const TSQuery *query, const TSQueryMatch *match,
                        const TSQueryPredicateStep *preds,
                        const char *text, uint32_t length,
                        uint32_t *capture_map) {
  if (preds[0].type != TSQueryPredicateStepTypeCapture) {
    Rf_error("First argument of an #eq? predicate must be a capture");
  }
  return true;
}

bool pred_ok(const TSQuery *query, const TSQueryMatch *match,
             uint32_t pattern_index, const TSQueryPredicateStep *preds,
             uint32_t num_steps, const char *text, uint32_t length,
             uint32_t *capture_map) {
  for (uint32_t st = 0; st < num_steps; st++) {
    if (preds[st].type != TSQueryPredicateStepTypeString) {
      Rf_error("First predicate must be a string");
    }
    uint32_t l;
    const char *op = ts_query_string_value_for_id(
      query,
      preds[st].value_id,
      &l
    );
    st++;

    if (!strncasecmp(op, "eq?", l)) {
      if (!pred_ok_eq(query, match, pattern_index, preds + st, text, length,
                      capture_map)) {
        return false;
      }
    } else if (!strncasecmp(op, "not-eq?", l)) {
      if (!pred_ok_not_eq(query, match, preds+st, text, length,
                          capture_map)) {
        return false;
      }
    } else if (!strncasecmp(op, "any-eq?", l)) {
      if (!pred_ok_any_eq(query, match, preds+st, text, length,
                          capture_map)) {
        return false;
      }
    } else if (!strncasecmp(op, "any-not-eq?", l)) {
      if (!pred_ok_any_not_eq(query, match, preds+st, text, length,
                              capture_map)) {
        return false;
      }
    } else {
      Rf_error("Predicate not implemented: %s", op);
    }
    while (st < num_steps &&
           preds[st].type != TSQueryPredicateStepTypeDone) {
      st++;
    }
  }

  return true;
}

SEXP code_query(SEXP input, SEXP pattern) {
  const TSLanguage *rlang = NULL;
  TSParser *parser = NULL;

  rlang = tree_sitter_r();
  r_call_on_exit((cleanup_fn_t) ts_language_delete, (cleanup_data_t) rlang);
  parser = ts_parser_new();
  if (!ts_parser_set_language(parser, rlang)) {
    Rf_error("Failed to set R language, internal error.");
  }
  r_call_on_exit((cleanup_fn_t) ts_parser_delete, parser);

  const char *cpattern = CHAR(STRING_ELT(pattern, 0));
  uint32_t error_offset;
  TSQueryError error_type;
  TSQuery *query = ts_query_new(
    rlang,
    cpattern,
    strlen(cpattern),
    &error_offset,
    &error_type
  );
  if (!query) {
    Rf_error("Failed to parse TS query at char %d.", (int) error_offset);
  }
  r_call_on_exit((cleanup_fn_t) ts_query_delete, query);

  uint32_t num_patterns = ts_query_pattern_count(query);
  const TSQueryPredicateStep **preds =
    malloc(sizeof(TSQueryPredicateStep*) * num_patterns);
  if (!preds) {
    Rf_error("Out of memory");
  }
  r_call_on_exit(r_free, preds);
  uint32_t *num_steps = malloc(sizeof(uint32_t) * num_patterns);
  for (uint32_t pt = 0; pt < num_patterns; pt++) {
    preds[pt] = ts_query_predicates_for_pattern(query, pt, num_steps + pt);
  }

  uint32_t capture_count = ts_query_capture_count(query);
  uint32_t *capture_map = malloc(sizeof(uint32_t) *capture_count);
  if (!capture_map) {
    Rf_error("Out of memory");
  }
  r_call_on_exit(r_free, capture_map);

  const char *c_input = (const char*) RAW(input);
  uint32_t length = Rf_length(input);
  TSTree *tree = ts_parser_parse_string(parser, NULL, c_input, length);
  r_call_on_exit((cleanup_fn_t) ts_tree_delete, tree);
  TSNode root = ts_tree_root_node(tree);

  // TODO: we should allocate a DF here, probably
  PROTECT_INDEX rpi;
  SEXP result = Rf_allocVector(VECSXP, 100);
  PROTECT_WITH_INDEX(result, &rpi);
  uint32_t total_capture_count = 0, residx = 0;

  TSQueryCursor *cursor = ts_query_cursor_new();
  ts_query_cursor_exec(cursor, query, root);
  r_call_on_exit((cleanup_fn_t) ts_query_cursor_delete, cursor);
  TSQueryMatch match;
  uint32_t match_idx = 0;
  while (ts_query_cursor_next_match(cursor, &match)) {
    // Create a capture id -> capture_idx in match mapping
    // We point to the last node that has this capture id, and then we can
    // work backwards
    for (uint16_t cc = 0; cc < match.capture_count; cc++) {
      uint32_t cidx = match.captures[cc].index;
      capture_map[cidx] = cc;
    }

    // evaluate the predicates
    const TSQueryPredicateStep *mpreds = preds[match.pattern_index];
    uint32_t mnum_steps = num_steps[match.pattern_index];
    if (!pred_ok(query, &match, match.pattern_index, mpreds, mnum_steps,
                 c_input, length, capture_map)) {
      continue;
    }

    match_idx++;
    total_capture_count += match.capture_count;
    if (total_capture_count > Rf_length(result)) {
      REPROTECT(result = Rf_xlengthgets(result, total_capture_count * 2), rpi);
    }

    // collect the results
    for (uint16_t cc = 0; cc < match.capture_count; cc++) {
      SEXP res1 = PROTECT(Rf_allocVector(VECSXP, 6));
      SET_VECTOR_ELT(result, residx++, res1);
      UNPROTECT(1);

      SET_VECTOR_ELT(res1, 0, Rf_ScalarInteger(match.pattern_index + 1));
      SET_VECTOR_ELT(res1, 1, Rf_ScalarInteger(match_idx));
      SET_VECTOR_ELT(res1, 2, Rf_ScalarInteger(match.captures[cc].index + 1));

      uint32_t cnl;
      const char *cn = ts_query_capture_name_for_id(
        query,
        match.captures[cc].index,
        &cnl
      );
      SET_VECTOR_ELT(res1, 3, Rf_ScalarString(Rf_mkCharLenCE(
        cn,
        cnl,
        CE_UTF8
      )));

      TSNode node = match.captures[cc].node;
      uint32_t start_byte = ts_node_start_byte(node);
      uint32_t end_byte = ts_node_end_byte(node);
      SET_VECTOR_ELT(res1, 4, Rf_ScalarString(Rf_mkCharLenCE(
        c_input + start_byte,
        end_byte - start_byte,
        CE_UTF8
      )));
      SET_VECTOR_ELT(res1, 5, Rf_ScalarInteger(start_byte + 1));
    }
  }

  REPROTECT(result = Rf_xlengthgets(result, total_capture_count), rpi);
  UNPROTECT(1);
  return result;
}
