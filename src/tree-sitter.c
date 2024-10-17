#define R_NO_REMAP
#include "R.h"
#include "Rinternals.h"
#include "cleancall.h"

#include "tree_sitter/api.h"
extern const TSLanguage* tree_sitter_r(void);

static void r_free(void *data) {
  free(data);
}

SEXP s_expr(SEXP input) {
  const TSLanguage *rlang = NULL;
  TSParser *parser = NULL;

  rlang = tree_sitter_r();
  r_call_on_exit((cleanup_fn_t) ts_language_delete, (cleanup_data_t) rlang);
  parser = ts_parser_new();
  if (!ts_parser_set_language(parser, rlang)) {
    Rf_error("Failed to set R language, internal error.");
  }
  r_call_on_exit((cleanup_fn_t) ts_parser_delete, parser);

  const char *c_input = (const char*) RAW(input);
  uint32_t length = Rf_length(input);
  TSTree *tree = ts_parser_parse_string(parser, NULL, c_input, length);
  r_call_on_exit((cleanup_fn_t) ts_tree_delete, tree);
  TSNode root = ts_tree_root_node(tree);
  char *code = ts_node_string(root);

  SEXP result = Rf_mkString(code);
  r_free(code);
  return result;
}

typedef enum {
  EQ,
  NOT_EQ,
  ANY_EQ,
  ANY_NOT_EQ,
  MATCH,
  NOT_MATCH,
  ANY_MATCH,
  ANY_NOT_MATCH,
  ANY_OF,
  NOT_ANY_OF
} predicate_type;

bool check_predicates(const TSQuery *query, const TSQueryMatch *match,
                      uint32_t pattern_index,
                      const TSQueryPredicateStep *preds,
                      uint32_t num_steps, const char *text,
                      uint32_t length, uint32_t *capture_map,
                      uint32_t *capture_map_pattern) {

  for (uint32_t st = 0; st < num_steps; st++) {

    // Operation, like #eq? etc. ------------------------------------------
    if (preds[st].type != TSQueryPredicateStepTypeString) {
      Rf_error("First predicate must be a string");
    }
    uint32_t l;
    const char *ops = ts_query_string_value_for_id(
      query,
      preds[st].value_id,
      &l
    );
    st++;
    predicate_type op;
    if (strcasecmp("eq?", ops)) {
      op = EQ;
    } else if (strcasecmp("not-eq?", ops)) {
      op = NOT_EQ;
    } else if (strcasecmp("any-eq?", ops)) {
      op = ANY_EQ;
    } else if (strcasecmp("any-not-eq?", ops)) {
      op = ANY_NOT_EQ;
    } else if (strcasecmp("match?", ops)) {
      op = MATCH;
    } else if (strcasecmp("not-match?", ops)) {
      op = NOT_MATCH;
    } else if (strcasecmp("any-match?", ops)) {
      op = ANY_MATCH;
    } else if (strcasecmp("any-not-match", ops)) {
      op = ANY_NOT_MATCH;
    } else if (strcasecmp("any-of?", ops)) {
      op = ANY_OF;
    } else if (strcasecmp("not-any-of?", ops)) {
      op = NOT_ANY_OF;
    } else {
      Rf_error("Unknown predicate: #%s", ops);
    }

    // First argument must be a capture. ----------------------------------
    // Possibly 1-n nodes
    uint32_t first_id = preds[st].value_id;
    uint32_t first_quant = ts_query_capture_quantifier_for_id(
      query, pattern_index, first_id);
    if (capture_map_pattern[first_id] != pattern_index + 1) {
      REprintf("!MATCHING NOTHING!\n");
    }
    uint32_t first_idx = capture_map[first_id];
    TSNode first_node = match->captures[first_idx].node;
    uint32_t first_start = ts_node_start_byte(first_node);
    uint32_t first_length = ts_node_end_byte(first_node) - first_start;
    // check the nodes that were captured with this one
    uint32_t first_nodes_count = 1;
    while (first_idx > 0) {
      first_idx--;
      if (match->captures[first_idx].index != first_id) break;
      first_nodes_count++;
    }
    REprintf("found %u first nodes\n", first_nodes_count);
    st++;

    // second argument
    if (preds[st].type == TSQueryPredicateStepTypeCapture) {

    } else if (preds[st].type == TSQueryPredicateStepTypeString) {

    } else if (preds[st].type == TSQueryPredicateStepTypeDone) {
        if (!strcasecmp("any-of?", ops)) {
          return false;
        } else if (!strcasecmp("not-any-of?", ops)) {
          // this predicate is ok
          continue;
        } else {
          Rf_error("Second argument missing for #%s predicate", ops);
        }
    }

    // now compare the rest to these
    while (preds[st].type != TSQueryPredicateStepTypeDone) {
      switch (preds[st].type) {
        case TSQueryPredicateStepTypeCapture: {
          uint32_t next_id = preds[st].value_id;
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
          if (capture_map_pattern[next_id] != pattern_index + 1) {
            REprintf("!MATCHING NOTHING!\n");
          }
          uint32_t next_idx = capture_map[next_id];
          TSNode next_node = match->captures[next_idx].node;
          uint32_t next_start = ts_node_start_byte(next_node);
          uint32_t next_length = ts_node_end_byte(next_node) - next_start;
          // TODO: this capture may match multiple nodes, if quantified
          uint32_t next_nodes_count = 1;
          while (next_idx > 0) {
            next_idx--;
            if (match->captures[next_idx].index != next_id) break;
            next_nodes_count++;
          }
          REprintf("found %u next nodes\n", next_nodes_count);
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
  uint32_t *capture_map = malloc(sizeof(uint32_t) * capture_count);
  if (!capture_map) {
    Rf_error("Out of memory");
  }
  r_call_on_exit(r_free, capture_map);
  uint32_t *capture_map_pattern = malloc(sizeof(uint32_t) * capture_count);
  if (!capture_map_pattern) {
    Rf_error("Out of memory");
  }
  r_call_on_exit(r_free, capture_map_pattern);
  memset(capture_map_pattern, 0, sizeof(uint32_t) * capture_count);

  const char *c_input = (const char*) RAW(input);
  uint32_t length = Rf_length(input);
  TSTree *tree = ts_parser_parse_string(parser, NULL, c_input, length);
  r_call_on_exit((cleanup_fn_t) ts_tree_delete, tree);
  TSNode root = ts_tree_root_node(tree);

  uint32_t pattern_count = ts_query_pattern_count(query);
  SEXP result_matches = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result_matches, 0, Rf_allocVector(STRSXP, pattern_count));
  SET_VECTOR_ELT(result_matches, 1, Rf_allocVector(INTSXP, pattern_count));
  for (uint32_t i = 0; i < pattern_count; i++) {
    uint32_t start = ts_query_start_byte_for_pattern(query, i);
    uint32_t end = ts_query_end_byte_for_pattern(query, i);
    SET_STRING_ELT(
      VECTOR_ELT(result_matches, 0), i,
      Rf_mkCharLenCE(cpattern + start, end - start, CE_UTF8)
    );
  }
  memset(
    INTEGER(VECTOR_ELT(result_matches, 1)),
    0,
    sizeof(int) * pattern_count
  );

  // TODO: we should allocate a DF here, probably
  PROTECT_INDEX rpi;
  SEXP result_captures = Rf_allocVector(VECSXP, 100);
  PROTECT_WITH_INDEX(result_captures, &rpi);
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
    REprintf("Potential match, pattern %u (%u captures)\n", match.pattern_index, match.capture_count);
    for (uint16_t cc = 0; cc < match.capture_count; cc++) {
      uint32_t cidx = match.captures[cc].index;
      capture_map_pattern[cidx] = match.pattern_index + 1;
      capture_map[cidx] = cc;
    }

    // evaluate the predicates
    const TSQueryPredicateStep *mpreds = preds[match.pattern_index];
    uint32_t mnum_steps = num_steps[match.pattern_index];
    if (!check_predicates(
         query, &match, match.pattern_index, mpreds, mnum_steps,
         c_input, length, capture_map, capture_map_pattern)) {
      continue;
    }

    match_idx++;
    INTEGER(VECTOR_ELT(result_matches, 1))[match.pattern_index] += 1;
    total_capture_count += match.capture_count;
    if (total_capture_count > Rf_length(result_captures)) {
      REPROTECT(result_captures = Rf_xlengthgets(result_captures, total_capture_count * 2), rpi);
    }

    // collect the results
    for (uint16_t cc = 0; cc < match.capture_count; cc++) {
      SEXP res1 = PROTECT(Rf_allocVector(VECSXP, 6));
      SET_VECTOR_ELT(result_captures, residx++, res1);
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

  REPROTECT(result_captures = Rf_xlengthgets(result_captures, total_capture_count), rpi);
  SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 0, result_matches);
  SET_VECTOR_ELT(result, 1, result_captures);
  UNPROTECT(3);
  return result;
}
