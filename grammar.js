/**
 * @file FASM grammar for tree-sitter
 * @author Amaan Qureshi <contact@amaanq.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

export default grammar({
  name: 'fasm',

  extras: $ => [
    /\s/,
    $.comment,
  ],

  conflicts: $ => [
    [$.calm_command, $.assembly_directive],
    [$.macro_directive, $.calm_command],
    [$.primary_expression, $.token],
    [$.calm_command, $.control_directive],
  ],

  supertypes: $ => [
    $.line,
    $.instruction,
    $.primary_expression,
    $.symbol_identifier,
    $.identifier,
    $.token,
    $.data_value,
  ],

  rules: {
    source_file: $ => repeat($.line),

    line: $ => prec(1, choice(
      $.instruction,
      $.labeled_instruction,
      $.label_definition,
      $.area_label_definition,
      $.symbol_definition,
    )),

    comment: _ => token(seq(';', /.*/)),

    identifier: $ => choice(
      $.simple_identifier,
      $.case_insensitive_identifier,
      $.forced_identifier,
      $.repeat_parameter,
    ),

    simple_identifier: _ => /[a-zA-Z_][a-zA-Z0-9_]*/,

    case_insensitive_identifier: _ => /[a-zA-Z_][a-zA-Z0-9_]*\?/,

    forced_identifier: _ => /\?[a-zA-Z0-9_]+/,

    repeat_parameter: _ => token(choice('%', '%%')),

    symbol_identifier: $ => prec.right(choice(
      $.identifier,
      $.namespaced_identifier,
      $.relative_identifier,
      $.macro_concatenation,
    )),

    namespaced_identifier: $ => prec.right(seq(
      $.identifier,
      repeat1(seq(
        token.immediate('.'),
        choice($.identifier, $.number),
      )),
    )),

    relative_identifier: _ => seq('.', /[a-zA-Z_][a-zA-Z0-9_]*/),

    number: _ => {
      const decimal = /\d+[dD]?/;
      const hex = /\$[0-9a-fA-F]+|0x[0-9a-fA-F]+|[0-9a-fA-F]+[hH]/;
      const binary = /[01]+[bB]/;
      const octal = /[0-7]+[oqOQ]/;
      const float = /\d*\.\d+([eE][+-]?\d+)?[fF]?|\d+[eE][+-]?\d+[fF]?|\d+[fF]/;

      return token(choice(
        float,
        hex,
        binary,
        octal,
        decimal,
      ));
    },

    string: $ => choice(
      seq(
        '\'',
        alias(repeat(choice(/[^']/, '\'\'')), $.string_content),
        '\'',
      ),
      seq(
        '"',
        alias(repeat(choice(/[^"]/, '""')), $.string_content),
        '"',
      ),
    ),

    expression: $ => choice(
      $.primary_expression,
      $.binary_expression,
      $.unary_expression,
    ),

    primary_expression: $ => choice(
      $.number,
      $.string,
      $.symbol_identifier,
      $.current_address,
      $.base_address,
      $.file_offset,
      $.parenthesized_expression,
    ),

    current_address: _ => '$',
    base_address: _ => '$$',
    file_offset: _ => choice('$%', '$%%'),

    parenthesized_expression: $ => seq('(', $.expression, ')'),

    binary_expression: $ => choice(
      prec.left(1, seq($.expression, '|', $.expression)),
      prec.left(2, seq($.expression, '&', $.expression)),
      prec.left(3, seq($.expression, choice('=', '<', '>', '<=', '>=', '<>', 'relativeto', 'eqtype', 'eq'), $.expression)),
      prec.left(3, seq($.expression, choice('xor', 'and', 'or'), $.expression)),
      prec.left(4, seq($.expression, choice('shl', 'shr', 'bswap'), $.expression)),
      prec.left(5, seq($.expression, choice('+', '-'), $.expression)),
      prec.left(6, seq($.expression, choice('*', '/', 'mod'), $.expression)),
    ),

    unary_expression: $ => choice(
      prec(7, seq(choice('+', '-', 'not', 'bsf', 'bsr'), $.expression)),
      prec(7, seq('sizeof', $.expression)),
      prec(7, seq('lengthof', $.expression)),
      prec(7, seq('float', $.expression)),
      prec(7, seq('trunc', $.expression)),
      prec(7, seq('string', $.expression)),
      prec(8, seq('~', $.expression)),
      prec(8, seq('`', $.symbol_identifier)),
    ),

    logical_expression: $ => choice(
      $.expression,
      $.logical_operator_expression,
    ),

    logical_operator_expression: $ => choice(
      prec.right(seq('defined', optional($.expression))),
      seq('definite', $.expression),
      seq('used', $.symbol_identifier),
    ),

    symbol_definition: $ => choice(
      $.variable_definition,
      $.constant_definition,
      $.stacked_definition,
      $.symbolic_definition,
    ),

    variable_definition: $ => seq(
      $.symbol_identifier,
      '=',
      $.expression,
      /\n/,
    ),

    constant_definition: $ => seq(
      $.symbol_identifier,
      ':=',
      $.expression,
      /\n/,
    ),

    stacked_definition: $ => seq(
      $.symbol_identifier,
      '=:',
      $.expression,
      /\n/,
    ),

    symbolic_definition: $ => seq(
      $.symbol_identifier,
      choice('equ', 'reequ'),
      optional($.tokens),
      /\n/,
    ),

    label_definition: $ => choice(
      seq(field('label', $.symbol_identifier), ':'),
      seq(
        'label',
        field('label', $.symbol_identifier),
        optional(seq(':', $.expression)),
        optional(seq('at', $.expression)),
      ),
    ),

    area_label_definition: $ => seq(
      field('label', $.symbol_identifier),
      '::',
    ),

    instruction: $ => choice(
      $.data_directive,
      $.control_directive,
      $.macro_directive,
      $.calm_directive,
      $.assembly_directive,
      $.output_directive,
      $.simple_instruction,
    ),

    labeled_instruction: $ => seq($.symbol_identifier, $.instruction),

    data_directive: $ => choice(
      seq(choice('db', 'dw', 'dd', 'dp', 'dq', 'dt', 'ddq', 'dqq', 'ddqq'), $.data_arguments),
      seq(choice('rb', 'rw', 'rd', 'rp', 'rq', 'rt', 'rdq', 'rqq', 'rdqq'), $.expression),
      seq('emit', $.expression, ':', $.data_arguments),
      seq('file', $.string, optional(seq(':', $.expression)), optional(seq(',', $.expression))),
    ),

    data_arguments: $ => sep1($.data_value, ','),

    data_value: $ => prec(1, choice(
      $.expression,
      $.string,
      '?',
      $.dup_expression,
    )),

    dup_expression: $ => seq(
      $.expression,
      'dup',
      choice(
        field('value', $.data_value),
        seq('(', $.data_arguments, ')'),
      ),
    ),

    control_directive: $ => choice(
      $.if_directive,
      $.repeat_directive,
      $.match_directive,
      $.postpone_directive,
    ),

    if_directive: $ => seq(
      'if',
      $.logical_expression,
      repeat($.line),
      repeat($.elseif_clause),
      optional($.else_clause),
      'end', 'if',
    ),

    elseif_clause: $ => prec.right(seq(
      'else', 'if',
      $.logical_expression,
      repeat($.line),
    )),

    else_clause: $ => seq(
      'else',
      repeat($.line),
    ),

    repeat_directive: $ => choice(
      seq(
        choice('repeat', 'rept'),
        $.expression,
        optional(seq(',', $.repeat_parameters)),
        repeat($.line),
        'end', choice('repeat', 'rept'),
      ),
      seq(
        'while',
        $.logical_expression,
        repeat($.line),
        'end', 'while',
      ),
      seq(
        choice('iterate', 'irp'),
        $.iterate_parameters,
        repeat($.line),
        'end', choice('iterate', 'irp'),
      ),
    ),

    repeat_parameters: $ => sep1($.parameter_definition, ','),

    parameter_definition: $ => seq(
      $.identifier,
      optional(seq(':', $.expression)),
    ),

    iterate_parameters: $ => seq(
      choice(
        $.identifier,
        seq('<', sep1($.identifier, ','), '>'),
      ),
      ',',
      sep1($.iterate_value, ','),
    ),

    iterate_value: $ => choice(
      $.expression,
      seq('<', optional($.tokens), '>'),
    ),

    match_directive: $ => seq(
      choice('match', 'rawmatch', 'rmatch'),
      $.match_pattern,
      ',',
      // optional($.tokens),
      repeat($.line),
      repeat($.else_match_clause),
      optional($.else_clause),
      'end', 'match',
    ),

    else_match_clause: $ => prec.right(seq(
      'else', 'match',
      $.match_pattern,
      ',',
      // optional($.tokens),
      repeat($.line),
    )),

    match_pattern: $ => prec.right(repeat1($.pattern_element)),

    pattern_element: $ => choice(
      $.identifier,
      $.case_insensitive_pattern,
      $.literal_pattern,
      $.special_char,
      $.string,
    ),

    case_insensitive_pattern: $ => seq($.identifier, '?'),

    literal_pattern: $ => seq(
      '=',
      choice($.identifier, $.special_char),
    ),

    special_char: _ => /[+\-/*=<>()\[\]\{\}:?!,|&~#`\\]/,

    postpone_directive: $ => seq(
      'postpone',
      optional('?'),
      choice(
        seq(repeat($.line), 'end', 'postpone'),
        seq(
          alias(choice('{', '\\{'), '{'),
          repeat($.line),
          alias(choice('}', '\\}'), '}'),
        ),
      ),
    ),

    macro_directive: $ => choice(
      $.macro_definition,
      $.struc_definition,
      $.purge_instruction,
      $.local_instruction,
    ),

    macro_definition: $ => seq(
      'macro',
      optional('!'),
      field('name', $.identifier),
      optional(':'),
      optional($.macro_parameters),
      choice(
        seq(repeat($.line), 'end', 'macro'),
        seq('{', repeat(choice($.line, $.macro_processing_directive)), '}'),
      ),
    ),

    macro_processing_directive: _ => choice(
      'forward',
      'reverse',
      'common',
    ),

    macro_concatenation: $ => prec.right(seq(
      choice($.identifier, '.', $.string, $.number),
      repeat1(seq('#', choice($.identifier, $.string, $.number))),
      optional(choice($.identifier, $.string, $.number)),
    )),

    struc_definition: $ => seq(
      'struc',
      optional(seq('(', $.identifier, ')')),
      optional('!'),
      field('name', $.identifier),
      optional($.macro_parameters),
      choice(
        seq(repeat($.line), 'end', 'struc'),
        seq('{', repeat($.line), '}'),
      ),
    ),

    macro_parameters: $ => seq(
      sep1(field('parameter', $._macro_parameter), ','),
      optional(seq(
        optional(','),
        '[',
        field('parameter', $.identifier),
        ']',
      )),
    ),

    _macro_parameter: $ => prec(1, seq(
      optional('&'),
      $.identifier,
      optional('?'),
      optional(choice(
        '*',
        seq(':', optional($.tokens), /\n/),
      )),
    )),

    purge_instruction: $ => seq(
      choice('purge', 'restruc'),
      sep1($.symbol_identifier, ','),
    ),

    local_instruction: $ => seq(
      'local',
      sep1($.identifier, ','),
    ),

    calm_directive: $ => choice($.calm_definition, $.calm_command),

    calm_definition: $ => seq(
      'calminstruction',
      optional(seq('(', $.identifier, ')')),
      optional('!'),
      $.identifier,
      optional($.macro_parameters),
      repeat($.calm_line),
      'end', 'calminstruction',
    ),

    calm_line: $ => choice($.calm_command, $.calm_label),

    calm_label: $ => seq(
      field('label', $.identifier),
      ':',
    ),

    calm_command: $ => choice(
      seq('assemble', $.symbol_identifier),
      $.match_directive,
      seq('arrange', $.symbol_identifier, ',', $.match_pattern),
      seq('compute', $.symbol_identifier, ',', $.expression),
      seq('check', $.logical_expression),
      seq('publish', optional(':'), $.symbol_identifier, optional(':'), ',', $.symbol_identifier),
      seq('transform', $.symbol_identifier, optional(seq(',', $.symbol_identifier))),
      seq('stringify', $.symbol_identifier),
      seq('take', optional($.symbol_identifier), ',', $.symbol_identifier),
      seq('taketext', optional($.symbol_identifier), ',', $.symbol_identifier),
      seq('call', field('name', $.symbol_identifier), repeat(seq(',', $.symbol_identifier))),
      $.local_instruction,
      seq(choice('jump', 'jyes', 'jno'), $.identifier),
      'exit',
      seq('display', $.expression),
      seq('err', $.expression),
      seq('emit', $.expression, optional(seq(',', $.expression))),
      seq('load', $.symbol_identifier, ',', $.expression, ',', $.expression),
      seq('store', $.expression, ',', $.expression, ',', $.expression),
    ),

    match_options: _ => choice(/[()\[\]\{\}]/, /./),

    assembly_directive: $ => choice(
      $.include_directive,
      seq('eval', sep1($.expression, ',')),
      seq('display', sep1($.expression, ',')),
      seq('err', optional($.tokens), /\n/),
      seq('assert', $.logical_expression),
      seq('org', $.expression),
      seq('section', $.expression),
      prec.right(seq('restartout', optional($.expression))),
      seq('format', $.identifier, optional(seq('as', $.string))),
      seq('define', $.symbol_identifier, optional($.tokens), /\n/),
      seq('redefine', $.symbol_identifier, optional($.tokens), /\n/),
      seq('element', $.symbol_identifier, optional(seq(':', $.expression))),
      seq('restore', sep1($.symbol_identifier, ',')),
    ),

    include_directive: $ => seq(
      'include',
      optional('!'),
      $.string,
      optional(seq(',', optional($.tokens))),
      /\n/,
    ),

    output_directive: $ => choice(
      $.virtual_block,
      $.namespace_block,
      $.load_instruction,
      $.store_instruction,
    ),

    virtual_block: $ => seq(
      'virtual',
      optional(choice(
        seq('at', $.expression),
        seq('as', $.string),
        seq('at', $.expression, 'as', $.string),
        $.symbol_identifier,
      )),
      repeat($.line),
      'end', 'virtual',
    ),

    namespace_block: $ => seq(
      'namespace',
      $.symbol_identifier,
      repeat($.line),
      'end', 'namespace',
    ),

    load_instruction: $ => seq(
      'load',
      $.symbol_identifier,
      optional(seq(':', $.identifier)),
      'from',
      choice(
        $.expression,
        seq($.symbol_identifier, ':', $.expression),
        seq(':', $.expression),
      ),
    ),

    store_instruction: $ => seq(
      'store',
      $.expression,
      optional(seq(':', $.identifier)),
      'at',
      choice(
        $.expression,
        seq($.symbol_identifier, ':', $.expression),
        seq(':', $.expression),
      ),
    ),

    simple_instruction: $ => seq(
      choice($.identifier, 'break'),
      repeat(choice($.token, $.expression)),
      /\n/,
    ),

    tokens: $ => repeat1($.token),

    token: $ => prec.right(choice(
      $.symbol_identifier,
      $.number,
      $.string,
      $.special_char,
    )),
  },
});

/**
 *
 * @param {RuleOrLiteral} rule
 * @param {RuleOrLiteral} separator
 */
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}
