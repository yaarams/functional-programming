import { Parser as P, ParsingResult } from "./parserCombinator";

// ---------------- //
// Parsed Structure //
// ---------------- //

export interface LiteralValue {
  type: "literal";
  value: string;
}

export interface ListValue {
  type: "list";
  name: string;
}

export type Value = LiteralValue | ListValue;

export interface WordConstraint {
  type: "word";
  alternatives: Array<Value>;
}

export interface TagConstraint {
  type: "tag";
  alternatives: Array<Value>;
}

export interface LemmaConstraint {
  type: "lemma";
  alternatives: Array<Value>;
}

export interface EntityConstraint {
  type: "entity";
  alternatives: Array<Value>;
}

type Constraint = WordConstraint | TagConstraint | LemmaConstraint | EntityConstraint;
type ConstraintType = Constraint["type"]

export interface Token {
  type: "token";
  word: string;
}

export interface Anchor {
  type: "anchor";
  word: string;
  constraints: Array<Constraint>;
}

export interface Capture {
  type: "capture";
  word: string;
  constraints: Array<Constraint>;
  name?: string;
  expand?: string;
}

export type SearchTerm = Token | Anchor | Capture;

export interface SpikeQuery {
  terms: Array<SearchTerm>;
}

// convineice builders
const buildLiteralValue = (value: string): LiteralValue => ({ type: "literal", value });
const buildListValue = (name: string): ListValue => ({ type: "list", name });
const buildConstraint = (type: ConstraintType, alternatives: Array<Value>): Constraint => ({ type, alternatives });

// ----------------- //
// Parser Definition //
// ----------------- //

// some general parsers
const whitespace = P.regex(/\s+/).desc("white space");
const escapedValue = P.regex(/[^`]*/).surroundedBy("`");

// name of a field (including the shorthand aliases)
const fieldName = P.alternatives(
  P.str("word"),
  P.str("w").result("word"),
  P.str("lemma"),
  P.str("l").result("lemma"),
  P.str("tag"),
  P.str("t").result("tag"),
  P.str("entity"),
  P.str("e").result("entity"),
).desc("field name");

// constrinat values, for example:
// abc
// `multiple words`
// {list_name}
const escapedConstraintValue = escapedValue.map(buildLiteralValue);
const unescapedConstraintValue = P.regex(/[a-zA-Z0-9-,#.;':()"_°]+/).map(buildLiteralValue);
const listConstraintValue = P.regex(/\w+/).surroundedBy("{", "}").map(buildListValue);

const constraintValue = P.alternatives<Value>(
  escapedConstraintValue,
  listConstraintValue,
  unescapedConstraintValue,
).desc("constraint value");

// single constraint values combined into an OR expression, for example:
// abc|`two words`
// {list_name}|abc
const constraintsOrExpression = constraintValue.oneOrMoreTimes({ delimiter: "|" }).desc("constraints or expression");

// single field expression, for example:
// w=founded
// entity=CITY|LOCATION
// tag                 <-- implicit constraint (derived from sentence), no value is specified
const explicitFieldExpression = P.map2(fieldName.skip(P.str("=")), constraintsOrExpression, buildConstraint);
const implicitFieldExpression = fieldName.map((fn) => buildConstraint(fn, []));

const singleFieldExpression = P.alternatives(
  explicitFieldExpression,
  implicitFieldExpression,
).desc("field constraints");

// multiple field expression combined into an AND expression, for example:
// w=foo&t={list_name}|`long name`
const multipleFiledConstraintsExpression = singleFieldExpression.oneOrMoreTimes(
  { delimiter: "&" },
).desc("fields AND expression");
const tokenConstraintsExpression = multipleFiledConstraintsExpression.surroundedBy("[", "]").desc("token constratins");

const numberToken = P.regex(/-?\d+(\.\d+)?/);
const unescapedToken = P.regex(/[^\s[\]<>:$?][a-zA-Z-0-9|&]*/);

// a single token (not an anchor and not a capture)
const token = P.alternatives(escapedValue, numberToken, unescapedToken).map<Token>((word) => ({ word, type: "token" }));
// single anchor token
const anchor = P.sequence(
  P.str("$"),
  tokenConstraintsExpression.recoverWith([]), // if no token constraints change the result to empty list
  token,
).map<Anchor>(
  ([_, constraints, t]) => ({ ...t, constraints, type: "anchor" }),
);

const captureName = P.regex(/\w+/);
const expansion = P.regex(/\w*/).surroundedBy("<", ">");

// single capture token
const capture = P.sequence(
  expansion.optional(),
  captureName.optional(),
  P.str(":"),
  tokenConstraintsExpression.recoverWith([]), // if no token constraints change the result to empty list
  token,
).map<Capture>(
  ([expand, name, _, constraints, t]) => ({
    ...t, constraints, name, expand, type: "capture",
  }),
);

// we're willing to accept standalone symbols of :, $ and ? without the special escape syntax with backticks
// we ensure they are standalone by saying they must be followed by either space or end of input
const specialSingleSymbolToken = P.alternatives(
  P.str(":"), P.str("$"), P.str("?"),
).skip(whitespace.or(P.EOF)).map<Token>((word) => ({ word, type: "token" }));

const searchTerm = P.alternatives<SearchTerm>(capture, anchor, token, specialSingleSymbolToken).desc("search term");

// the full parser for the structured query language
// the optional whitespace delimiter is intentional and it helps in separation of it's into it and 's
// (all the search terms are defined in a way where this works)
const spikeQuery = searchTerm.zeroOrMoreTimes({ delimiterParser: whitespace.optional() }).map((terms) => ({ terms }));

export function parseStructuralQuery(queryString: string): ParsingResult<SpikeQuery> {
  return spikeQuery.parse(queryString);
}
