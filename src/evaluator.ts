import {
  Cons,
  KnownSymbol,
  List,
  Nominal,
  Sym,
  add,
  append,
  apply,
  bignump,
  booleanp,
  car,
  cdr,
  cons,
  consp,
  divide,
  eq,
  eql,
  error,
  fixnump,
  float,
  floatp,
  integerp,
  intern,
  isBool,
  isCons,
  isList,
  isSymbol,
  isVector,
  iterate,
  list,
  listStar,
  listp,
  makeSymbol,
  mostNegativeFixnum,
  mostPositiveFixnum,
  multiply,
  nil,
  nilp,
  not,
  numberp,
  numericEqual,
  print,
  setcar,
  setcdr,
  stringp,
  subtract,
  symbolName,
  symbolp,
  t,
  truncate,
  vector,
  vectorp,
} from "./primitives";

import {
  Expression,
  ExpressionKind,
  FunctionCall,
  GetVariable,
  If,
  Lambda,
  Literal,
  Progn,
  SetVariable,
  While,
  compile,
} from "./compiler";

import { generate } from "astring";

export interface Environment extends Nominal<"environment"> {
  [key: string]: unknown;
}

export function createEnvironment(): Environment {
  return Object.create(environmentPrototype);
}

export function evaluate(form: unknown, environment: Environment): unknown {
  const lispTree = parse(form);
  const [jsTree, literals] = compile(lispTree);
  const jsText = generate(jsTree);
  const func = new Function("$E", "$L", "$G", "$T", `"use strict";return (${jsText});`);
  return func(environment, literals, get, test);

  function get(key: string) {
    return key in environment ? environment[key] : notBoundError(key);
  }
}

/**
 * Parse a Lisp form into an AST for input to the compiler.
 */
export function parse(form: unknown): Expression {
  let boundVariables: BoundVariableSet | undefined = undefined;
  return parseForm(form);

  function parseForm(form: unknown): Expression {
    if (isCons(form)) {
      return parseConsForm(form);
    } else if (isSymbol(form)) {
      return parseGetVariable(form);
    } else {
      return parseLiteral(form);
    }
  }

  function parseConsForm(list: Cons) {
    const [sym, rest] = expectList(list, _);
    if (isSymbol(sym)) {
      switch (sym) {
        // special forms
        case KnownSymbol.Quote:
          return parseQuote(rest);
        case KnownSymbol.Setq:
          return parseSetq(rest);
        case KnownSymbol.If:
          return parseIf(rest);
        case KnownSymbol.While:
          return parseWhile(rest);
        case KnownSymbol.Lambda:
          return parseLambda(rest);
        case KnownSymbol.Progn:
          return parseProgn(rest);
        // "macros" currently implemented with hard-coded expansion in parser
        case KnownSymbol.Defun:
          return parseDefun(rest);
        case KnownSymbol.Let:
          return parseLet(rest);
        case KnownSymbol.Quasiquote:
          return parseQuasiquote(rest);
      }
    }

    return parseFunctionCall(list);
  }

  function parseGetVariable(sym: Sym): GetVariable | Literal {
    if (isBool(sym)) {
      return parseLiteral(sym);
    }

    return {
      kind: ExpressionKind.GetVariable,
      symbol: sym,
      isBound: isBound(sym),
    };
  }

  function parseQuote(rest: List) {
    const [value] = expectTuple(rest, _);
    return parseLiteral(value);
  }

  function parseQuasiquote(rest: List): Expression {
    const [form] = expectTuple(rest, _);
    const expansion = expandQuasiquote(form);
    return parseForm(expansion);
  }

  function parseSetq(rest: List): SetVariable {
    const [sym, value] = expectTuple(rest, isVariableSymbol, _);
    return {
      kind: ExpressionKind.SetVariable,
      symbol: sym,
      value: parseForm(value),
      isBound: isBound(sym),
    };
  }

  function parseIf(list: List): If {
    const [test, rest] = expectList(list, _);
    const [then, restRest] = expectList(rest, _);
    const [otherwise] = restRest ? expectTuple(restRest, _) : [nil];

    return {
      kind: ExpressionKind.If,
      test: parseForm(test),
      then: parseForm(then),
      otherwise: parseForm(otherwise),
    };
  }

  function parseWhile(rest: List): While {
    const [test, expressions] = expectList(rest, _);
    return {
      kind: ExpressionKind.While,
      test: parseForm(test),
      body: parseBody(expressions),
    };
  }

  function parseProgn(rest: List): Progn {
    return {
      kind: ExpressionKind.Progn,
      expressions: parseExpressionList(rest),
    };
  }

  function parseBody(list: List): Expression {
    if (list === nil) {
      return parseLiteral(nil);
    } else if (list.cdr === nil) {
      return parseForm(list.car);
    } else {
      return parseProgn(list);
    }
  }

  function parseLambda(list: List, name?: string): Lambda {
    const [parameterList, body] = expectList(list, isList);
    const parameters = expectArray(parameterList, isVariableSymbol);
    return parseLambdaBody(parameters, body, name);
  }

  function parseLambdaBody(parameters: symbol[], body: List, name?: string) {
    let lambda: Lambda;
    pushBoundVariables(parameters);
    try {
      lambda = {
        kind: ExpressionKind.Lambda,
        name,
        parameters,
        body: parseBody(body),
      };
    } finally {
      popBoundVariables();
    }
    return lambda;
  }

  function parseFunctionCall(list: List): FunctionCall {
    const [func, args] = expectList(list, _);
    return {
      kind: ExpressionKind.FunctionCall,
      function: parseForm(func),
      arguments: parseExpressionList(args),
    };
  }

  function parseLiteral(value: unknown): Literal {
    return { kind: ExpressionKind.Literal, value };
  }

  function parseLet(rest: List): FunctionCall {
    const [bindingList, body] = expectList(rest, isList);
    const bindings = expectArray(bindingList, isCons);
    const parameters = bindings.map(list => expect(list.car, isVariableSymbol));
    const args = bindings.map(list => expectTuple(expect(list.cdr, isList), _)[0]);
    const lambda = parseLambdaBody(parameters, body);

    return {
      kind: ExpressionKind.FunctionCall,
      function: lambda,
      arguments: args.map(parseForm),
    };
  }

  function parseDefun(rest: List): SetVariable {
    const [sym, lambda] = expectList(rest, isVariableSymbol);
    return {
      kind: ExpressionKind.SetVariable,
      symbol: sym,
      value: parseLambda(lambda, symbolName(sym)),
      isBound: false, // defun operates on global not lexical environment
    };
  }

  function parseExpressionList(list: List) {
    let expressions = [];

    for (const each of iterate(list)) {
      const expression = parseForm(each);
      expressions.push(expression);
    }

    return expressions;
  }

  function pushBoundVariables(variables: symbol[]) {
    boundVariables = new BoundVariableSet(variables, boundVariables);
  }

  function popBoundVariables() {
    boundVariables = boundVariables!.parent;
  }

  function isBound(variable: symbol) {
    for (let bv = boundVariables; bv !== undefined; bv = bv.parent) {
      if (bv.set.has(variable)) {
        return true;
      }
    }
    return false;
  }
}

/**
 * Expand macro form to equivalent code.
 *
 * (We don't first class macros yet so this is just hard-coded to expand
 * quasiquote or return unchanged form.)
 */
export function macroexpand(form: unknown) {
  if (!isCons(form) || form.car !== KnownSymbol.Quasiquote) {
    return form;
  }
  if (!isCons(form.cdr)) {
    throw error("Invalid quasiquote");
  }
  return expandQuasiquote(form.cdr.car);
}

export function compileToString(form: unknown) {
  const lispTree = parse(form);
  const [jsTree, literals] = compile(lispTree);
  const jsText = generate(jsTree);

  let comment = "\n";
  for (let i = 0; i < literals.length; i++) {
    comment += `// $L[${i}]: ${print(literals[i])}\n`;
  }

  return comment + jsText + "\n";
}

class BoundVariableSet {
  public readonly set: ReadonlySet<symbol>;
  constructor(symbols: readonly symbol[], public parent?: BoundVariableSet) {
    this.set = new Set<symbol>(symbols);
  }
}

/** A predicate that checks that a value is of the given type. */
type TypePredicate<Type = unknown> = (value: unknown) => value is Type;

/** The type checked by a TypePredicate */
type CheckedType<Predicate> = Predicate extends TypePredicate<infer Type> ? Type : never;

/** The tuple of types checked by a tuple of TypePredicates */
type CheckedTypes<Predicates extends TypePredicate[]> = {
  [Key in keyof Predicates]: CheckedType<Predicates[Key]>;
};

function expectList<T>(list: List, carType: TypePredicate<T>): [T, List] {
  if (list === nil) {
    throw error("Expected non-empty list.");
  }

  const car = expect(list.car, carType);
  const cdr = expect(list.cdr, isList);
  return [car, cdr];
}

function expectArray<T>(list: List, elementType: TypePredicate<T>) {
  let elements = [];

  for (const each of iterate(list)) {
    const e = expect(each, elementType);
    elements.push(e);
  }

  return elements;
}

function expectTuple<T extends TypePredicate[]>(list: List, ...elementTypes: T) {
  let elements = new Array<unknown>(elementTypes.length);
  let index = 0;

  for (const each of iterate(list)) {
    if (index === elementTypes.length) {
      throw error("Too many elements.");
    }
    elements[index] = expect(each, elementTypes[index]);
    index++;
  }

  if (index !== elementTypes.length) {
    throw error("Too few elements.");
  }

  return elements as CheckedTypes<T>;
}

/**
 * Expands `form, (read as (quasiquote form)) to equivalent code.
 *
 * This implementation attempts to follow the formal rules as directly as
 * possible. Modest simplification is performed on the fly to make the expansion
 * somewhat readable, but it's still sub-optimal at this point.
 *
 * Note that seemingly obvious simplifications can fail notoriously easily in
 * the presence of nested splicing. Significant testing will be needed before
 * any further simplification is attempted.
 *
 * References:
 * 1. Common Lisp HyperSpec: 2.4.6 Backquote
 *    https://www.lispworks.com/documentation/HyperSpec/Body/02_df.htm
 *
 * 2. Common Lisp the Language, 2nd Edition, Guy L. Steele Jr.
 *    https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node190.html#BACKQUOTE
 *    https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node367.html#SECTION003600000000000000000
 *
 * 3. Quasiquotation in Lisp, Alan Bawden
 *    https://www.brics.dk/NS/99/1/BRICS-NS-99-1.pdf
 */
function expandQuasiquote(form: unknown): unknown {
  if (isCons(form)) {
    if (isQuasiquote(form)) {
      const inner = expandQuasiquote(argumentOf(form));
      return expandQuasiquote(inner);
    } else if (isUnquote(form)) {
      return argumentOf(form);
    } else if (isUnquoteSplicing(form)) {
      throw error(",@ outside list.");
    } else {
      return expandQuasiquotedList(form);
    }
  } else if (isVector(form)) {
    const asList = list.apply(undefined, form);
    const expandedList = expandQuasiquote(asList);
    return list(KnownSymbol.Apply, KnownSymbol.Vector, expandedList);
  } else {
    return quote(form);
  }
}

function expandQuasiquotedList(form: Cons) {
  let expansion: unknown[] = [KnownSymbol.Append];
  let listInProgress: unknown[] | undefined = undefined;

  while (true) {
    const first = form.car;
    if (isCons(first)) {
      if (isUnquote(first)) {
        pushList(argumentOf(first));
      } else if (isUnquoteSplicing(first)) {
        push(argumentOf(first));
      } else {
        pushList(expandQuasiquote(first));
      }
    } else if (isUnquote(form)) {
      push(argumentOf(form));
      break;
    } else if (isUnquoteSplicing(form)) {
      throw error(",@ after .");
    } else {
      pushList(expandQuasiquote(first));
    }

    const rest = form.cdr;
    if (rest === nil) {
      break;
    } else if (!isCons(rest)) {
      push(quote(rest));
      break;
    }

    form = rest;
  }

  return finishList();

  /** Append form to the expansion in progress. */
  function push(form: any) {
    flushList();
    expansion.push(form);
  }

  /**
   * Logically append (list form) to the expansion in progress, but buffer it
   * so that we can simplify (append (list x) (list y)) to (append (list x y))
   * so long as neither x nor y are ,@ forms.
   */
  function pushList(form: any) {
    if (isUnquoteSplicing(form)) {
      push(list(KnownSymbol.List, form));
    } else if (listInProgress !== undefined) {
      listInProgress.push(form);
    } else {
      listInProgress = [KnownSymbol.List, form];
    }
  }

  /** Flush buffered pushList calls to the expansion. */
  function flushList() {
    if (listInProgress !== undefined) {
      expansion.push(list.apply(undefined, listInProgress));
      listInProgress = undefined;
    }
  }

  /**
   * Flush buffered pushList calls to the expansion and make a final
   * simplification of (append x) to x, provided x is not a ,@ form.
   */
  function finishList() {
    flushList();

    if (expansion.length === 2) {
      const x = expansion[1];
      if (!isCons(x) || !isUnquoteSplicing(x)) {
        return x;
      }
    }

    return list.apply(undefined, expansion);
  }
}

const environmentPrototype = (function () {
  const constants = {
    nil,
    t,
    "most-positive-fixnum": mostPositiveFixnum,
    "most-negative-fixnum": mostNegativeFixnum,
  };

  const env = {
    ...constants,
    append,
    apply,
    bignump,
    booleanp,
    car,
    cdr,
    cons,
    consp,
    error,
    fixnump,
    float,
    floatp,
    macroexpand,
    numberp,
    eq,
    eql,
    listp,
    not,
    integerp,
    intern,
    list,
    print,
    setcar,
    setcdr,
    stringp,
    symbolp,
    truncate,
    vector,
    vectorp,

    compile: compileToString,
    null: nilp,

    "+": add,
    "-": subtract,
    "/": divide,
    "*": multiply,
    "=": numericEqual,
    "list*": listStar,
    "make-symbol": makeSymbol,
    "symbol-name": symbolName,
    "to-string": (x: any) => x.toString(),
    "to-json": (x: any) => JSON.stringify(x),
  };

  for (const key in constants) {
    Object.defineProperty(env, key, { writable: false });
  }

  return env;
})();

function expect<T>(datum: unknown, typep: TypePredicate<T>) {
  if (!typep(datum)) {
    throw error(`Expected ${typep.name}.`);
  }

  return datum;
}

function _(_value: unknown): _value is unknown {
  return true;
}

function isVariableSymbol(value: unknown): value is symbol {
  if (isBool(value)) {
    error(`${value === t ? "t" : "nil"} cannot be used as a variable.`);
  }
  return typeof value === "symbol";
}

/** Determine if form is `x (read as (quasiquote x)) */
function isQuasiquote(form: Cons) {
  return isQuasiquotingForm(KnownSymbol.Quasiquote, form);
}

/** Determine if form is ,x (read as (unquote x)) */
function isUnquote(form: Cons) {
  return isQuasiquotingForm(KnownSymbol.Unquote, form);
}

/** Determine if form is ,@x (read as (unquote-splicing x)) */
function isUnquoteSplicing(form: Cons) {
  return isQuasiquotingForm(KnownSymbol.UnquoteSplicing, form);
}

function isQuasiquotingForm(sym: Sym, form: Cons) {
  if (form.car !== sym) {
    return false;
  }

  if (!isCons(form.cdr) || form.cdr.cdr !== nil) {
    throw error(`Malformed ${print(sym)}.`);
  }

  return true;
}

/** Obtain x of (quasiquote x), (unquote x), or (unquote-splicing x). */
function argumentOf(quasiquotingForm: Cons) {
  return (quasiquotingForm.cdr as Cons).car;
}

/** Return form if it's self-evaluating, otherwise (quote form). */
function quote(form: unknown) {
  if ((!isCons(form) && !isSymbol(form)) || isBool(form)) {
    return form;
  }
  return list(KnownSymbol.Quote, form);
}

function notBoundError(key: string): never {
  throw new TypeError(`'${key}' is not bound.`);
}

function test(value: unknown) {
  return value !== nil && value !== null && value !== false;
}
