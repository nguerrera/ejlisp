import {
  Cons,
  List,
  Sym,
  error,
  isBool,
  isCons,
  isList,
  isSymbol,
  nil,
  symbolName,
  t,
  iterate,
  list,
  isVector,
  print,
  append,
  cons,
} from "./primitives";
import { KnownSymbol } from "./reader";

export type Expression =
  | Literal
  | GetVariable
  | SetVariable
  | Progn
  | If
  | While
  | Lambda
  | FunctionCall;

export enum ExpressionKind {
  Literal = "Quote",
  GetVariable = "GetVariable",
  SetVariable = "SetVariable",
  Progn = "Progn",
  If = "If",
  While = "While",
  FunctionCall = "FunctionCall",
  Lambda = "Lambda",
}

interface ExpressionBase {
  readonly kind: ExpressionKind;
}

export interface Literal extends ExpressionBase {
  readonly kind: ExpressionKind.Literal;
  readonly value: unknown;
}

export interface GetVariable extends ExpressionBase {
  readonly kind: ExpressionKind.GetVariable;
  readonly symbol: symbol;
  readonly isBound: boolean;
}

export interface SetVariable extends ExpressionBase {
  readonly kind: ExpressionKind.SetVariable;
  readonly symbol: symbol;
  readonly value: Expression;
  readonly isBound: boolean;
}

export interface Progn {
  readonly kind: ExpressionKind.Progn;
  readonly expressions: readonly Expression[];
}

export interface If extends ExpressionBase {
  readonly kind: ExpressionKind.If;
  readonly test: Expression;
  readonly then: Expression;
  readonly otherwise: Expression;
}

export interface While extends ExpressionBase {
  readonly kind: ExpressionKind.While;
  readonly test: Expression;
  readonly body: Expression;
}

export interface Lambda extends ExpressionBase {
  readonly kind: ExpressionKind.Lambda;
  readonly parameters: readonly symbol[];
  readonly body: Expression;
  readonly name?: string;
}

export interface FunctionCall extends ExpressionBase {
  readonly kind: ExpressionKind.FunctionCall;
  readonly function: Expression;
  readonly arguments: readonly Expression[];
}

class BoundVariableSet {
  public readonly set: ReadonlySet<symbol>;
  constructor(symbols: readonly symbol[], public parent?: BoundVariableSet) {
    this.set = new Set<symbol>(symbols);
  }
}

/**
 * Parse a Lisp object (usually read from source by read()) into an AST.
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
export function expandQuasiquote(form: unknown): unknown {
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

  /**
   * If atom is a non-self-evaluating symbol, return (quote atom). Otherwise
   * return atom itself as a simplification..
   */
  function quote(atom: unknown) {
    if (!isSymbol(atom) || isBool(atom)) {
      return atom;
    }
    return list(KnownSymbol.Quote, atom);
  }
}
