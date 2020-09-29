import {
  Cons,
  error,
  isCons,
  isList,
  isNull,
  isSymbol,
  List,
  nil,
  Sym,
  symbolName,
  t,
} from "./primitives";

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
  Literal = "Literal",
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
  readonly symbol: Sym;
  readonly isBound: boolean;
}

export interface SetVariable extends ExpressionBase {
  readonly kind: ExpressionKind.SetVariable;
  readonly symbol: Sym;
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
  readonly parameters: readonly Sym[];
  readonly body: Expression;
  readonly name?: string;
}

export interface FunctionCall extends ExpressionBase {
  readonly kind: ExpressionKind.FunctionCall;
  readonly function: Expression;
  readonly arguments: readonly Expression[];
}

class BoundVariableSet {
  public readonly set: ReadonlySet<Sym>;
  constructor(symbols: Sym[] = [], public parent?: BoundVariableSet) {
    this.set = new Set<Sym>(symbols);
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
      switch (symbolName(sym)) {
        // special forms
        case "quote":
          return parseQuote(rest);
        case "setq":
          return parseSetq(rest);
        case "if":
          return parseIf(rest);
        case "while":
          return parseWhile(rest);
        case "lambda":
          return parseLambda(rest);
        case "progn":
          return parseProgn(rest);
        // "macros" currently implemented with hard-coded expansion in parser
        case "defun":
          return parseDefun(rest);
        case "let":
          return parseLet(rest);
      }
    }

    return parseFunctionCall(list);
  }

  function parseGetVariable(sym: Sym): GetVariable | Literal {
    const bound = isBound(sym);

    if (!bound && (sym === nil || sym === t)) {
      return parseLiteral(sym);
    }

    return {
      kind: ExpressionKind.GetVariable,
      symbol: sym,
      isBound: bound,
    };
  }

  function parseQuote(rest: List) {
    const [value] = expectTuple(rest, _);
    return parseLiteral(value);
  }

  function parseSetq(rest: List): SetVariable {
    const [sym, value] = expectTuple(rest, isSymbol, _);
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
    if (isNull(list)) {
      return parseLiteral(nil);
    } else if (isNull(list.cdr)) {
      return parseForm(list.car);
    } else {
      return parseProgn(list);
    }
  }

  function parseLambda(list: List, name?: string): Lambda {
    const [parameterList, body] = expectList(list, isList);
    const parameters = expectArray(parameterList, isSymbol);
    return parseLambdaBody(parameters, body, name);
  }

  function parseLambdaBody(parameters: Sym[], body: List, name?: string) {
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
    const parameters = bindings.map(list => expect(list.car, isSymbol));
    const args = bindings.map(list => expectTuple(expect(list.cdr, isList), _)[0]);
    const lambda = parseLambdaBody(parameters, body);

    return {
      kind: ExpressionKind.FunctionCall,
      function: lambda,
      arguments: args.map(parseForm),
    };
  }

  function parseDefun(rest: List): SetVariable {
    const [sym, lambda] = expectList(rest, isSymbol);
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

  function pushBoundVariables(variables: Sym[]) {
    boundVariables = new BoundVariableSet(variables, boundVariables);
  }

  function popBoundVariables() {
    boundVariables = boundVariables!.parent;
  }

  function isBound(variable: Sym) {
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
  if (isNull(list)) {
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

function* iterate(list: List) {
  while (!isNull(list)) {
    yield list.car;
    list = expect(list.cdr, isList);
  }
}

function _(_value: unknown): _value is unknown {
  return true;
}
