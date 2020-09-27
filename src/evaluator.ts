import { generate } from "astring";
import { Nominal, isInterned } from "./primitives";
import * as es from "estree";
import * as primitives from "./primitives";

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
  parse,
} from "./expressions";

export interface Environment extends Nominal<"environment"> {
  [key: string]: unknown;
}

export function evaluate(form: unknown, environment: Environment): unknown {
  const lispTree = parse(form);
  const jsTree = compile(lispTree, environment);
  const jsText = generate(jsTree);
  const func = new Function(environmentId.name, `"use strict";return (${jsText});`);
  return func(environment);
}

export function createEnvironment(): Environment {
  return Object.create(environmentPrototype);
}

function compile(expression: Expression, environment: Environment): es.Expression {
  return compileExpression(expression);

  function compileExpression(expression: Expression) {
    switch (expression.kind) {
      case ExpressionKind.GetVariable:
        return compileGetVariable(expression);
      case ExpressionKind.If:
        return compileIf(expression);
      case ExpressionKind.Literal:
        return compileLiteral(expression);
      case ExpressionKind.Progn:
        return compileProgn(expression);
      case ExpressionKind.While:
        return compileWhile(expression);
      case ExpressionKind.Lambda:
        return compileLambda(expression);
      case ExpressionKind.SetVariable:
        return compileSetVariable(expression);
      case ExpressionKind.FunctionCall:
        return compileFunctionCall(expression);
    }
  }

  function compileFunctionCall(call: FunctionCall): es.CallExpression {
    const args = call.arguments.map(compileExpression);
    const callee = compileExpression(call.function);
    return {
      type: "CallExpression",
      optional: false,
      arguments: args,
      callee,
    };
  }

  function compileGetVariable(get: GetVariable) {
    return getVariable(get.symbol, get.isBound);
  }

  function compileSetVariable(set: SetVariable): es.AssignmentExpression {
    const left = getVariable(set.symbol, set.isBound);
    const right = compileExpression(set.value);
    return {
      type: "AssignmentExpression",
      operator: "=",
      left,
      right,
    };
  }

  function compileIf(conditional: If): es.ConditionalExpression {
    // BUG: This should test (not nil or false) instead of JavaScript truthiness.
    const test = compileExpression(conditional.test);
    const consequent = compileExpression(conditional.then);
    const alternate = compileExpression(conditional.otherwise);
    return {
      type: "ConditionalExpression",
      test,
      consequent,
      alternate,
    };
  }

  function compileLiteral(literal: Literal): es.Expression {
    const value = literal.value;

    if (value === undefined) {
      return undefinedLiteral;
    }

    if (value === null) {
      return nullLiteral;
    }

    switch (typeof value) {
      case "number":
      case "string":
      case "boolean":
        return { type: "Literal", value };
      default:
        const sym = Symbol();
        const id = getFreeSymbolId(sym);
        Object.defineProperty(environment, id, { value, writable: false });
        return getFreeVariable(sym);
    }
  }

  function compileProgn(progn: Progn): es.SequenceExpression {
    const expressions = progn.expressions.map(compileExpression);
    return { type: "SequenceExpression", expressions };
  }

  function compileLambda(lambda: Lambda): es.Expression {
    const body = compileExpression(lambda.body);
    const params = lambda.parameters.map(getBoundVariable);
    const id: es.Identifier | undefined = lambda.name
      ? { type: "Identifier", name: lambda.name }
      : undefined;

    return {
      type: "FunctionExpression",
      params,
      id,
      body: {
        type: "BlockStatement",
        body: [{ type: "ReturnStatement", argument: body }],
      },
    };
  }

  function compileWhile(loop: While): es.CallExpression {
    const test = compileExpression(loop.test);
    const body = compileExpression(loop.body);

    const loopBody: es.ExpressionStatement = {
      type: "ExpressionStatement",
      expression: body,
    };

    // BUG: This should test (not nil or false) instead of JavaScript truthiness
    const whileLoop: es.WhileStatement = {
      type: "WhileStatement",
      test,
      body: loopBody,
    };

    const callee: es.FunctionExpression = {
      type: "FunctionExpression",
      params: [],
      body: {
        type: "BlockStatement",
        body: [whileLoop, returnNil],
      },
    };

    return {
      type: "CallExpression",
      optional: false,
      arguments: [],
      callee,
    };
  }
}

const undefinedLiteral: es.UnaryExpression = {
  type: "UnaryExpression",
  prefix: true,
  operator: "void",
  argument: { type: "Literal", value: 0 },
};

const nullLiteral: es.Literal = {
  type: "Literal",
  value: null,
};

const environmentId: es.Identifier = {
  type: "Identifier",
  name: "$",
};

const returnNil: es.ReturnStatement = {
  type: "ReturnStatement",
  argument: undefinedLiteral,
};

/**
 * Map symbols to unique strings that are safe JavaScript identifiers. This
 * allows us to use JavaScript's lexical scoping directly even though Lisp
 * symbols can have names that are not valid identifiers. It further allows us
 * to achieve Lisp semantics where two symbols can have the same name but bind
 * to different slots if they're not both interned.
 *
 * The same scheme is used for uninterned global variables, but interned global
 * variables keep their descriptive names and are accessed as `$["<name>"]` to
 * make things easier to debug.
 */
const symbolIds = new Map<symbol, string>();

const environmentPrototype = (function () {
  const constants = {
    nil: primitives.nil,
    t: primitives.t,
    "most-positive-fixnum": primitives.mostPositiveFixnum,
    "most-negative-fixnum": primitives.mostNegativeFixnum,
  };
  const env = {
    ...constants,
    bignump: primitives.bignump,
    car: primitives.car,
    cdr: primitives.cdr,
    cons: primitives.cons,
    consp: primitives.consp,
    error: primitives.error,
    fixnump: primitives.fixnump,
    float: primitives.float,
    floatp: primitives.floatp,
    numberp: primitives.numberp,
    eq: primitives.eq,
    listp: primitives.listp,
    not: primitives.not,
    integerp: primitives.integerp,
    intern: primitives.intern,
    list: primitives.list,
    print: primitives.print,
    setcar: primitives.setcar,
    setcdr: primitives.setcdr,
    stringp: primitives.stringp,
    symbolp: primitives.symbolp,
    truncate: primitives.truncate,
    vectorp: primitives.vectorp,
    "+": primitives.add,
    "-": primitives.subtract,
    "/": primitives.divide,
    "*": primitives.multiply,
    "=": primitives.numericEqual,
    "make-symbol": primitives.makeSymbol,
    "symbol-name": primitives.symbolName,
    "to-string": (x: any) => x.toString(),
    "to-json": (x: any) => JSON.stringify(x, undefined, 2),
  };

  for (const key in constants) {
    Object.defineProperty(env, key, { writable: false });
  }

  return Object.freeze(env);
})();

function getBoundSymbolId(sym: symbol): string {
  let id = symbolIds.get(sym);
  if (id === undefined) {
    id = "$" + symbolIds.size.toString(36);
    symbolIds.set(sym, id);
  }
  return id;
}

function getFreeSymbolId(sym: symbol): string {
  return sym.description && isInterned(sym) ? sym.description : getBoundSymbolId(sym);
}

function getBoundVariable(sym: symbol): es.Identifier {
  const name = getBoundSymbolId(sym);
  return { type: "Identifier", name };
}

function getFreeVariable(sym: symbol): es.MemberExpression {
  const id = getFreeSymbolId(sym);
  return {
    type: "MemberExpression",
    computed: true,
    optional: false,
    object: environmentId,
    property: { type: "Literal", value: id },
  };
}

function getVariable(sym: symbol, isBound: boolean): es.Identifier | es.MemberExpression {
  return isBound ? getBoundVariable(sym) : getFreeVariable(sym);
}
