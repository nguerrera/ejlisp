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

export function createEnvironment(): Environment {
  return Object.create(environmentPrototype);
}

export function evaluate(form: unknown, environment: Environment): unknown {
  const jsText = compileFormToString(form, environment);
  const func = new Function(environmentId.name, `"use strict";return (${jsText});`);
  return func(environment);
}

export function compileFormToString(form: unknown, environment: Environment) {
  const lispTree = parse(form);
  const jsTree = compile(lispTree, environment);
  const jsText = generate(jsTree);
  return jsText;
}

export function compile(expression: Expression, environment: Environment): es.Expression {
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

  function compileGetVariable(get: GetVariable): es.Expression {
    if (get.isBound) {
      return getBoundVariable(get.symbol);
    } else {
      const id = getFreeSymbolId(get.symbol);
      return {
        type: "CallExpression",
        callee: environmentGetter,
        arguments: [getLiteral(id)],
        optional: false,
      };
    }
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
        return getLiteral(value);
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

    return nameFunction(lambda.name, {
      type: "FunctionExpression",
      params,
      body: {
        type: "BlockStatement",
        body: [{ type: "ReturnStatement", argument: body }],
      },
    });
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

const nullLiteral = getLiteral(null);
const environmentId = getIdentifier("$");

const environmentGetter: es.MemberExpression = {
  type: "MemberExpression",
  object: environmentId,
  property: environmentId,
  computed: false,
  optional: false,
};

const undefinedLiteral: es.UnaryExpression = {
  type: "UnaryExpression",
  prefix: true,
  operator: "void",
  argument: getLiteral(0),
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
 * variables keep their descriptive names and are accessed as `$["<name>"]`
 * where `$` is always in scope as the global environment. This helps debugging
 * and allows a JavaScript host to easily inject things into the Lisp global
 * environment.
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

    [environmentId.name]: function (this: Environment, key: string) {
      return key in this ? this[key] : notBoundError(key);
    },
  };

  for (const key in constants) {
    Object.defineProperty(env, key, { writable: false });
  }

  return env;
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
  return getIdentifier(name);
}

function getFreeVariable(sym: symbol): es.MemberExpression {
  const id = getFreeSymbolId(sym);
  return {
    type: "MemberExpression",
    computed: true,
    optional: false,
    object: environmentId,
    property: getLiteral(id),
  };
}

function getVariable(sym: symbol, isBound: boolean) {
  return isBound ? getBoundVariable(sym) : getFreeVariable(sym);
}

function getIdentifier(name: string): es.Identifier {
  return { type: "Identifier", name };
}

function getLiteral(value: string | number | boolean | null): es.Literal {
  return { type: "Literal", value };
}

function nameFunction(name: string | undefined, func: es.FunctionExpression): es.Expression {
  if (!name) {
    return func;
  }

  const key = getLiteral(name);

  const obj: es.ObjectExpression = {
    type: "ObjectExpression",
    properties: [
      {
        type: "Property",
        kind: "init",
        method: false,
        shorthand: false,
        computed: false,
        key,
        value: func,
      },
    ],
  };

  return {
    type: "MemberExpression",
    computed: true,
    object: obj,
    optional: false,
    property: key,
  };
}

function notBoundError(key: string): never {
  throw new TypeError(`'${key}' is not bound.`);
}
