import { generate } from "astring";
import { Nominal, isInterned, print, symbolName, nil, isCons, error } from "./primitives";
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
  expandQuasiquote,
} from "./expressions";
import { KnownSymbol } from "./reader";

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

export function compileFormToString(form: unknown) {
  const lispTree = parse(form);
  const [jsTree, literals] = compile(lispTree);
  const jsText = generate(jsTree);

  let comment = "\n";
  for (let i = 0; i < literals.length; i++) {
    comment += `// $L[${i}]: ${print(literals[i])}\n`;
  }

  return comment + jsText + "\n";
}

export function compile(expression: Expression): [es.Expression, unknown[]] {
  let literals: unknown[] = [];
  return [compileExpression(expression), literals];

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
        callee: variableGetterId,
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
    const test = getTest(compileExpression(conditional.test));
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

    switch (value) {
      case nil:
        return nilLiteral;
      case null:
        return nullLiteral;
      case Infinity:
        return infinityLiteral;
      case -Infinity:
        return negativeInfinityLiteral;
      default:
        if (Number.isNaN(value)) {
          return nanLiteral;
        }
    }

    switch (typeof value) {
      case "number":
      case "string":
      case "boolean":
        return getLiteral(value);
      default:
        literals.push(value);
        return getIndexedLiteral(literals.length - 1);
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
    const test = getTest(compileExpression(loop.test));
    const body = compileExpression(loop.body);

    const loopBody: es.ExpressionStatement = {
      type: "ExpressionStatement",
      expression: body,
    };

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
const nilLiteral = getIdentifier("undefined");
const infinityLiteral = getIdentifier("Infinity");
const nanLiteral = getIdentifier("NaN");
const environmentId = getIdentifier("$E");
const literalsId = getIdentifier("$L");
const variableGetterId = getIdentifier("$G");
const testerId = getIdentifier("$T");

const negativeInfinityLiteral: es.UnaryExpression = {
  type: "UnaryExpression",
  argument: infinityLiteral,
  operator: "-",
  prefix: true,
};

const returnNil: es.ReturnStatement = {
  type: "ReturnStatement",
  argument: nilLiteral,
};

/**
 * Map symbols to unique strings that are safe JavaScript identifiers. This
 * allows us to use JavaScript's lexical scoping directly even though Lisp
 * symbols can have names that are not valid identifiers. It further allows us
 * to achieve Lisp semantics where two symbols can have the same name but bind
 * to different slots if they're not both interned.
 *
 * The same scheme is used for uninterned global variables, but interned global
 * variables keep their descriptive names and are accessed as `$E["<name>"]`
 * where `$E` is always in scope as the global environment. This helps debugging
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
    append: primitives.append,
    apply: primitives.apply,
    bignump: primitives.bignump,
    booleanp: primitives.booleanp,
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
    eql: primitives.eql,
    listp: primitives.listp,
    not: primitives.not,
    null: primitives.nilp,
    integerp: primitives.integerp,
    intern: primitives.intern,
    list: primitives.list,
    print: primitives.print,
    setcar: primitives.setcar,
    setcdr: primitives.setcdr,
    stringp: primitives.stringp,
    symbolp: primitives.symbolp,
    truncate: primitives.truncate,
    vector: primitives.vector,
    vectorp: primitives.vectorp,
    "+": primitives.add,
    "-": primitives.subtract,
    "/": primitives.divide,
    "*": primitives.multiply,
    "=": primitives.numericEqual,
    "list*": primitives.listStar,
    "make-symbol": primitives.makeSymbol,
    "symbol-name": primitives.symbolName,

    // hack for now to debug quasiquote expansion
    macroexpand: function (form: unknown) {
      if (!isCons(form) || form.car !== KnownSymbol.Quasiquote) {
        return form;
      }
      if (!isCons(form.cdr)) {
        throw error("Invalid quasiquote");
      }
      return expandQuasiquote(form.cdr.car);
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
    id = "$V" + symbolIds.size.toString(36);
    symbolIds.set(sym, id);
  }
  return id;
}

function getFreeSymbolId(sym: symbol): string {
  return isInterned(sym) ? symbolName(sym) : getBoundSymbolId(sym);
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

function getIndexedLiteral(index: number): es.MemberExpression {
  return {
    type: "MemberExpression",
    object: literalsId,
    property: getLiteral(index),
    computed: true,
    optional: false,
  };
}

function getTest(expression: es.Expression): es.CallExpression {
  return {
    type: "CallExpression",
    callee: testerId,
    arguments: [expression],
    optional: false,
  };
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

function test(value: unknown) {
  return value !== nil && value !== null && value !== false;
}
