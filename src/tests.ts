import { assert } from "chai";
import { createEnvironment, evaluate } from "./evaluator";
import { nil, print, t } from "./primitives";
import { createScanner, read } from "./reader";

describe("arithmetic", () => {
  describeEvaluation(
    ["(+ 1 1)", 2],
    ["(/ 1 3)", 0],
    ["(/ 1.0 3)", 1.0 / 3.0],
    ["(+ most-positive-fixnum 1)", 2147483648n],
    ["(- most-negative-fixnum 1)", -2147483649n],
    ["(* most-negative-fixnum -1)", 2147483648n],
    ["(/ most-negative-fixnum -1)", 2147483648n],
    ["(* most-positive-fixnum most-positive-fixnum)", 4611686014132420609n],
    ["(* 4503599627370496 2)", 9007199254740992n]
  );
});

describe("truthiness", () => {
  describeEvaluation(
    ["#null", null],
    ["#undefined", undefined],
    ["#true", true],
    ["#false", false],
    ["nil", nil],
    ["t", t],
    ["(null #null)", t],
    ["(null #undefined)", t],
    ["(null #true)", nil],
    ["(null #false)", nil],
    ["(null nil)", t],
    ["(null t)", nil],
    ["(eq #null nil)", nil],
    ["(eq #undefined nil)", t],
    ["(eq #true t)", t],
    ["(eq #null #undefined)", nil],
    ["(eq #false nil)", nil],
    ["(symbolp nil)", t],
    ["(symbol-name nil)", "nil"],
    ["(symbolp t)", t],
    ["(symbol-name t)", "t"],
    ["(symbolp #null)", nil],
    ["(symbolp #undefined)", t],
    ["(symbolp #true)", t],
    ["(symbolp #false)", nil],
    ["(not #null)", t],
    ["(not #undefined)", t],
    ["(not #true)", nil],
    ["(not #false)", t],
    ["(not nil)", t],
    ["(not t)", nil],
    ["(not 0)", nil],
    ['(not "")', nil],
    ["(if #null 1 2)", 2],
    ["(if #undefined 1 2)", 2],
    ["(if #true 1 2)", 1],
    ["(if #false 1 2)", 2],
    ["(if nil 1 2)", 2],
    ["(if t 1 2)", 1],
    ["(if 0 1 2)", 1],
    ['(if "" 1 2)', 1]
  );
});

function describeEvaluation(...args: [string, unknown][]) {
  for (const [expression, expected] of args) {
    it(`evaluates ${expression} to ${print(expected)}`, () => {
      const environment = createEnvironment();
      const scanner = createScanner(expression);
      const datum = read(scanner);
      const value = evaluate(datum, environment);
      assert.strictEqual(value, expected);
    });
  }
}
