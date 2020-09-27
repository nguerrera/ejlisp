import { assert } from "chai";
import { createEnvironment, evaluate } from "./evaluator";
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

function describeEvaluation(...args: [string, unknown][]) {
  for (const [expression, expected] of args) {
    it(`evaluates ${expression} to ${expected}`, () => {
      const environment = createEnvironment();
      const scanner = createScanner(expression);
      const datum = read(scanner);
      const value = evaluate(datum, environment);
      assert.strictEqual(value, expected);
    });
  }
}
