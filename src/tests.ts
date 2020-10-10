import { assert, AssertionError } from "chai";
import { createEnvironment, evaluate } from "./evaluator";
import { cons, equal, intern, list, listStar, nil, print, t } from "./primitives";
import { createScanner, read } from "./reader";

describe("arithmetic", () => {
  describeEvaluation(
    ["(+ 1 1)", 2],
    ["(/ 1 3)", 0],
    ["(/ 1 0)", "#<error>"],
    ["(/ 1.0 0)", Infinity],
    ["(/ -1.0 0)", -Infinity],
    ["(/ 0.0 0)", NaN],
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
    ["(null #null)", nil],
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

describe("symbol and number reading", () => {
  describeEvaluation(
    ["1", 1],
    ["2.", 2],
    [".3", 0.3],
    [".4e5", 0.4e5],
    [".5E+6", 0.5e6],
    ["1.0e+INF", Infinity],
    ["-1.0e+INF", -Infinity],
    ["0.0e+NaN", NaN],
    ["'1+", intern("1+")],
    ["'1-", intern("1-")],
    ["'+", intern("+")],
    ["'-", intern("-")],
    ["+1", 1],
    ["-1", -1],
    ["'a\\(b", intern("a(b")],
    ["'😎", intern("😎")],
    ["'💩", intern("💩")],
    // Test symbol normalization
    ["'\u0065\u0301", intern("\u00E9")]
  );
});

describe("functions", () => {
  it("can compute a factorial recursively", () => {
    const src = `
      (defun fact (n)
        (if (= n 0)
            1
            (* n (fact (- n 1)))))`;

    const fact = evaluateText(src) as Function;
    assert.strictEqual(fact(5), 120);
    assert.strictEqual(fact(20), 2432902008176640000n);
  });

  it("can compute a factorial iteratively", () => {
    const src = `
      (defun fact (n)
        (let ((v 1))
          (while (not (= n 0))
            (setq v (* v n))
            (setq n (- n 1)))
          v))`;

    const fact = evaluateText(src) as Function;
    assert.strictEqual(fact(5), 120);
    assert.strictEqual(fact(20), 2432902008176640000n);
  });
});

describe("quasiquoting", () => {
  describeEvaluation(
    ["`(1 2 3 ,(+ 2 2))", list(1, 2, 3, 4)],
    ["`(1 2 3 ,(+ 2 2) ,@(list 5))", list(1, 2, 3, 4, 5)],
    ["`(1 2 3 (4 5 ,@'(6 7)) ,8)", list(1, 2, 3, list(4, 5, 6, 7), 8)],
    [
      '`(A B C ,(intern "D") ,@(list \'E) . F)',
      listStar.apply(undefined, ["A", "B", "C", "D", "E", "F"].map(intern)),
    ],
    ["`(1 ,@ 2)", cons(1, 2)]
  );

  it("handles nested quasiquotes", () => {
    // Adapted from examples in appendix C of CLTL 2nd edition.
    // It's remarkably easy to break these.
    const env = createEnvironment();
    evaluateText(
      `(progn
        (setq q '(r s)) 
        (defun r (x) (apply * x)) 
        (setq s '(4 6)))`,
      env
    );
    assertLispEqual(doubleEvaluateText("``(,,q))", env), list(24));
    assertLispEqual(doubleEvaluateText("``(,@,q))", env), 24);
    evaluateText("(setq r '(3 5))", env);
    assertLispEqual(doubleEvaluateText("``(,,@q)", env), list(list(3, 5), list(4, 6)));
    assertLispEqual(doubleEvaluateText("``(,@,@q)", env), list(3, 5, 4, 6));
  });
});

function describeEvaluation(...args: [string, unknown][]) {
  for (const [expression, expected] of args) {
    it(`evaluates ${expression} to ${print(expected)}`, () => {
      let value;
      let success = false;
      try {
        value = evaluateText(expression);
        success = true;
      } catch (e) {
        if (expected === "#<error>") {
          return;
        }
        throw e;
      }
      assert.strictEqual(success, true);
      assertLispEqual(value, expected);
    });
  }
}

function evaluateText(expression: string, environment = createEnvironment()) {
  const scanner = createScanner(expression);
  const datum = read(scanner);
  return evaluate(datum, environment);
}

function doubleEvaluateText(expression: string, environment = createEnvironment()) {
  const firstEval = evaluateText(expression, environment);
  return evaluate(firstEval, environment);
}

function assertLispEqual(value: unknown, expected: unknown) {
  if (!equal(value, expected)) {
    throw new AssertionError(`Expected ${print(value)} to be ${print(expected)}.`);
  }
}
