/**
 * Unique symbol that is private to this module and serves to implement
 * `Nominal<T>` trick below.
 *
 * The `[typename]` property is never actually set and gets erased by
 * compilation. It is also not offered by code completion outside this file.
 */
const typename = Symbol();

/**
 * Creates an effectively nominal type in TypeScript's structural type system.
 */
export interface Nominal<TypeName extends string> {
  readonly [typename]: TypeName;
}

/**
 * Lisp nil value is JS undefined. It appears as an interned symbol named "nil"
 * to Lisp. It can also be spelled as #undefined to emphasize JS interop.
 *
 * JS undefined was chosen over JS null because it allows seamless interop with
 * optional parameters and optional chaining in JS. The naming is unfortunate.
 */
export const nil = undefined;

/**
 * Lisp null type.
 *
 * Both nil and #null (JS null) are considered to null in the sense that (null
 * nil) and (null #null) are both t. However, (eq nil #null) is nil, and a list
 * with a final cdr of #null will be printed in dotted form.
 */
export type Null = typeof nil | null;

/**
 * Lisp t value is JS true. It appears as an interned symbol named "t" to Lisp.
 * It can also be spelled as #true to emphasize JS interop.
 */
export const t = true;

/**
 * Canonical Lisp boolean type: nil (JS null) or t (JS true).
 *
 * nil, #false, #undefined are falsy and all other values are truthy, but only
 * nil and t are canonical booleans.
 */
export type Bool = typeof nil | typeof t;

/**
 * Lisp cons cell type.
 *
 * Cons cells are represented as objects with a car and cdr property.
 *
 * Note, however, that this interface cannot be implemented externally, nor can
 * object literals be used to make new cells. Construct new cells using cons(x,
 * y).
 */
export interface Cons extends Nominal<"cons"> {
  car: unknown;
  cdr: unknown;
}

/**
 * Lisp list type: cons cell or nil.
 */
export type List = Cons | Null;

/**
 * Lisp vector type: one dimensional array.
 *
 * Represented directly as JavaScript array.
 */
export type Vector = unknown[];

/**
 * Lisp number type.
 *
 * All primitive JavaScript numbers are lisp numbers. `number`s in fixnum range
 * are lisp fixnums. `number`s outside fixnum range are lisp floats.
 *
 * However, not all lisp numbers are primitive JavaScript primitive numbers.
 * Lisp floats with values in fixnum range are "boxed" as `Number` objects in
 * order to distinguish them from actual fixnums.
 */
export type Num = number | bigint | Float;

/**
 * Lisp fixnum type: 32-bit signed integer.
 *
 * Arithmetic on fixnums does not overflow, it produces bignums.
 *
 * Represented directly as JavaScript `number`s.
 */
export type Fixnum = number & Nominal<"fixnum">;

/**
 * Lisp bignum type: arbitrary sized integer.
 *
 * Represented directly as JavaScript `bigint`.
 */
export type Bignum = bigint;

/**
 * Lispt integer type: fixnum or bignum.
 */
export type Integer = Fixnum | Bignum;

/**
 * Lisp float type: double precision floating point number.
 *
 * Values that have fractional parts or are outside fixnum range represented
 * directly as JavaScript `numbers`. Values in fixnum range are "boxed" into
 * `Number`s to distinguish them from actual fixnums.
 */
export type Float = (number | Number) & Nominal<"float">;

/**
 * Return type of binary arithmetic operation on numbers.
 *
 * If either operand is a float, operands are coerced to float and operation is
 * done in floating point, producing a float result. If both operands are integers,
 * the result is an integer.
 */
// prettier-ignore
export type Arithmetic<T1 extends Num, T2 extends Num> =
  T1 extends Float ? Float :
  T2 extends Float ? Float :
  T1 extends Integer ? (T2 extends Integer ? Integer : Num) :
  Num;

/**
 * Lisp symbol type.
 *
 * Represented as JavaScript symbol with two exceptions: JavaScript null
 * represents the interned nil symbol and JavaScript true represents the
 * interned t symbol.
 */
export type Sym = symbol | Bool;

/** Largest integer value that can be represented as a fixnum. */
export const mostNegativeFixnum = -0x80000000 as Fixnum;

/** Smallest integer value that can be represented as a fixnum. */
export const mostPositiveFixnum = 0x7fffffff as Fixnum;

export function isNull(value: unknown): value is Null {
  return value == null; // null or undefined
}

export function nullp(value: unknown): Bool {
  return isNull(value) || nil;
}

export function isCons(value: unknown): value is Cons {
  return (value as object)?.constructor === ConsCell;
}

export function consp(value: unknown): Bool {
  return isCons(value) || nil;
}

export function isList(value: unknown): value is List {
  return isCons(value) || isNull(value);
}

export function listp(value: unknown): Bool {
  return isList(value) || nil;
}

export function isVector(value: unknown): value is Vector {
  return Array.isArray(value);
}

export function vectorp(value: unknown): Bool {
  return isVector(value) || nil;
}

export function isString(value: unknown): value is string {
  return typeof value === "string";
}

export function stringp(value: unknown): Bool {
  return isString(value) || nil;
}

export function isSymbol(value: unknown): value is Sym {
  return value === nil || value === t || typeof value === "symbol";
}

export function symbolp(value: unknown): Bool {
  return isSymbol(value) || nil;
}

export function isNumber(value: unknown): value is Num {
  return (
    typeof value === "number" ||
    typeof value === "bigint" ||
    (value as object)?.constructor === FixnumFloat
  );
}

export function numberp(value: unknown): Bool {
  return isNumber(value) || nil;
}

export function isFixnum(value: unknown): value is Fixnum {
  return typeof value === "number" && value === (value | 0);
}

export function fixnump(value: unknown): Bool {
  return isFixnum(value) || nil;
}

export function isBignum(value: unknown): value is Bignum {
  return typeof value === "bigint";
}

export function bignump(value: unknown): Bool {
  return isBignum(value) || nil;
}

export function isInteger(value: unknown): value is Integer {
  return isFixnum(value) || isBignum(value);
}

export function integerp(value: unknown): Bool {
  return isInteger(value) || nil;
}

export function isFloat(value: unknown): value is Float {
  return isNumber(value) && !isInteger(value);
}

export function floatp(value: unknown): Bool {
  return isFloat(value) || nil;
}

export function cons(car: unknown, cdr: unknown): Cons {
  return new ConsCell(car, cdr);
}

export function car(list: List): unknown {
  if (isNull(list)) {
    return nil;
  }
  isCons(list) || listError(list);
  return list.car;
}

export function cdr(list: List): unknown {
  if (isNull(list)) {
    return nil;
  }
  isCons(list) || listError(list);
  return list.cdr;
}

export function setcar<T>(cons: Cons, value: T): T {
  isCons(cons) || consError(cons);
  return (cons.car = value);
}

export function setcdr<T>(cons: Cons, value: T): T {
  isCons(cons) || consError(cons);
  return (cons.cdr = value);
}

export function list(...values: unknown[]): List {
  let result: List = nil;
  for (let i = values.length - 1; i >= 0; i--) {
    result = cons(values[i], result);
  }
  return result;
}

/**
 * Converts a JavaScript number to a lisp integer.
 *
 * Throws if value has a fractional part.
 */
export function Integer(value: number | bigint): Integer {
  !new.target || notAConstructorError(new.target);

  if (isFixnum(value)) {
    return value;
  }
  if (isBignum(value)) {
    return normalizeBignum(value);
  }
  return Number.isInteger(value) ? BigInt(value) : integerError(value);
}

/**
 * Converts a JavaScript number to a lisp float
 */
export function Float(value: number | bigint): Float {
  !new.target || notAConstructorError(new.target);
  value = Number(value);
  return isFixnum(value) ? new FixnumFloat(value) : (value as Float);
}

export function add<T1 extends Num, T2 extends Num>(x: T1, y: T2): Arithmetic<T1, T2>;
export function add(x: Num, y: Num): Num {
  return isFixnum(x) && isFixnum(y) ? fixnumAdd(x, y) : slowAdd(x, y);
}

export function subtract<T1 extends Num, T2 extends Num>(x: T1, y: T2): Arithmetic<T1, T2>;
export function subtract(x: Num, y: Num): Num {
  return isFixnum(x) && isFixnum(y) ? fixnumSubtract(x, y) : slowSubtract(x, y);
}

export function multiply<T1 extends Num, T2 extends Num>(x: T1, y: T2): Arithmetic<T1, T2>;
export function multiply(x: Num, y: Num): Num {
  return isFixnum(x) && isFixnum(y) ? fixnumMultiply(x, y) : slowMultiply(x, y);
}

export function divide<T1 extends Num, T2 extends Num>(x: T1, y: T2): Arithmetic<T1, T2>;
export function divide(x: Num, y: Num): Num {
  return isFixnum(x) && isFixnum(y) ? fixnumDivide(x, y) : slowDivide(x, y);
}

export function numericEqual(x: Num, y: Num): Bool {
  isNumber(x) || numberError(x);
  isNumber(y) || numberError(y);
  return x == y || nil;
}

export function eq(x: unknown, y: unknown): Bool {
  return x === y || nil;
}

export function not(x: unknown): Bool {
  return x == null || x === false || nil;
}

export function error(message: string): never {
  throw new Error(message);
}

/**
 * Truncates a number to an fixnum value.
 *
 * Rounds towards 0.
 */
export function truncate(value: Num): Integer {
  if (isInteger(value)) {
    return value;
  }
  isNumber(value) || numberError(value);
  return Integer(Math.trunc(+value));
}

/** Coerces a number to a float. */
export function float(value: Num): Float {
  isNumber(value) || numberError(value);
  if (isFloat(value)) {
    return value;
  }
  value = Number(value);
  return isFixnum(value) ? new FixnumFloat(value) : (value as Float);
}

/** Creates a new, uninterned symbol */
export function makeSymbol(name: string): symbol {
  isString(name) || stringError(name);
  return Symbol(name);
}

/** Gets or creates an interned symbol */
export function intern(name: string): Sym {
  isString(name) || stringError(name);
  if (name === "nil") {
    return nil;
  }
  if (name === "t") {
    return t;
  }
  return Symbol.for(name);
}

export function isInterned(symbol: Sym) {
  if (symbol === nil || symbol === true) {
    return true;
  }
  typeof symbol === "symbol" || symbolError(symbol);
  return Symbol.keyFor(symbol) !== undefined;
}

/** Gets the name of a symbol. */
export function symbolName(symbol: Sym): string {
  if (symbol === nil) {
    return "nil";
  }
  if (symbol === t) {
    return "t";
  }
  typeof symbol === "symbol" || symbolError(symbol);
  return symbol.description || "";
}

export function print(value: unknown): string {
  if (value === false) {
    return "#false";
  }
  if (value === null) {
    return "#null";
  }

  if (isSymbol(value)) {
    const name = symbolName(value);
    if (isInterned(value)) {
      return name || "##";
    } else {
      return `#:${name}`;
    }
  }

  if (isString(value)) {
    return '"' + value + '"';
  }

  if (isNumber(value)) {
    if (isFloat(value) && Number.isInteger(+value)) {
      return value.toString() + ".0";
    } else {
      return value.toString();
    }
  }

  if (isVector(value)) {
    return "[" + value.map(print).join(" ") + "]";
  }

  if (isCons(value)) {
    let s = "(" + print(value.car);
    value = value.cdr;
    while (isCons(value)) {
      s += " " + print(value.car);
      value = value.cdr;
    }
    if (value !== nil) {
      s += " . " + print(value);
    }
    s += ")";
    return s;
  }

  if (typeof value === "function") {
    return `#<function:${value.name || "(anonymous)"}>`;
  }

  return `#<${typeof value}>`;
}

/** Internal representation of cons cells. */
class ConsCell implements Cons {
  readonly [typename]!: "cons";
  constructor(public car: unknown, public cdr: unknown) {}
}

/** Internal representation of floats with value in fixnum range. */
class FixnumFloat extends Number implements Nominal<"float"> {
  readonly [typename]!: "float";
  constructor(value: Fixnum) {
    super(value);
  }
}

function normalizeBignum(value: Bignum): Integer {
  if (value <= mostNegativeFixnum || value >= mostPositiveFixnum) {
    return value;
  }
  return Number(value) as Fixnum;
}

function fixnumAdd(x: Fixnum, y: Fixnum): Integer {
  return Integer(x + y);
}

function slowAdd(x: Num, y: Num): Num {
  return isInteger(x) && isInteger(y) ? integerAdd(x, y) : floatAdd(x, y);
}

function integerAdd(x: Integer, y: Integer): Integer {
  return Integer(BigInt(x) + BigInt(y));
}

function floatAdd(x: Num, y: Num): Float {
  isNumber(x) || numberError(x);
  isNumber(y) || numberError(y);
  return Float(Number(x) + Number(y));
}

function fixnumSubtract(x: Fixnum, y: Fixnum): Integer {
  return Integer(x - y);
}

function slowSubtract(x: Num, y: Num): Num {
  return isInteger(x) && isInteger(y) ? integerSubtract(x, y) : floatSubtract(x, y);
}

function integerSubtract(x: Integer, y: Integer): Integer {
  return Integer(BigInt(x) - BigInt(y));
}

function floatSubtract(x: Num, y: Num): Float {
  isNumber(x) || numberError(x);
  isNumber(y) || numberError(y);
  return Float(Number(x) - Number(y));
}

function fixnumMultiply(x: Fixnum, y: Fixnum): Integer {
  const xy = x * y;
  return isFixnum(xy) ? xy : integerMultiply(x, y);
}

function slowMultiply(x: Num, y: Num): Num {
  return isInteger(x) && isInteger(y) ? integerMultiply(x, y) : floatMultiply(x, y);
}

function integerMultiply(x: Integer, y: Integer): Integer {
  return Integer(BigInt(x) * BigInt(y));
}

function floatMultiply(x: Num, y: Num): Float {
  isNumber(x) || numberError(x);
  isNumber(y) || numberError(y);
  return Float(Number(x) * Number(y));
}

function fixnumDivide(x: Fixnum, y: Fixnum): Integer {
  y !== 0 || divideByZeroError();
  return Integer(Math.trunc(x / y));
}

function slowDivide(x: Num, y: Num): Num {
  return isInteger(x) && isInteger(y) ? integerDivide(x, y) : floatDivide(x, y);
}

function integerDivide(x: Integer, y: Integer): Integer {
  return Integer(BigInt(x) / BigInt(y));
}

function floatDivide(x: Num, y: Num): Float {
  isNumber(x) || numberError(x);
  isNumber(y) || numberError(y);
  return Float(Number(x) / Number(y));
}

function divideByZeroError() {
  throw new RangeError("Division by zero");
}

function notAConstructorError(target: Function): never {
  throw new TypeError(`${target.name} is not a constructor`);
}

function typeError(expected: string, value: unknown): never {
  throw new TypeError(`Incorrect argument type: ${expected}, ${value}`);
}

function numberError(value: unknown): never {
  typeError("numberp", value);
}

function integerError(value: unknown): never {
  typeError("integerp", value);
}

function stringError(value: unknown): never {
  typeError("stringp", value);
}

function symbolError(value: unknown): never {
  typeError("symbolp", value);
}

function listError(value: unknown): never {
  typeError("listp", value);
}

function consError(value: unknown): never {
  typeError("lisp", value);
}
