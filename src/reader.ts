import {
  List,
  Float,
  Integer,
  Num,
  Sym,
  Vector,
  error,
  cons,
  intern,
  list,
  nil,
} from "./primitives";

import { CharCode, isDelimiter, isDigit, isSymbolChar, isWhitespace } from "./characters";

export interface Scanner {
  readonly input: string;
  readonly position: number;
  readonly token: Token;
  readonly tokenStart: number;
  readonly tokenEnd: number;
  scan(): Token;
  getTokenText(): string;
}

/** A value that can be expressed with S-Expression syntax. */
export type Datum = string | Sym | Num | List | Vector | false | null;

export enum Token {
  None,
  Eof,
  OpenParenthesis,
  CloseParenthesis,
  OpenBracket,
  CloseBracket,
  Dot,
  Quote,
  SharpSymbol,
  Symbol,
  EscapedSymbol,
  String,
  Integer,
  Float,
  NaN,
  PositiveInfinity,
  NegativeInfinity,
  Quasiquote,
  Unquote,
  UnquoteSplicing,
}

export namespace KnownSymbol {
  export const Quote = intern("quote");
  export const Quasiquote = intern("quasiquote");
  export const Unquote = intern("unquote");
  export const UnquoteSplicing = intern("unquote-splicing");
  export const Setq = intern("setq");
  export const If = intern("if");
  export const While = intern("while");
  export const Lambda = intern("lambda");
  export const Progn = intern("progn");
  export const Defun = intern("defun");
  export const Let = intern("let");
  export const List = intern("list");
  export const Append = intern("append");
  export const Apply = intern("apply");
  export const Vector = intern("vector");
}

/** A token that represents a number. */
export type NumberToken =
  | Token.Integer
  | Token.Float
  | Token.NaN
  | Token.PositiveInfinity
  | Token.NegativeInfinity;

/** A token that represents a symbol */
export type SymbolToken = Token.Symbol | Token.EscapedSymbol;

export class EofError extends Error {
  constructor() {
    super("Unexpected end of file.");
  }
}

export function read(scanner: Scanner): Datum {
  let currentToken = Token.None;
  return readDatum();

  function readDatum() {
    switch (token()) {
      case Token.NaN:
        consume();
        return Number.NaN;
      case Token.PositiveInfinity:
        consume();
        return Number.POSITIVE_INFINITY;
      case Token.NegativeInfinity:
        consume();
        return Number.NEGATIVE_INFINITY;
      case Token.String:
        return readString();
      case Token.Integer:
        return readInteger();
      case Token.Float:
        return readFloat();
      case Token.EscapedSymbol:
        return readEscapedSymbol();
      case Token.Symbol:
        return readSymbol();
      case Token.OpenParenthesis:
        return readList();
      case Token.OpenBracket:
        return readVector();
      case Token.SharpSymbol:
        return readSharpSymbol();
      case Token.Quote:
        return readAbbreviation(KnownSymbol.Quote);
      case Token.Quasiquote:
        return readAbbreviation(KnownSymbol.Quasiquote);
      case Token.Unquote:
        return readAbbreviation(KnownSymbol.Unquote);
      case Token.UnquoteSplicing:
        return readAbbreviation(KnownSymbol.UnquoteSplicing);
      case Token.Eof:
        throw new EofError();
      default:
        throw error(`Unexpected token: ${Token[token()]}: '${scanner.getTokenText()}'.`);
    }
  }

  function readAbbreviation(symbol: Sym): List {
    consume();
    return list(symbol, readDatum());
  }

  function readString() {
    consume();
    const str = scanner.getTokenText().slice(1, -1);
    return str;
  }

  function readInteger() {
    consume();
    let text = scanner.getTokenText();

    if (text.length < 10) {
      return Integer(Number(text));
    }

    // BigInt does not allow trailing decimal point
    if (text.charCodeAt(text.length - 1) === CharCode.Dot) {
      text = text.slice(0, -1);
    }

    return Integer(BigInt(text));
  }

  function readFloat() {
    consume();
    const text = scanner.getTokenText();
    const num = Float(Number(text));
    return num;
  }

  function readSharpSymbol() {
    consume();
    const text = scanner.getTokenText();

    switch (text) {
      case "#undefined":
        return undefined;
      case "#null":
        return null;
      case "#false":
        return false;
      case "#true":
        return true;
      default:
        throw error(`Invalid read syntax: '${text}'`);
    }
  }

  function readEscapedSymbol(): Sym {
    consume();
    let input = scanner.getTokenText();
    let start = 0;
    let pos = 0;
    let s = "";

    for (; pos < input.length; pos++) {
      if (input.charCodeAt(pos) === CharCode.Backslash) {
        s += input.substring(start, pos);
        start = pos + 1;
      }
    }

    s += input.substring(start, pos);
    return intern(s);
  }

  function readSymbol(): Sym {
    consume();
    return intern(scanner.getTokenText());
  }

  function readVector() {
    consume();
    const vector: Vector = [];
    while (!tryConsume(Token.CloseBracket)) {
      vector.push(readDatum());
    }
    return Object.freeze(vector) as Vector;
  }

  function readList(): List {
    consume();
    return readRest();
  }

  function readRest(): List {
    if (tryConsume(Token.CloseParenthesis)) {
      return nil;
    }
    const car = readDatum();
    const cdr = token() === Token.Dot ? readDottedCdr() : readRest();
    return Object.freeze(cons(car, cdr));
  }

  function readDottedCdr() {
    consume();
    const cdr = readDatum();
    expect(Token.CloseParenthesis);
    return cdr;
  }

  function token() {
    if (currentToken === Token.None) {
      currentToken = scanner.scan();
    }
    return currentToken;
  }

  function consume() {
    currentToken = Token.None;
  }

  function tryConsume(expected: Token) {
    if (token() !== expected) {
      return false;
    }
    consume();
    return true;
  }

  function expect(expected: Token) {
    if (!tryConsume(expected)) {
      throw error(`Expected ${Token[expected]}, found ${Token[token()]}.`);
    }
  }
}

export function createScanner(input: string): Scanner {
  let position = 0;
  let tokenStart = 0;
  let tokenEnd = 0;
  let token = Token.None;

  skipTrivia();

  //prettier-ignore
  return {
    get position() { return position; },
    get token() {  return token; },
    get tokenStart() { return tokenStart; },
    get tokenEnd() { return tokenEnd; },
    get input() { return input; },
    scan,
    getTokenText,
  }

  function scan(): Token {
    tokenStart = position;
    token = scanToken();
    tokenEnd = position;
    skipTrivia();
    return token;
  }

  function getTokenText() {
    return input.substring(tokenStart, tokenEnd);
  }

  function scanToken(): Token {
    if (position === input.length) {
      return Token.Eof;
    }

    const ch = input.charCodeAt(position);
    switch (ch) {
      case CharCode.OpenParenthesis:
        position++;
        return Token.OpenParenthesis;
      case CharCode.CloseParenthesis:
        position++;
        return Token.CloseParenthesis;
      case CharCode.Quote:
        position++;
        return Token.Quote;
      case CharCode.OpenBracket:
        position++;
        return Token.OpenBracket;
      case CharCode.CloseBracket:
        position++;
        return Token.CloseBracket;
      case CharCode.BackQuote:
        position++;
        return Token.Quasiquote;
      case CharCode.Comma:
        return scanComma();
      case CharCode.Dot:
        return scanDot();
      case CharCode.Sharp:
        return scanSharp();
      case CharCode.DoubleQuote:
        return scanString();
      case CharCode.Plus:
      case CharCode.Minus:
        return scanNumberOrSymbol();
      default:
        if (isDigit(ch)) {
          return scanNumberOrSymbol();
        }
        if (isSymbolChar(ch)) {
          return scanSymbol();
        }
        throw error(`Unexpected character: '${input.charAt(position)}'`);
    }
  }

  function scanSharp() {
    position++;
    scanSymbol();
    return Token.SharpSymbol;
  }

  function scanComma() {
    position++;
    return match(CharCode.At) ? Token.UnquoteSplicing : Token.Unquote;
  }

  function scanDot(): Token.Dot | NumberToken | SymbolToken {
    if (isDelimiter(input.charCodeAt(position + 1))) {
      position++;
      return Token.Dot;
    } else {
      return scanNumberOrSymbol();
    }
  }

  // Annoyingly, from an implementor's perspective at least, Lisp's lexical
  // grammar does not make it possible to tell if something will be a number or
  // a symbol when the leading character is a digit or a plus or minus sign.
  //
  // For example:
  //  * `1`:  integer (positive one)
  //  * `+1`: integer (positive one)
  //  * `-1`: integer (negative one)
  //  * `1+`: symbol  (function that adds one)
  //  * `1-`: symbol  (function that subtracts one)
  //  * `+`:  symbol  (function that adds)
  //  * `-`:  symbol  (function thaat subtracts)
  //
  // The way this works is that anything with a leading `+`, `-`, or digit that
  // fails to scan as a number is a symbol. (This can lead to some surprises
  // where a typo in a number silently scans as a symbol.)
  //
  // Note that scanning does not parse the value of a numeric token, only its
  // range in the input and its classification as +Infinity, -Infinity, NaN,
  // Integer, or Float. Parsing the value is left to read(). Conveniently, that
  // part is trivial as the Lisp syntax for floats, and NaN) and integers is a
  // subset of JavaScript syntax, with the excpetion of +/- Infinity and NaN
  // that scanning does distinguish.
  function scanNumberOrSymbol(): NumberToken | SymbolToken {
    // **Leading plus or minus sign**
    let negative = match(CharCode.Minus);
    if ((negative || match(CharCode.Plus)) && atDelimiter()) {
      return Token.Symbol;
    }

    // **Whole part of significand**
    while (isDigit(input.charCodeAt(position))) {
      if (!advance()) {
        return Token.Integer;
      }
    }

    // **Fractional part of significand**
    let numberToken: NumberToken = Token.Integer;
    if (match(CharCode.Dot)) {
      if (atDelimiter()) {
        // Trailing decimal point, e.g. `123.` is an integer not a float in Lisp.
        // Amusingly, dates back to long ago when the no trailing decimal point
        // meant octal!
        return Token.Integer;
      }

      numberToken = Token.Float;
      while (isDigit(input.charCodeAt(position))) {
        if (!advance()) {
          return Token.Float;
        }
      }
    }

    // ** Exponent **
    if (match(CharCode.E) || match(CharCode.e)) {
      numberToken = Token.Float;
      if (match(CharCode.Plus)) {
        // e+NaN (NaN is case sensitive, e is not, + is required): NaN
        if (match3(CharCode.N, CharCode.a, CharCode.N) && atDelimiter()) {
          return Token.NaN;
        }
        // e+INF (INF is case sensitive, e is not, + is required): +/- Infinity
        if (match3(CharCode.I, CharCode.N, CharCode.F) && atDelimiter()) {
          return negative ? Token.NegativeInfinity : Token.PositiveInfinity;
        }
      } else {
        match(CharCode.Minus);
      }

      if (atDelimiter()) {
        return Token.Symbol;
      }

      if (!isDigit(input.charCodeAt(position))) {
        return scanSymbol();
      }

      do {
        if (!advance()) {
          return Token.Float;
        }
      } while (isDigit(input.charCodeAt(position)));
    }

    return atDelimiter() ? numberToken : scanSymbol();
  }

  function scanSymbol(): SymbolToken {
    let escaped = false;

    while (position < input.length) {
      let ch = input.charCodeAt(position);

      if (ch === CharCode.Backslash) {
        escaped = true;
        position++;
        if (position < input.length) {
          position++;
        }
        continue;
      }

      if (!isSymbolChar(ch)) {
        break;
      }
      position++;
    }

    return escaped ? Token.EscapedSymbol : Token.Symbol;
  }

  function scanString() {
    let ch: number;

    do {
      position++;
      if (position === input.length) {
        throw new EofError();
      }
      ch = input.charCodeAt(position);
    } while (ch !== CharCode.DoubleQuote);

    position++;
    return Token.String;
  }

  function skipTrivia() {
    while (position < input.length) {
      const ch = input.charCodeAt(position);
      if (ch === CharCode.Semicolon) {
        skipComment();
      } else if (isWhitespace(ch)) {
        position++;
      } else {
        break;
      }
    }
  }

  function skipComment() {
    let ch: number;
    do {
      position++;
      ch = input.charCodeAt(position);
    } while (position < input.length && ch !== CharCode.CarriageReturn && ch !== CharCode.LineFeed);
  }

  function advance() {
    position++;
    return !atDelimiter();
  }

  function atDelimiter() {
    return position === input.length || isDelimiter(input.charCodeAt(position));
  }

  function match(ch1: number) {
    if (input.charCodeAt(position) !== ch1) {
      return false;
    }

    position++;
    return true;
  }

  function match3(ch1: number, ch2: number, ch3: number) {
    if (
      input.charCodeAt(position) !== ch1 ||
      input.charCodeAt(position + 1) !== ch2 ||
      input.charCodeAt(position + 2) !== ch3
    ) {
      return false;
    }

    position += 3;
    return true;
  }
}
