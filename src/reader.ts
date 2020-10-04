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

export interface Scanner {
  readonly input: string;
  readonly position: number;
  readonly token: Token;
  readonly tokenPosition: number;
  readonly tokenEnd: number;
  scan(): Token;
  getTokenText(): string;
  appendInput(input: string): void;
}

/** A value that can be expressed with S-Expression syntax. */
export type Datum = string | Sym | Num | List | Vector | false | null;

export enum Token {
  None,
  Eof,
  OpenParenthesis,
  CloseParenthesis,
  OpenSquareBracket,
  CloseSquareBracket,
  Dot,
  Quote,
  PoundSymbol,
  Symbol,
  EscapedSymbol,
  String,
  Integer,
  Float,
  NaN,
  PositiveInfinity,
  NegativeInfinity,
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

/**
 * Maps names of well-known characters to UTF-16 code unit numeric value
 */
// prettier-ignore
export const enum CharCode {
  Tab = 9,
  LineFeed = 10,
  CarriageReturn = 13,
  Space = 32,
  Bang = 33,
  DoubleQuote = 34,
  Pound = 35,
  Dollar = 36,
  Percent = 37,
  Ampersand = 38,
  Quote = 39,
  OpenParenthesis = 40,
  CloseParenthesis = 41,
  Asterisk = 42,
  Plus = 43,
  Comma = 44,
  Minus = 45,
  Dot = 46,
  Slash = 47,
  Colon = 58,
  Semicolon = 59,
  LessThan = 60,
  Equal = 61,
  GreaterThan = 62,
  Question = 63,
  At = 64,
  OpenSquareBracket = 91,
  Backslash = 92,
  CloseSquareBracket = 93,
  Circumflex = 94,
  Underscore = 95,
  OpenBrace = 123,
  CloseBrace = 125,
  Tilde = 126,

  A = 65, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
  a = 97, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z,
  _0 = 48, _1, _2, _3, _4, _5, _6, _7, _8, _9,
}

export function read(scanner: Scanner): Datum {
  let currentToken = Token.None;
  return readDatum();

  function readDatum() {
    switch (token()) {
      case Token.String:
        return readString();
      case Token.Integer:
        return readInteger();
      case Token.Float:
        return readFloat();
      case Token.NaN:
        consume(Token.NaN);
        return NaN;
      case Token.PositiveInfinity:
        consume(Token.PositiveInfinity);
        return Infinity;
      case Token.NegativeInfinity:
        consume(Token.NegativeInfinity);
        return -Infinity;
      case Token.EscapedSymbol:
        return readEscapedSymbol();
      case Token.Symbol:
        return readSymbol();
      case Token.OpenParenthesis:
        consume(Token.OpenParenthesis);
        return readList();
      case Token.OpenSquareBracket:
        consume(Token.OpenSquareBracket);
        return readVector();
      case Token.PoundSymbol:
        return readPoundSymbol();
      case Token.Quote:
        return readQuote();
      case Token.Eof:
        throw error("Unexpected end of file.");
      default:
        throw error(`Unexpected token: ${Token[token()]}: '${scanner.getTokenText()}'.`);
    }
  }

  function readQuote(): List {
    consume(Token.Quote);
    return list(intern("quote"), readDatum());
  }

  function readString() {
    const str = scanner.getTokenText().slice(1, -1);
    consume(Token.String);
    return str;
  }

  function readInteger() {
    let text = scanner.getTokenText();
    consume(Token.Integer);

    if (text.length < 10) {
      return Integer(Number(text));
    }

    if (text.charCodeAt(text.length - 1) === CharCode.Dot) {
      text = text.slice(0, -1);
    }

    return Integer(BigInt(text));
  }

  function readFloat() {
    const text = scanner.getTokenText();
    const num = Float(Number(text));
    consume(Token.Float);
    return num;
  }

  function readPoundSymbol() {
    consume(Token.PoundSymbol);
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
    let input = scanner.getTokenText();
    let start = 0;
    let pos = 0;
    let s = "";

    while (pos < input.length) {
      if (input.charCodeAt(pos) === CharCode.Backslash) {
        s += input.substring(start, pos);
        start = pos + 1;
      }
      pos++;
    }

    s += input.substring(start, pos);
    consume(Token.EscapedSymbol);
    return intern(s);
  }

  function readSymbol(): Sym {
    consume(Token.Symbol);
    return intern(scanner.getTokenText());
  }

  function readList(): List {
    if (tryConsume(Token.CloseParenthesis)) {
      return nil;
    }
    const car = readDatum();
    const cdr = token() === Token.Dot ? readDottedCdr() : readList();
    return Object.freeze(cons(car, cdr));
  }

  function readVector() {
    const vector: Vector = [];
    while (!tryConsume(Token.CloseSquareBracket)) {
      vector.push(readDatum());
    }
    return Object.freeze(vector) as Vector;
  }

  function readDottedCdr() {
    consume(Token.Dot);
    const cdr = readDatum();
    expect(Token.CloseParenthesis);
    return cdr;
  }

  function token() {
    return currentToken === Token.None ? (currentToken = scanner.scan()) : currentToken;
  }

  function tryConsume(expected: Token) {
    if (token() !== expected) {
      return false;
    }
    currentToken = Token.None;
    return true;
  }

  function expect(expected: Token) {
    if (!tryConsume(expected)) {
      throw error(`Expected ${Token[expected]}, found ${Token[token()]}.`);
    }
  }

  function consume(expected: Token) {
    if (!tryConsume(expected)) {
      throw error(`Internal error: expected  ${Token[expected]}, found ${Token[token()]}.`);
    }
  }
}

export function createScanner(input: string, eofHandler = () => {}): Scanner {
  let position = 0;
  let tokenPosition = 0;
  let tokenEnd = 0;
  let token = Token.None;

  //prettier-ignore
  return {
    get position() { return position; },
    get token() {  return token; },
    get tokenPosition() { return tokenPosition; },
    get tokenEnd() { return tokenEnd; },
    get input() { return input; },
    scan,
    getTokenText,
    appendInput,
  }

  function scan(): Token {
    skipLeadingTrivia();
    tokenPosition = position;
    token = scanToken();
    tokenEnd = position;
    skipTrailingTrivia();
    return token;
  }

  function getTokenText() {
    return input.substring(tokenPosition, tokenEnd);
  }

  function appendInput(moreInput: string) {
    input += moreInput;
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
      case CharCode.OpenSquareBracket:
        position++;
        return Token.OpenSquareBracket;
      case CharCode.CloseSquareBracket:
        position++;
        return Token.CloseSquareBracket;
      case CharCode.Dot:
        return scanDot();
      case CharCode.Pound:
        return scanPound();
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

  function scanPound() {
    position++;
    scanSymbol();
    return Token.PoundSymbol;
  }

  function scanDot(): Token.Dot | NumberToken | SymbolToken {
    if (isDelimiter(input.charCodeAt(position + 1))) {
      position++;
      return Token.Dot;
    }
    return scanNumberOrSymbol();
  }

  // Annoyingly, from an implementor's perspective at least, Lisp's lexical
  // grammar does not make it possible to tell if something will be a number or
  // a symbol when the leading character is a digit or a plus or minus sign.
  //
  // For example:
  //  * `1`:  integer (positive one)
  //  * `+1`: integer (positive one)
  //  * `-1`: integer (negative one)
  //  * `1+`: symbol  (function to add 1)
  //  * `1-`: symbol  (function to subtract 1)
  //  * `+`:  symbol  (function to add)
  //  * `-`:  symbol  (function to add 1)
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
    if (isDigit(input.charCodeAt(position))) {
      do {
        if (!advance()) {
          return Token.Integer;
        }
      } while (isDigit(input.charCodeAt(position)));
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
          return numberToken;
        }
      } while (isDigit(input.charCodeAt(position)));
    }

    return atDelimiter() ? numberToken : scanSymbol();
  }

  function scanSymbol() {
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
      if (eof()) {
        throw error("Unexpected end of input in string literal.");
      }
      ch = input.charCodeAt(position);
    } while (ch !== CharCode.DoubleQuote);

    position++;
    return Token.String;
  }

  function skipLeadingTrivia() {
    skipTrivia(true);
  }

  function skipTrailingTrivia() {
    skipTrivia(false);
  }

  function skipTrivia(useHandler: boolean) {
    while (!eof(useHandler)) {
      const ch = input.charCodeAt(position);
      if (ch === CharCode.Semicolon) {
        skipComment();
        continue;
      }

      if (isWhitespace(ch)) {
        position++;
        continue;
      }

      break;
    }
  }

  function skipComment() {
    let ch: number;
    do {
      position++;
      ch = input.charCodeAt(position);
    } while (position < input.length && ch != CharCode.CarriageReturn && ch != CharCode.LineFeed);
  }

  function eof(useHandler = true) {
    if (useHandler && position === input.length) {
      eofHandler();
    }
    return position === input.length;
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
      input.charCodeAt(position + 1) != ch2 ||
      input.charCodeAt(position + 2) != ch3
    ) {
      return false;
    }

    position += 3;
    return true;
  }
}

//
// TODO: See how well optimized these are and consider making a table over all ascii chars.
//

function isDigit(ch: number) {
  return ch >= CharCode._0 && ch <= CharCode._9;
}

function isAsciiLetter(ch: number) {
  return (ch >= CharCode.A && ch <= CharCode.Z) || (ch >= CharCode.a && ch <= CharCode.z);
}

function isDelimiter(ch: number) {
  return isWhitespace(ch) || !isSymbolChar(ch);
}

function isSymbolChar(ch: number) {
  return (
    isAsciiLetter(ch) ||
    isDigit(ch) ||
    isSymbolPunctuation(ch) ||
    ch == CharCode.Backslash ||
    ch >= 128
  );
}

function isWhitespace(ch: number) {
  return (
    ch === CharCode.Space ||
    ch === CharCode.Tab ||
    ch === CharCode.CarriageReturn ||
    ch === CharCode.LineFeed
  );
}

function isSymbolPunctuation(ch: number) {
  return (
    ch === CharCode.Minus ||
    ch === CharCode.Plus ||
    ch === CharCode.Equal ||
    ch === CharCode.Asterisk ||
    ch === CharCode.Slash ||
    ch === CharCode.Underscore ||
    ch === CharCode.Tilde ||
    ch === CharCode.Bang ||
    ch === CharCode.At ||
    ch === CharCode.Dollar ||
    ch === CharCode.Percent ||
    ch === CharCode.Circumflex ||
    ch === CharCode.Ampersand ||
    ch === CharCode.Colon ||
    ch === CharCode.LessThan ||
    ch === CharCode.GreaterThan ||
    ch === CharCode.OpenBrace ||
    ch === CharCode.CloseBrace ||
    ch === CharCode.Question ||
    ch === CharCode.Dot
  );
}
