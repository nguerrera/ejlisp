import { error } from "./primitives";

export interface Scanner {
  readonly input: string;
  readonly position: number;
  readonly token: Token;
  readonly tokenStart: number;
  readonly tokenEnd: number;
  scan(): Token;
  getTokenText(): string;
}

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
  VerticalTab = 11,
  FormFeed = 12, 
  CarriageReturn = 13,
  Space = 32,
  Bang = 33,
  DoubleQuote = 34,
  Sharp = 35,
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
  OpenBracket = 91,
  Backslash = 92,
  CloseBracket = 93,
  Circumflex = 94,
  Underscore = 95,
  BackQuote = 96,
  OpenBrace = 123,
  CloseBrace = 125,
  Tilde = 126,

  NextLine = 0x0085,
  LeftToRightMark = 0x200E,
  RightToLeftMark = 0x200F,
  LineSeparator = 0x2028,
  ParagraphSeparator = 0x2029,

  A = 65, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
  a = 97, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z,
  _0 = 48, _1, _2, _3, _4, _5, _6, _7, _8, _9,
}

export class EndOfInputError extends Error {
  constructor() {
    super("Unexpected end of input.");
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
        throw new EndOfInputError();
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

//
// TODO: See how well optimized these are and consider making a table over all ascii chars.
//

export function isDigit(ch: number) {
  return ch >= CharCode._0 && ch <= CharCode._9;
}

export function isAsciiLetter(ch: number) {
  return (ch >= CharCode.A && ch <= CharCode.Z) || (ch >= CharCode.a && ch <= CharCode.z);
}

export function isDelimiter(ch: number) {
  return isWhitespace(ch) || !isSymbolChar(ch);
}

export function isSymbolChar(ch: number) {
  return (
    isAsciiLetter(ch) ||
    isDigit(ch) ||
    isSymbolPunctuation(ch) ||
    ch === CharCode.Backslash ||
    ch >= 128
  );
}

/**
 * Determines if the given UTF-16 code unit represents whitespace.
 *
 * Whitespace is defined by the full, immutable set of code points
 * with Pattern_White_Space property.
 *
 * References:
 *  https://www.unicode.org/Public/UCD/latest/ucd/PropList.txt
 *  https://www.unicode.org/reports/tr31/#Stability
 */
export function isWhitespace(ch: number) {
  return (
    ch === CharCode.Space ||
    ch === CharCode.Tab ||
    ch === CharCode.CarriageReturn ||
    ch === CharCode.LineFeed ||
    ch === CharCode.FormFeed ||
    ch === CharCode.VerticalTab ||
    ch === CharCode.NextLine ||
    ch === CharCode.LeftToRightMark ||
    ch === CharCode.RightToLeftMark ||
    ch === CharCode.LineSeparator ||
    ch === CharCode.ParagraphSeparator
  );
}

export function isSymbolPunctuation(ch: number) {
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
