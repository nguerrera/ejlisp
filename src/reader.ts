import { List, Float, Integer, Num, Vector, cons, intern, nil, list, error } from "./primitives";

export interface Scanner {
  readonly input: string;
  readonly position: number;
  readonly token: Token;
  readonly tokenPosition: number;
  scan(): Token;
  getTokenText(): string;
  appendInput(input: string): void;
}

/**
 * Any Lisp primitive type that has S-Expression syntax.
 */
export type Datum = string | symbol | Num | List | Vector;

export enum Token {
  None,
  Eof,
  OpenParenthesis,
  CloseParenthesis,
  OpenSquareBracket,
  CloseSquareBracket,
  Dot,
  Quote,
  Symbol,
  String,
  Number,
}

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
  Equal = 61,
  OpenSquareBracket = 91,
  CloseSquareBracket = 93,

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
      case Token.Number:
        return readNumber();
      case Token.Symbol:
        return readSymbol();
      case Token.OpenParenthesis:
        consume(Token.OpenParenthesis);
        return readList();
      case Token.OpenSquareBracket:
        consume(Token.OpenSquareBracket);
        return readVector();
      case Token.Quote:
        return readQuote();
      case Token.Eof:
        throw error("Unexpected end of file.");
      default:
        throw error(`Invalid token: ${Token[scanner.token]}`);
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

  function readNumber() {
    const text = scanner.getTokenText();
    const num = text.indexOf(".") >= 0 ? Float(Number(text)) : Integer(BigInt(text));
    consume(Token.Number);
    return num;
  }

  function readSymbol() {
    const sym = intern(scanner.getTokenText());
    consume(Token.Symbol);
    return sym;
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
    return currentToken == Token.None ? (currentToken = scanner.scan()) : currentToken;
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
  let token = Token.None;

  //prettier-ignore
  return {
    get position() { return position; },
    get token() {  return token; },
    get tokenPosition() { return tokenPosition; },
    get input() { return input; },
    scan,
    getTokenText,
    appendInput,
  }

  function scan(): Token {
    skipWhitespace();
    tokenPosition = position;
    return (token = scanToken());
  }

  function getTokenText() {
    return input.substring(tokenPosition, position);
  }

  function appendInput(moreInput: string) {
    input += moreInput;
  }

  function scanToken(): Token {
    const ch = input.charCodeAt(position);
    switch (ch) {
      case CharCode.OpenParenthesis:
        position++;
        return Token.OpenParenthesis;
      case CharCode.CloseParenthesis:
        position++;
        return Token.CloseParenthesis;
      case CharCode.Dot:
        position++;
        return Token.Dot;
      case CharCode.Quote:
        position++;
        return Token.Quote;
      case CharCode.OpenSquareBracket:
        position++;
        return Token.OpenSquareBracket;
      case CharCode.CloseSquareBracket:
        position++;
        return Token.CloseSquareBracket;
      case CharCode.DoubleQuote:
        return scanString();
      case CharCode.Plus:
      case CharCode.Minus:
        if (isNumberStart(input.charCodeAt(position + 1))) {
          return scanNumber();
        }
        break;
    }

    if (isNumberStart(ch)) {
      return scanNumber();
    } else if (isSymbolStart(ch)) {
      return scanSymbol();
    } else {
      throw error(`Invalid token: ${input.charAt(position)}`);
    }
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

  function scanSymbol() {
    let ch: number;
    do {
      position++;
      ch = input.charCodeAt(position);
    } while (position < input.length && isSymbolContinue(ch));

    return Token.Symbol;
  }

  function scanNumber() {
    let ch: number;
    do {
      position++;
      ch = input.charCodeAt(position);
    } while (position < input.length && isNumberContinue(ch));

    return Token.Number;
  }

  function skipWhitespace() {
    while (!eof()) {
      switch (input.charCodeAt(position)) {
        case CharCode.Tab:
        case CharCode.LineFeed:
        case CharCode.CarriageReturn:
        case CharCode.Space:
          position++;
          continue;
        default:
          return;
      }
    }
  }

  function eof() {
    if (position === input.length) {
      eofHandler();
    }
    return position === input.length;
  }
}

function isNumberStart(ch: number) {
  return ch >= CharCode._0 && ch <= CharCode._9;
}

function isNumberContinue(ch: number) {
  return isNumberStart(ch) || ch === CharCode.Dot;
}

function isSymbolStart(ch: number) {
  return (
    (ch >= CharCode.A && ch <= CharCode.Z) ||
    (ch >= CharCode.a && ch <= CharCode.z) ||
    ch === CharCode.Minus ||
    ch === CharCode.Plus ||
    ch === CharCode.Asterisk ||
    ch === CharCode.Slash ||
    ch === CharCode.Equal ||
    ch == CharCode.Colon
  );
}

function isSymbolContinue(ch: number) {
  return isSymbolStart(ch) || isNumberContinue(ch);
}
