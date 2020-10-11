import { CharCode, EndOfInputError, Scanner, Token } from "./scanner";

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
  KnownSymbol,
} from "./primitives";

/** A value that can be expressed with S-Expression syntax. */
export type Datum = string | Sym | Num | List | Vector | false | null;

/** Reads one datum and advances scanner to next datum. */
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
        throw new EndOfInputError();
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
