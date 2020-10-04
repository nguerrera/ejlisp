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

  NextLine = 0x0085,
  LeftToRightMark = 0x200E,
  RightToLeftMark = 0x200F,
  LineSeparator = 0x2028,
  ParagraphSeparator = 0x2029,

  A = 65, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
  a = 97, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z,
  _0 = 48, _1, _2, _3, _4, _5, _6, _7, _8, _9,
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
