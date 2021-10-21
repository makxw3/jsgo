package token

import (
	"fmt"
	"strconv"
)

type TokenType string

// The location of the token
type TokenLoc struct {
	LineNumber  int // The line at which the token is
	ColumnStart int // The index from the begining of the line at which the first character of the token is
	ColumnEnd   int // The index from the begining of the line at which the last character of the token is
	StartIndex  int // The index where the token begings in the input
	Advance     int // The number of chars from the first char of the token to the last char of the token
}

type Token struct {
	Type    TokenType // The type of the token
	Literal string    // The literal value of the token
	Loc     *TokenLoc // The location of the token
}

// Supported TokenType(s)
const (
	// Special
	NILL    = "NILL"
	ILLEGAL = "ILLEGAL" // Any unsopported token
	EOF     = "EOF"     // End-Of-File Token
	// Identifiers
	IDENTIFIER = "IDENTIFIER"
	// Numbers
	NUMBER = "NUMBER"
	// Strings
	STRING = "STRING"
	// Mathematical Operators
	PLUS     = "PLUS"     // +
	MINUS    = "MINUS"    // -
	ASTERISK = "ASTERISK" // *
	POWER    = "POWER"    // ^
	MODULUS  = "MODULUS"  // %
	SLASH    = "SLASH"    // /
	// Assignment Mathematical Operators
	EQ_PLUS     = "EQ_PLUS"     // +=
	EQ_MINUS    = "EQ_MINUS"    // -=
	EQ_ASTERISK = "EQ_ASTERISK" // *=
	EQ_POWER    = "EQ_POWER"    // ^=
	EQ_MODULUS  = "EQ_MODULUS"  // %=
	EQ_SLASH    = "EQ_SLASH"    // /=
	// POSTFIX OPERATORS
	POS_PLUS  = "POS_PLUS"  // ++
	POS_MINUS = "POS_MINUS" // --
	// Boolean Operators
	AND    = "AND"    // &&
	OR     = "OR"     // ||
	EQ     = "EQ"     // ==
	NOT_EQ = "NOT_EQ" // !=
	// Spread Operator
	SPREAD = "SPREAD" // ...
	// Keywords
	LET       = "LET"
	VAR       = "VAR"
	CONST     = "CONST"
	UNDEFINED = "UNDEFINED"
	TYPEOF    = "TYPEOF"
	DO        = "DO"
	WHILE     = "WHILE"
	IF        = "IF"
	ELSE      = "ELSE"
	SWITCH    = "SWITCH"
	CASE      = "CASE"
	DEFAULT   = "DEFAULT"
	RETURN    = "RETURN"
	FUNCTION  = "FUNCTION"
	TRUE      = "TRUE"
	FALSE     = "FALSE"
	OF        = "OF" // for of
	IN        = "IN" // for in
	// Symbols
	LPAREN     = "LPAREN"        // (
	RPAREN     = "RPAREN"        // )
	LCBRACE    = "LCBRACE"       // {
	RCBRACE    = "RCBRACE"       // }
	DOT        = "DOT"           // . -> Rename to Member Access Operator
	COMMA      = "COMMA"         // ,
	Q_MARK     = "Q_MARK"        // ?
	COLON      = "COLON"         // :
	SEMI_COLON = "SEMI_COLON"    // ;
	LSBRACE    = "LSBRACE"       // [
	RSBRACE    = "RSBRACE"       // ]
	LESS       = "LESS"          // <
	LESS_EQ    = "LESS OR EQ"    // <=
	GREATER    = "GREATER"       // >
	GREATER_EQ = "GREATER OR EQ" // >=
	ASSIGN     = "ASSIGN"        // =
	TEMPLATE   = "TEMPLATE"      // $
	NOT        = "NOT"           // !
	ARROW      = "ARROW"         // =>
)

// To 3 digits e.g. from '5' -> '005'
func to3d(num int) string {
	res := strconv.Itoa(num)
	for len(res) != 3 {
		res = "0" + res
	}
	return res
}

func (tk *Token) Print() {
	if tk.Loc != nil {
		fmt.Printf("#L %s [CS %s:CE %s] #I %s -> <%s:%s>\n", to3d(tk.Loc.LineNumber), to3d(tk.Loc.ColumnStart), to3d(tk.Loc.ColumnEnd), to3d(tk.Loc.StartIndex), tk.Type, "'"+tk.Literal+"'")
	}
}
