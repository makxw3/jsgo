package lexer

import (
	"fmt"
	"jsgo/token"
)

type Lexer struct {
	input       string
	index       int  // The next character to be read
	char        byte // The byte that was last read by the lexer
	lineCount   int  // The index of the current line
	columnCount int  // The index of the current column
}

/** Error **/
type Error struct {
	Message string
}

func Get(src string) *Lexer {
	lx := Lexer{
		lineCount:   0,  //
		columnCount: -1, // -1 signifies no input
		index:       0,
		input:       src,
	}
	lx.nextChar()
	return &lx
}

// TODO: Solve the issue with the indexes so that the full value of identifiers is gotten
func (lx *Lexer) nextChar() {
	// The highest value that lx.index can have is len(lx.input)
	if lx.index > 0 {
		// Check if the last character to be read was '\n'
		if lx.input[lx.index-1] == '\n' {
			lx.lineCount++
			lx.columnCount = -1
		}
	}
	if lx.index >= len(lx.input) {
		lx.char = 0
	} else {
		lx.char = lx.input[lx.index]
		// The highest value of lx.columnCount will always point to the last valid value of lx.char
		// Just before token.EOF
		lx.columnCount++
		lx.index++
	}
}

func (lx *Lexer) peekChar() byte {
	if lx.index >= len(lx.input) {
		return 0
	}
	return lx.input[lx.index]
}

func (lx *Lexer) makeToken(advance int, tType token.TokenType) token.Token {
	var literal string = ""
	if len(lx.input) > 0 {
		literal = string(lx.input[lx.index-advance-1 : lx.index])
	}
	tokenLoc := token.TokenLoc{
		LineNumber:  lx.lineCount,
		ColumnStart: lx.columnCount - advance,
		ColumnEnd:   lx.columnCount,
		StartIndex:  lx.index - 1,
	}
	tok := token.Token{
		Loc:     &tokenLoc,
		Type:    tType,
		Literal: literal,
	}
	return tok
}

func (lx *Lexer) makeWordToken(advance int, word string, isKeyword bool, _type token.TokenType) token.Token {
	_columnEnd := 0
	_startIndex := 0
	var _tokenType token.TokenType = token.IDENTIFIER
	if isKeyword {
		_tokenType = _type
	}
	if lx.char == 0 {
		_columnEnd = lx.columnCount
		_startIndex = (lx.index - 1) - advance
	} else {
		_columnEnd = lx.columnCount - 1
		_startIndex = (lx.index - 2) - advance
	}
	tokenLoc := token.TokenLoc{
		LineNumber:  lx.lineCount,
		ColumnEnd:   _columnEnd,
		ColumnStart: _columnEnd - advance,
		StartIndex:  _startIndex,
	}
	tok := token.Token{
		Loc:     &tokenLoc,
		Type:    _tokenType,
		Literal: word,
	}
	return tok
}

func (lx *Lexer) makeNumberToken(advance int, number string) token.Token {
	_columnEnd := 0
	_startIndex := 0
	if lx.char == 0 {
		_columnEnd = lx.columnCount
		_startIndex = (lx.index - 1) - advance
	} else {
		_columnEnd = lx.columnCount - 1
		_startIndex = (lx.index - 2) - advance
	}

	tokenLoc := token.TokenLoc{
		LineNumber:  lx.lineCount,
		ColumnEnd:   _columnEnd,
		ColumnStart: _columnEnd - advance,
		StartIndex:  _startIndex,
	}

	token := token.Token{
		Loc:     &tokenLoc,
		Literal: number,
		Type:    token.NUMBER,
	}
	return token
}

/** Keywords **/
var keywords = map[string]token.TokenType{
	"function":  token.FUNCTION,
	"let":       token.LET,
	"var":       token.VAR,
	"const":     token.CONST,
	"undefined": token.UNDEFINED,
	"typeof":    token.TYPEOF,
	"do":        token.DO,
	"while":     token.WHILE,
	"if":        token.IF,
	"else":      token.ELSE,
	"switch":    token.SWITCH,
	"case":      token.CASE,
	"default":   token.DEFAULT,
	"return":    token.RETURN,
	"true":      token.TRUE,
	"false":     token.FALSE,
	"of":        token.OF,
	"in":        token.IN,
}

func (lx *Lexer) skipAllWhiteSpaces() {
	for lx.char == '\n' || lx.char == '\t' || lx.char == ' ' || lx.char == '\r' {
		lx.nextChar()
	}
}

func isLetter(char byte) bool {
	return char >= 'a' && char <= 'z' || char >= 'A' && char <= 'Z'
}

func isDigit(char byte) bool {
	return char >= '0' && char <= '9'
}

func (lx *Lexer) readWord() (string, int) {
	pos := lx.index - 1 // The index of lx.char
	advance := -1
	for isLetter(lx.char) {
		lx.nextChar()
		advance++
	}
	return lx.input[pos : pos+advance+1], advance
}

func (lx *Lexer) readString() (string, int, *Error) {
	advance := 0
	pos := lx.index
	lx.nextChar()
	advance++
	if lx.char != 0 {
		for lx.char != '"' {
			lx.nextChar()
			advance++
			if lx.char == 0 {
				_err := Error{Message: fmt.Sprintf("***** Lexing Error! Unexpected EOF char at index %d *****", lx.index)}
				return lx.input[pos : pos+advance-1], advance, &_err
			}
		}
	} else if lx.char == 0 {
		err := Error{Message: fmt.Sprintf("***** Lexing Error! Unexpected EOF char at index %d *****", lx.index)}
		return "", advance, &err
	}
	return lx.input[pos : pos+advance-1], advance, nil
}

func (lx *Lexer) readNumber() (string, int) {
	pos := lx.index - 1
	count := -1
	for isDigit(lx.char) {
		lx.nextChar()
		count++
	}
	return lx.input[pos : pos+count+1], count
}

func (lx *Lexer) makeStringToken(advance int, _string string) token.Token {
	tokenLoc := token.TokenLoc{
		LineNumber:  lx.lineCount,
		ColumnEnd:   lx.columnCount,
		ColumnStart: lx.columnCount - advance,
		StartIndex:  lx.index - advance,
	}
	token := token.Token{
		Literal: _string,
		Type:    token.STRING,
		Loc:     &tokenLoc,
	}
	return token
}

func (lx *Lexer) makeStrIllegalToken(advance int, _string string) token.Token {
	tokenLoc := token.TokenLoc{
		LineNumber:  lx.lineCount,
		ColumnEnd:   lx.columnCount,
		ColumnStart: lx.columnCount - (advance - 1),
		StartIndex:  lx.index - advance,
	}
	token := token.Token{
		Type:    token.ILLEGAL,
		Literal: "\"" + _string,
		Loc:     &tokenLoc,
	}
	return token
}

func (lx *Lexer) NextToken() *token.Token {
	lx.skipAllWhiteSpaces()
	var tok token.Token
	switch lx.char {
	case '+':
		if lx.peekChar() == '=' {
			lx.nextChar()
			tok = lx.makeToken(1, token.EQ_PLUS)
		} else if lx.peekChar() == '+' {
			lx.nextChar()
			tok = lx.makeToken(1, token.POS_PLUS)
		} else {
			tok = lx.makeToken(0, token.PLUS)
		}
	case '-':
		if lx.peekChar() == '=' {
			lx.nextChar()
			tok = lx.makeToken(1, token.EQ_MINUS)
		} else if lx.peekChar() == '-' {
			tok = lx.makeToken(1, token.POS_MINUS)
		} else {
			tok = lx.makeToken(0, token.MINUS)
		}
	case '*':
		if lx.peekChar() == '=' {
			lx.nextChar()
			tok = lx.makeToken(1, token.EQ_ASTERISK)
		} else {
			tok = lx.makeToken(0, token.ASTERISK)
		}
	case '/':
		if lx.peekChar() == '=' {
			lx.nextChar()
			tok = lx.makeToken(1, token.EQ_SLASH)
		} else {
			tok = lx.makeToken(0, token.SLASH)
		}
	case '^':
		if lx.peekChar() == '=' {
			lx.nextChar()
			tok = lx.makeToken(1, token.EQ_POWER)
		} else {
			tok = lx.makeToken(0, token.POWER)
		}
	case '%':
		if lx.peekChar() == '=' {
			lx.nextChar()
			tok = lx.makeToken(1, token.EQ_MODULUS)
		} else {
			tok = lx.makeToken(0, token.MODULUS)
		}
	case '(':
		tok = lx.makeToken(0, token.LPAREN)
	case ')':
		tok = lx.makeToken(0, token.RPAREN)
	case '{':
		tok = lx.makeToken(0, token.LCBRACE)
	case '}':
		tok = lx.makeToken(0, token.RCBRACE)
	case '.':
		if lx.peekChar() == '.' {
			lx.nextChar()
			if lx.peekChar() == '.' {
				lx.nextChar()
				tok = lx.makeToken(2, token.SPREAD)
			} else {
				tok = lx.makeToken(1, token.ILLEGAL)
			}
		} else {
			tok = lx.makeToken(0, token.DOT)
		}
	case ',':
		tok = lx.makeToken(0, token.COMMA)
	case '?':
		tok = lx.makeToken(0, token.Q_MARK)
	case ':':
		tok = lx.makeToken(0, token.COLON)
	case ';':
		tok = lx.makeToken(0, token.SEMI_COLON)
	case '[':
		tok = lx.makeToken(0, token.LSBRACE)
	case ']':
		tok = lx.makeToken(0, token.RSBRACE)
	case '<':
		tok = lx.makeToken(0, token.LESS)
	case '>':
		tok = lx.makeToken(0, token.GREATER)
	case '!':
		if lx.peekChar() == '=' {
			lx.nextChar()
			tok = lx.makeToken(1, token.NOT_EQ)
		} else {
			tok = lx.makeToken(0, token.NOT)
		}
	case '=':
		if lx.peekChar() == '=' {
			lx.nextChar()
			tok = lx.makeToken(1, token.EQ)
		} else {
			tok = lx.makeToken(0, token.ASSIGN)
		}
	case '$':
		tok = lx.makeToken(0, token.TEMPLATE)
	case '&':
		if lx.peekChar() == '&' {
			lx.nextChar()
			tok = lx.makeToken(1, token.AND)
		} else {
			tok = lx.makeToken(0, token.ILLEGAL)
		}
	case '|':
		if lx.peekChar() == '|' {
			lx.nextChar()
			tok = lx.makeToken(1, token.OR)
		} else {
			tok = lx.makeToken(0, token.ILLEGAL)
		}
	case '"':
		_string, advance, err := lx.readString()
		if err != nil {
			fmt.Println(err.Message)
			tok = lx.makeStrIllegalToken(advance, _string)
		} else {
			tok = lx.makeStringToken(advance, _string)
		}
	case 0:
		tok = lx.makeToken(0, token.EOF)
	default:
		if isLetter(lx.char) {
			word, advance := lx.readWord()
			if _type, ok := keywords[word]; ok {
				tok := lx.makeWordToken(advance, word, true, _type)
				return &tok
			}
			tok = lx.makeWordToken(advance, word, false, token.IDENTIFIER)
			return &tok
		} else if isDigit(lx.char) {
			number, adavance := lx.readNumber()
			tok := lx.makeNumberToken(adavance, number)
			return &tok
		} else {
			tok = lx.makeToken(0, token.ILLEGAL)
		}
	}
	lx.nextChar()
	return &tok
}
