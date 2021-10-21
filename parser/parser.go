package parser

import (
	"fmt"
	"jsgo/ast"
	"jsgo/lexer"
	"jsgo/token"
	"strconv"
)

type Parser struct {
	lexer          *lexer.Lexer
	currentToken   *token.Token
	peekToken      *token.Token
	prefixParseFns map[token.TokenType]tPrefixParseFn
	infixParseFns  map[token.TokenType]tInfixParseFn
}

func Get(lx *lexer.Lexer) *Parser {
	ps := Parser{
		lexer: lx,
	}
	ps.advance()
	ps.advance()
	ps.prefixParseFns = make(map[token.TokenType]tPrefixParseFn)
	ps.infixParseFns = make(map[token.TokenType]tInfixParseFn)

	ps.addPrefixParseFn(token.NUMBER, ps.parseIntegerLiteral)
	ps.addPrefixParseFn(token.IDENTIFIER, ps.parseIdentifierLiteral)
	ps.addPrefixParseFn(token.TRUE, ps.parseBooleanLiteral)
	ps.addPrefixParseFn(token.FALSE, ps.parseBooleanLiteral)
	ps.addPrefixParseFn(token.STRING, ps.parseStringLiteral)
	ps.addPrefixParseFn(token.NOT, ps.parseLogicalNotOperator)
	return &ps
}

func (ps *Parser) advance() {
	ps.currentToken = ps.peekToken
	ps.peekToken = ps.lexer.NextToken()
}

func precedence(op token.TokenType) (int, byte) {
	switch op {
	case token.ASSIGN:
		return 10, 'R'
	case token.EQ_PLUS:
		return 10, 'R'
	case token.EQ_MINUS:
		return 10, 'R'
	case token.EQ_ASTERISK:
		return 10, 'R'
	case token.EQ_SLASH:
		return 10, 'R'
	case token.EQ_MODULUS:
		return 10, 'R'
	case token.EQ_POWER:
		return 10, 'R'
	case token.Q_MARK:
		return 20, 'R' // -> Start evaluating the tannery operators from the right
	case token.OR:
		return 30, 'L'
	case token.AND:
		return 40, 'L'
	case token.EQ:
		return 50, 'L'
	case token.NOT_EQ:
		return 50, 'L'
	case token.LESS:
		return 60, 'L'
	case token.LESS_EQ:
		return 60, 'L'
	case token.GREATER:
		return 60, 'L'
	case token.GREATER_EQ:
		return 60, 'L'
	case token.IN:
		return 60, 'L'
	case token.PLUS:
		return 70, 'L'
	case token.MINUS:
		return 70, 'L'
	case token.ASTERISK:
		return 80, 'L'
	case token.SLASH:
		return 80, 'L'
	case token.MODULUS:
		return 80, 'L'
	case token.POWER:
		return 90, 'R'
	case token.NOT:
		return 100, 'R'
	case token.TYPEOF:
		return 100, 'R'
	case token.POS_PLUS:
		return 110, 'L'
	case token.POS_MINUS:
		return 110, 'L'
	case token.LPAREN: // -> For function calls in '(') or for grouping in 1 + (2 * 4)
		return 120, 'L'
	case token.LSBRACE: // -> For computed member access in obj["name"] or array[2]
		return 120, 'L'
	case token.DOT:
		return 120, 'L' // -> The dot member access
	default:
		return 0, 'L'
	}
}

type (
	tPrefixParseFn func() ast.Expression
	tInfixParseFn  func(ast.Expression) ast.Expression
)

func (ps *Parser) addPrefixParseFn(t token.TokenType, fn tPrefixParseFn) {
	ps.prefixParseFns[t] = fn
}

func (ps *Parser) addInfixParseFn(t token.TokenType, fn tInfixParseFn) {
	ps.infixParseFns[t] = fn
}

func (ps *Parser) printError(msg string) {
	fmt.Printf("Error! L: %d [%d,%d] --> %s", ps.currentToken.Loc.LineNumber, ps.currentToken.Loc.ColumnStart, ps.currentToken.Loc.ColumnEnd, msg)
}

func (ps *Parser) parseIntegerLiteral() ast.Expression {
	value, err := strconv.ParseInt(ps.currentToken.Literal, 0, 64)
	if err != nil {
		ps.printError(fmt.Sprintf("Could not parse the inetger %s\n", ps.currentToken.Literal))
		return nil
	}
	expr := ast.IntegerLiteralNode{
		Value: value,
		NodeLoc: ast.NodeLoc{
			StartIndex: ps.currentToken.Loc.StartIndex,
			EndIndex:   ps.currentToken.Loc.StartIndex + ps.currentToken.Loc.Advance,
			NodeType:   "IntegerLiteralNode",
		},
	}
	return &expr
}

func (ps *Parser) parseIdentifierLiteral() ast.Expression {
	expr := ast.IndentifierNode{
		Name: ps.currentToken.Literal,
		NodeLoc: ast.NodeLoc{
			StartIndex: ps.currentToken.Loc.StartIndex,
			EndIndex:   ps.currentToken.Loc.StartIndex + ps.currentToken.Loc.Advance,
			NodeType:   "IdentifierNode",
		},
	}
	return &expr
}

func (ps *Parser) parseBooleanLiteral() ast.Expression {
	value, err := strconv.ParseBool(ps.currentToken.Literal)
	if err != nil {
		ps.printError(fmt.Sprintf("Could not parse the boolean %v\n", ps.currentToken.Literal))
		return nil
	}
	expr := ast.BooleanNode{
		Value: value,
		NodeLoc: ast.NodeLoc{
			StartIndex: ps.currentToken.Loc.StartIndex,
			EndIndex:   ps.currentToken.Loc.StartIndex + ps.currentToken.Loc.Advance,
			NodeType:   "BooleanLiteralNode",
		},
	}
	return &expr
}

func (ps *Parser) parseStringLiteral() ast.Expression {
	expr := ast.StringLiteralNode{
		Value: ps.currentToken.Literal,
		NodeLoc: ast.NodeLoc{
			NodeType:   "StringLiteralNode",
			StartIndex: ps.currentToken.Loc.StartIndex,
			EndIndex:   ps.currentToken.Loc.StartIndex + ps.currentToken.Loc.Advance,
		},
	}
	return &expr
}

func (ps *Parser) parseVariableDeclarationStatement() ast.Statement {
	kind := ps.currentToken.Type
	_startIndex := ps.currentToken.Loc.StartIndex
	_declarations, _endIndex := ps.parseVariableDeclarations()
	expr := ast.VariableDeclarationNode{
		Kind:         kind,
		Declarations: _declarations,
		NodeLoc: ast.NodeLoc{
			NodeType:   "VariableDeclarationNode",
			StartIndex: _startIndex,
			EndIndex:   _endIndex,
		},
	}
	return &expr
}

// int is the end index
func (ps *Parser) parseVariableDeclarations() ([]ast.VariableDeclaratorNode, int) {
	var variableDeclarations = []ast.VariableDeclaratorNode{}
	// Expect the ps.peekToken == token.IDENTIFIER
	if ps.peekToken.Type != token.IDENTIFIER {
		// Move ps.currentToken to ps.peekToken
		ps.advance()
		ps.printError(fmt.Sprintf("Expected ps.currentToken.Type to be %s but found %s instead.\n", token.IDENTIFIER, ps.currentToken.Type))
		_endIndex := ps.currentToken.Loc.StartIndex + ps.currentToken.Loc.Advance
		return nil, _endIndex
	}
	// Move to the next token so that ps.currentToken is token.IDENTIFIER
	ps.advance()
	// Move inside the for loop and always check is ps.peekToken is either ',' or ';' or '='
	for {
		switch ps.peekToken.Type {
		case token.COMMA:
			// parse the identifier
			name := ps.parseIdentifierLiteral()
			decl := ast.VariableDeclaratorNode{
				Identifier: name.(*ast.IndentifierNode),
				Init:       nil,
			}
			variableDeclarations = append(variableDeclarations, decl)
			// Advance so that ps.currentToken is token.COMMA
			ps.advance()
			// Expect the next token to be an identifer
			if ps.peekToken.Type != token.IDENTIFIER {
				// Advance so that ps.currentToken is the 'illegal' token
				ps.advance()
				ps.printError(fmt.Sprintf("Expected ps.currentToken.Type to be %s but got %s instead.\n", token.IDENTIFIER, ps.currentToken.Type))
				_endIndex := ps.currentToken.Loc.StartIndex + ps.currentToken.Loc.Advance
				return nil, _endIndex
			}
			// If ps.peekToken.Type is token.IDENTIFIER then advance so that ps.currentToken is token.IDENTIFIER
			ps.advance()
		case token.ASSIGN:
			name := ps.parseIdentifierLiteral()
			ps.advance()
			// Expect ps.peekToken.Type != token.EOF
			if ps.peekToken.Type == token.EOF || ps.peekToken.Type == token.ILLEGAL {
				ps.advance()
				ps.printError(fmt.Sprintf("Expected an Expression but found %s instead.\n", ps.currentToken.Type))
				_endIndex := ps.currentToken.Loc.StartIndex + ps.currentToken.Loc.Advance
				return nil, _endIndex
			}
			ps.advance()
			// The next part should be an expression and thus any semantic or syntax errors that may arrise should
			// be handled with the parsing functions
			expr := ps.prattParse(token.NILL)
			decl := ast.VariableDeclaratorNode{
				Identifier: name.(*ast.IndentifierNode),
				Init:       expr,
			}
			variableDeclarations = append(variableDeclarations, decl)
			// Expect ps.peekToken.Type == token.SEMI_COLON
			if ps.peekToken.Type != token.SEMI_COLON {
				ps.advance()
				ps.printError(fmt.Sprintf("Expected ps.currentToken.Type to be %s but found %s instead.\n", token.SEMI_COLON, ps.currentToken.Type))
				_endIndex := ps.currentToken.Loc.StartIndex + ps.currentToken.Loc.Advance
				return nil, _endIndex
			}
			// Advance so that ps.currentToken.Type == token.SEMI_COLON
			ps.advance()
			// Return the variableDeclaration lists
			_endIndex := ps.currentToken.Loc.StartIndex + ps.currentToken.Loc.Advance
			return variableDeclarations, _endIndex
		case token.SEMI_COLON:
			name := ps.parseIdentifierLiteral()
			decl := ast.VariableDeclaratorNode{
				Identifier: name.(*ast.IndentifierNode),
				Init:       nil,
			}
			variableDeclarations = append(variableDeclarations, decl)
			// Advance so that ps.currentToken.Type == token.SEMI_COLON
			ps.advance()
			_endIndex := ps.currentToken.Loc.StartIndex + ps.currentToken.Loc.Advance
			return variableDeclarations, _endIndex
		default:
			// This means that ps.peekToken.Type != ',' or ';' or '='
			// Advance so that ps.currentToken is the 'illegal' token
			ps.advance()
			ps.printError(fmt.Sprintf("Expected ps.currentToken to be either ',' or ';' or '=' but found %s instead.\n", ps.currentToken.Type))
			_endIndex := ps.currentToken.Loc.StartIndex + ps.currentToken.Loc.Advance
			return nil, _endIndex
		}
	}
}

func (ps *Parser) parseLogicalNotOperator() ast.Expression {
	// Adavance so that ps.currentToken is the first token in the expression
	ps.advance()
	parsedExpr := ps.prattParse(token.NOT)
	expr := ast.LogicalNotExpression{
		Value: parsedExpr,
	}
	return &expr
}

func (ps *Parser) prattParse(prevOp token.TokenType) ast.Expression {
	parseFn, ok := ps.prefixParseFns[ps.currentToken.Type]
	if !ok {
		fmt.Printf("Error! No prefix parse func found for the token-type %s\n", ps.currentToken.Type)
		return nil
	}
	leftExpr := parseFn()
	return leftExpr
}

func (ps *Parser) parseExpressionStatement() ast.Statement {
	_startIndex := ps.currentToken.Loc.StartIndex
	expr := ps.prattParse(token.NILL)
	// After every statement there should be a semi-colon
	if ps.peekToken.Type != token.SEMI_COLON {
		ps.advance()
		ps.printError(fmt.Sprintf("Expected ps.currentToken.Type to be %s but got %s instead.\n", token.SEMI_COLON, ps.currentToken.Type))
		return nil
	}
	exprStmt := ast.ExpressionStatement{
		Expression: expr,
		NodeLoc: ast.NodeLoc{
			NodeType:   "ExpressionStatement",
			StartIndex: _startIndex,
			EndIndex:   ps.currentToken.Loc.StartIndex + ps.currentToken.Loc.Advance,
		},
	}
	return &exprStmt
}

func (ps *Parser) ParseStatement() ast.Statement {
	switch ps.currentToken.Type {
	case token.LET:
		return ps.parseVariableDeclarationStatement()
	case token.VAR:
		return ps.parseVariableDeclarationStatement()
	case token.CONST:
		return ps.parseVariableDeclarationStatement()
	default:
		return ps.parseExpressionStatement()
	}
}
