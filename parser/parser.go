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
	ps.addPrefixParseFn(token.LPAREN, ps.parseGroupedExpression)

	ps.addInfixParseFn(token.PLUS, ps.parseBinaryExpression)
	ps.addInfixParseFn(token.MINUS, ps.parseBinaryExpression)
	ps.addInfixParseFn(token.ASTERISK, ps.parseBinaryExpression)
	ps.addInfixParseFn(token.SLASH, ps.parseBinaryExpression)
	ps.addInfixParseFn(token.MODULUS, ps.parseBinaryExpression)
	ps.addInfixParseFn(token.POWER, ps.parseBinaryExpression)
	ps.addInfixParseFn(token.POS_PLUS, ps.parsePostfixExpression)
	ps.addInfixParseFn(token.POS_MINUS, ps.parsePostfixExpression)
	ps.addInfixParseFn(token.LESS, ps.parseBinaryExpression)
	ps.addInfixParseFn(token.GREATER, ps.parseBinaryExpression)
	ps.addInfixParseFn(token.LESS_EQ, ps.parseBinaryExpression)
	ps.addInfixParseFn(token.GREATER_EQ, ps.parseBinaryExpression)
	ps.addInfixParseFn(token.OR, ps.parseBinaryExpression)
	ps.addInfixParseFn(token.AND, ps.parseBinaryExpression)
	ps.addInfixParseFn(token.ASSIGN, ps.parseBinaryExpression)
	// ps.addInfixParseFn(token.DOT, ps.parseBinaryExpression) --> Member Access Operator
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

// TODO -> The value of _startIndex should be gotten from left
func (ps *Parser) parseBinaryExpression(left ast.Expression) ast.Expression {
	_startIndex := ps.currentToken.Loc.StartIndex
	expr := ast.BinaryExpression{
		Left: left,
		Op:   ps.currentToken.Literal,
	}
	operator := ps.currentToken.Type
	ps.advance()
	expr.Right = ps.prattParse(operator)
	expr.NodeLoc = ast.NodeLoc{
		NodeType:   "BinaryExpressionNode",
		StartIndex: _startIndex,
		EndIndex:   ps.currentToken.Loc.StartIndex + ps.currentToken.Loc.Advance,
	}
	return &expr
}

func (ps *Parser) parseGroupedExpression() ast.Expression {
	// Advance so that ps.currentToken is an expression
	ps.advance()
	expr := ps.prattParse(token.NILL)
	// Expect the next token to be token.RPAREN
	if ps.peekToken.Type != token.RPAREN {
		// Advance so that the next token is the 'illegal' token
		ps.printError(fmt.Sprintf("Expected ps.currentToken to be %s but got %s instead.\n", token.RPAREN, ps.currentToken.Type))
		return nil
	}
	// Advance so that ps.currentToken is token.RPAREN
	ps.advance()
	return expr
}

func (ps *Parser) parseReturnStatement() ast.Statement {
	_startIndex := ps.currentToken.Loc.StartIndex
	// Advance so that ps.currentToken is the begining of the expression
	ps.advance()
	expr := ps.prattParse(token.NILL)
	// Expect that ps.peekToken is token.SEMI_COLON
	if ps.peekToken.Type != token.SEMI_COLON {
		// Advance so that ps.currentToken is the 'illegal' token
		ps.advance()
		ps.printError(fmt.Sprintf("Expected ps.currentToken to be %s but got %s instead.\n", token.SEMI_COLON, ps.currentToken.Type))
		return nil
	}
	// Advance so that ps.currentToken is token.SEMI_COLON
	ps.advance()
	returnExpr := ast.ReturnStatementNode{
		Expression: expr,
		NodeLoc: ast.NodeLoc{
			NodeType:   "ReturnStatement",
			StartIndex: _startIndex,
			EndIndex:   ps.currentToken.Loc.StartIndex + ps.currentToken.Loc.Advance,
		},
	}
	return &returnExpr
}

// TODO -> The value of startIndex should be gotten from left
func (ps *Parser) parsePostfixExpression(left ast.Expression) ast.Expression {
	// ps.currentToken  is the operator token
	_startIndex := ps.currentToken.Loc.StartIndex
	postExpr := ast.PostfixExpression{
		Expression: left,
		Op:         *ps.currentToken,
		NodeLoc: ast.NodeLoc{
			NodeType:   "PosfixExpression",
			StartIndex: _startIndex,
			EndIndex:   ps.currentToken.Loc.StartIndex + ps.currentToken.Loc.Advance,
		},
	}
	return &postExpr
}

func (ps *Parser) parseBlockStatement() ast.Statement {
	blockStmt := ast.BlockStatementNode{
		Statements: []ast.Statement{},
	}
	for ps.peekToken.Type != token.EOF && ps.peekToken.Type != token.RCBRACE {
		stmt := ps.parseStatement()
		blockStmt.Statements = append(blockStmt.Statements, stmt)
	}
	return &blockStmt
}

// Expect peek checks if ps.peekToken is the correct token and advances
func (ps *Parser) expectPeek(t token.TokenType) bool {
	if ps.peekToken.Type != t {
		ps.advance()
		ps.printError(fmt.Sprintf("Expected ps.currentToken to be %s but found %s instead.\n", t, ps.currentToken.Type))
		return false
	}
	ps.advance()
	return true
}

func (ps *Parser) parseIfStatement() ast.Statement {
	// Expect ps.peekToken to be token.LPAREN
	if !ps.expectPeek(token.LPAREN) {
		return nil
	}
	// Advance so that ps.currentToken is pointing to the beginging of the testExpr
	ps.advance()
	testExpr := ps.prattParse(token.NILL)
	ifStmt := ast.IfStatementNode{
		Test: testExpr,
	}
	// Expect ps.peekToken to be token.RPAREN
	if !ps.expectPeek(token.RPAREN) {
		return nil
	}
	if !ps.expectPeek(token.LCBRACE) {
		return nil
	}
	// Chech if the blockStatement is empty i.e. if ps.peekToken.Type == token.RCBRACE
	if ps.peekToken.Type == token.RCBRACE {
		blockStmt := ast.BlockStatementNode{}
		ifStmt.Consequence = &blockStmt
	} else {
		// Advance so that ps.currentToken is in the beginnig of the blockStatement
		ps.advance()
		blockStmt := ps.parseBlockStatement()
		ifStmt.Consequence = blockStmt.(*ast.BlockStatementNode)
	}
	if !ps.expectPeek(token.RCBRACE) {
		return nil
	}
	// Check if there is an else statement
	if ps.peekToken.Type == token.ELSE {
		ps.advance()
		// Check if there is another 'if' statement for the 'if-else statement'
		if ps.peekToken.Type == token.IF {
			// Advance so that ps.currentToken is token.IF
			ps.advance()
			_blockStmt := ast.BlockStatementNode{
				Statements: []ast.Statement{},
			}
			_ifStmt := ps.parseIfStatement()
			_blockStmt.Statements = append(_blockStmt.Statements, _ifStmt)
			// Expect that ps.peekToken is token.RCBRACE
			ifStmt.Alternate = &_blockStmt
			return &ifStmt
		}
		// Expect ps.peekToken to be token.LCBRACE
		if !ps.expectPeek(token.LCBRACE) {
			return nil
		}
		// Check if the blockStatement is empty i.e. if ps.peekToken is token.RCBRACE
		if ps.peekToken.Type == token.RCBRACE {
			// ps.advance()
			_blockStmt := ast.BlockStatementNode{}
			ifStmt.Alternate = &_blockStmt
		} else {
			// Advance so that ps.currentToken is in the begining of the blockStatement
			ps.advance()
			_blockStmt := ps.parseBlockStatement()
			ifStmt.Alternate = _blockStmt.(*ast.BlockStatementNode)
		}
		// Expect that ps.peekToken is not token.EOF but token.RCBRACE
		if !ps.expectPeek(token.RCBRACE) {
			return nil
		}
	}
	return &ifStmt
}

func (ps *Parser) parseBreakStatement() ast.Statement {
	stmt := ast.BreakStatement{}
	// Expect that ps.peekToken is token.SEMI_COLON
	if !ps.expectPeek(token.SEMI_COLON) {
		return nil
	}
	return &stmt
}

func (ps *Parser) parseCaseBlockStatement() ast.BlockStatementNode {
	blockStmt := ast.BlockStatementNode{
		Statements: []ast.Statement{},
	}
	for {
		switch ps.peekToken.Type {
		case token.BREAK:
			// Advance so that ps.currentToken is token.BREAK
			ps.advance()
			brStmt := ps.parseBreakStatement()
			blockStmt.Statements = append(blockStmt.Statements, brStmt)
			return blockStmt
		case token.CASE:
			return blockStmt
		case token.DEFAULT:
			return blockStmt
		default:
			// Check if ps.peekToken is at the end of the switch statement
			if ps.peekToken.Type == token.RCBRACE {
				return blockStmt
			}
			// ps.currentToken is already at the begining of the Statement
			stmt := ps.parseStatement()
			blockStmt.Statements = append(blockStmt.Statements, stmt)
		}
	}
}

func (ps *Parser) parseSwitchCases() []ast.SwitchCase {
	_cases := []ast.SwitchCase{}
	if ps.peekToken.Type == token.CASE || ps.peekToken.Type == token.DEFAULT {
		for {
			switch ps.peekToken.Type {
			case token.CASE:
				switchCase := ast.SwitchCase{}
				// Advance so that ps.currentToken is token.CASE
				ps.advance()
				// Advance so that ps.currentToken is at the begining of the expression
				ps.advance()
				testExpr := ps.prattParse(token.NILL)
				// Expect that ps.peekToken is token.COLON
				if !ps.expectPeek(token.COLON) {
					return nil
				}
				// Parse the blockStatement for the consequnce
				// Advance so that ps.currentToken is at the begining of the blockStatemet
				ps.advance()
				blockStmt := ps.parseCaseBlockStatement()
				switchCase.TestExpr = testExpr
				switchCase.Consequence = &blockStmt
				_cases = append(_cases, switchCase)
			case token.DEFAULT:
				switchCase := ast.SwitchCase{}
				// Advance so that ps.currentToken is token.DEFAULT
				ps.advance()
				// Expect that ps.peekToken is token.COLON
				if !ps.expectPeek(token.COLON) {
					return nil
				}
				// Advance so that ps.currentToken is at the begining of the blockStatement
				ps.advance()
				blockStmt := ps.parseCaseBlockStatement()
				switchCase.TestExpr = nil
				switchCase.Consequence = &blockStmt
				_cases = append(_cases, switchCase)
			default:
				if ps.peekToken.Type == token.RCBRACE {
					// Advance so that ps.currentToken is at the end of the switch statement
					ps.advance()
					return _cases
				} else {
					ps.advance()
					ps.printError(fmt.Sprintf("Expecected ps.currentToken to be %s but got %s instead.\n", token.RCBRACE, ps.currentToken.Type))
					return nil
				}
			}
		}
	}
	// Expect that ps.peekToken is either token.CASE or token.DEFAULT
	ps.advance()
	ps.printError(fmt.Sprintf("Expected ps.currentToken to be either %s or %s but got %s instead/.\n", token.CASE, token.DEFAULT, ps.currentToken.Type))
	return nil
}

func (ps *Parser) parseSwithStatement() ast.Statement {
	if !ps.expectPeek(token.LPAREN) {
		return nil
	}
	// Advance so that ps.currentToken is at the begining of the testExpr
	ps.advance()
	testExpr := ps.prattParse(token.NILL)
	if !ps.expectPeek(token.RPAREN) {
		return nil
	}
	if !ps.expectPeek(token.LCBRACE) {
		return nil
	}
	// ps.peekToken should be either token.CASE or token.DEFAULT
	switchStmt := ast.SwitchStatement{
		TestExpr: testExpr,
	}
	switchStmt.Cases = ps.parseSwitchCases()
	return &switchStmt
}

func (ps *Parser) evalRightFirst(op token.TokenType) bool {
	peekTokenPrec, _ := precedence(ps.peekToken.Type)
	opPrec, opAss := precedence(op)

	if ps.peekToken.Type == op {
		if opAss == 'R' {
			return true
		}
	}
	if peekTokenPrec > opPrec {
		return true
	}
	return false
}

func (ps *Parser) parseForStatement() ast.Statement {
	// Expect that ps.peekToken is token.LPAREN
	if !ps.expectPeek(token.LPAREN) {
		return nil
	}
	// Advance so that ps.currentToken is at the begining of the Init Statement
	ps.advance()
	// Parse the first statement
	_initStmt := ps.parseStatement()
	// ps.currentToken is token.SEMI_COLON
	// Advance so that ps.currentToken is at the beginig of the TestExpression
	ps.advance()
	_testExpr := ps.prattParse(token.NILL)
	// Expect that ps.currentToken is token.SEMI_COLON
	if !ps.expectPeek(token.SEMI_COLON) {
		return nil
	}
	// Advance so that ps.currentToken is at the beginig of the UpdateExpresion
	ps.advance()
	_updateExpr := ps.prattParse(token.NILL)
	// Expect that ps.peekToken is token.RPAREN
	if !ps.expectPeek(token.RPAREN) {
		return nil
	}
	_body := ast.BlockStatementNode{}
	forStmt := ast.ForLoopStatement{
		Init:   _initStmt,
		Test:   _testExpr,
		Update: _updateExpr,
		Body:   &_body,
	}
	return &forStmt
}

func (ps *Parser) prattParse(prevOp token.TokenType) ast.Expression {
	parseFn, ok := ps.prefixParseFns[ps.currentToken.Type]
	if !ok {
		fmt.Printf("Error! No prefix parse func found for TokenType %s\n", ps.currentToken.Type)
		return nil
	}
	leftExpr := parseFn()
	for ps.evalRightFirst(prevOp) {
		parseFn, ok := ps.infixParseFns[ps.peekToken.Type]
		if !ok {
			fmt.Printf("Error! No infix parse func found for TokenType %s\n", ps.peekToken.Type)
		}
		// Adavace so that ps.currentToken is the operator
		ps.advance()
		leftExpr = parseFn(leftExpr)
	}
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
	// Advance so that ps.currentToke is token.SEMI_COLON
	ps.advance()
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

func (ps *Parser) ParseProgram() ast.Statement {
	pr := ast.Program{
		Statements: []ast.Statement{},
	}
	for ps.peekToken.Type != token.EOF {
		stmt := ps.parseStatement()
		pr.Statements = append(pr.Statements, stmt)
		// Advance so that ps.currentToken is at the begining of the next Statement
		ps.advance()
	}
	return &pr
}

func (ps *Parser) parseStatement() ast.Statement {
	switch ps.currentToken.Type {
	case token.LET:
		return ps.parseVariableDeclarationStatement()
	case token.VAR:
		return ps.parseVariableDeclarationStatement()
	case token.CONST:
		return ps.parseVariableDeclarationStatement()
	case token.RETURN:
		return ps.parseReturnStatement()
	case token.IF:
		return ps.parseIfStatement()
	case token.SWITCH:
		return ps.parseSwithStatement()
	case token.FOR:
		return ps.parseForStatement()
	default:
		return ps.parseExpressionStatement()
	}
}
