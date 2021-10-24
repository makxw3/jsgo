package ast

import (
	"bytes"
	"fmt"
	"jsgo/token"
)

type Node interface {
	NodeStr() string // Prints out the node as a string
}

type Statement interface {
	Node
	StatementNode() //
}

type Expression interface {
	Node
	ExpressionNode() //
}

type NodeLoc struct {
	NodeType   string // The type of Node
	StartIndex int    // The start index of the first token
	EndIndex   int    // The end index of the last token
}

/** The Program is the root of the ast**/
type Program struct {
	NodeLoc
	Statements []Statement
}

func (pr *Program) StatementNode() {}
func (pr *Program) NodeStr() string {
	var out bytes.Buffer
	for _, stmt := range pr.Statements {
		if stmt != nil {
			out.WriteString(stmt.NodeStr() + "\n")
		}
	}
	return out.String()
}

/** Identifier **/
type IndentifierNode struct {
	NodeLoc
	Name string // The identifier literal
}

func (id *IndentifierNode) ExpressionNode() {}
func (id *IndentifierNode) NodeStr() string {
	return id.Name
}

/** IntergerLiteralNode **/
type IntegerLiteralNode struct {
	NodeLoc
	Value int64
}

func (il *IntegerLiteralNode) NodeStr() string {
	return fmt.Sprintf("%d", il.Value)
}

func (il *IntegerLiteralNode) ExpressionNode() {}

/** Booleans **/
type BooleanNode struct {
	NodeLoc
	Value bool
}

func (bl *BooleanNode) ExpressionNode() {}
func (bl *BooleanNode) NodeStr() string {
	return fmt.Sprintf("%v", bl.Value)
}

/** Strings **/
type StringLiteralNode struct {
	NodeLoc
	Value string
}

func (sl *StringLiteralNode) ExpressionNode() {}
func (sl *StringLiteralNode) NodeStr() string {
	return "\"" + sl.Value + "\""
}

/** Variable Declarations **/
type VariableDeclaratorNode struct {
	NodeLoc
	Identifier *IndentifierNode
	Init       Expression
}

func (vd *VariableDeclaratorNode) StatementNode() {}
func (vd *VariableDeclaratorNode) NodeStr() string {
	var out bytes.Buffer
	out.WriteString(vd.Identifier.Name)
	if vd.Init != nil {
		out.WriteString("=")
		out.WriteString(vd.Init.NodeStr())
	}
	return out.String()
}

type VariableDeclarationNode struct {
	NodeLoc
	Declarations []VariableDeclaratorNode
	Kind         token.TokenType // The kind of declaration i.e. let or var or const
}

func (vd *VariableDeclarationNode) StatementNode() {}
func (vd *VariableDeclarationNode) NodeStr() string {
	var out bytes.Buffer
	out.WriteString(string(vd.Kind) + " ")
	for i, decl := range vd.Declarations {
		out.WriteString(decl.NodeStr())
		if i != len(vd.Declarations)-1 {
			out.WriteString(",")
		}
	}
	return out.String()
}

/** Logial Not Operator **/
type LogicalNotExpression struct {
	Value Expression
}

func (lne *LogicalNotExpression) ExpressionNode() {}
func (lne *LogicalNotExpression) NodeStr() string {
	var out bytes.Buffer
	out.WriteString("!")
	out.WriteString("(")
	out.WriteString(lne.Value.NodeStr())
	out.WriteString(")")
	return out.String()
}

/** Expression Statement **/
type ExpressionStatement struct {
	NodeLoc
	Expression Expression
}

func (es *ExpressionStatement) StatementNode() {}
func (es *ExpressionStatement) NodeStr() string {
	if es.Expression != nil {
		return es.Expression.NodeStr()
	}
	return ""
}

/** Binary Expressions **/
type BinaryExpression struct {
	NodeLoc
	Left  Expression
	Op    string
	Right Expression
}

func (be *BinaryExpression) ExpressionNode() {}
func (be *BinaryExpression) NodeStr() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(be.Left.NodeStr())
	out.WriteString(be.Op)
	out.WriteString(be.Right.NodeStr())
	out.WriteString(")")
	return out.String()
}

/** Return Statements **/
type ReturnStatementNode struct {
	NodeLoc
	Expression Expression
}

func (rs *ReturnStatementNode) StatementNode() {}
func (rs *ReturnStatementNode) NodeStr() string {
	var out bytes.Buffer
	out.WriteString("return ")
	if rs.Expression != nil {
		out.WriteString(rs.Expression.NodeStr())
	}
	return out.String()
}

/** Use prefix parsing functions to parse post-fix operators **/
type PostfixExpression struct {
	NodeLoc
	Op         token.Token // The postfix operator
	Expression Expression
}

func (pe *PostfixExpression) ExpressionNode() {}
func (pe *PostfixExpression) NodeStr() string {
	var out bytes.Buffer
	out.WriteString("(")
	if pe.Expression != nil {
		out.WriteString(pe.Expression.NodeStr())
	}
	out.WriteString(")")
	out.WriteString(pe.Op.Literal)
	return out.String()
}

/** Block Statement **/
type BlockStatementNode struct {
	Statements []Statement
}

func (bl *BlockStatementNode) StatementNode() {}
func (bl *BlockStatementNode) NodeStr() string {
	var out bytes.Buffer
	for _, stmt := range bl.Statements {
		out.WriteString(stmt.NodeStr())
	}
	return out.String()
}

/** IF ELSE Statements **/
type IfStatementNode struct {
	NodeLoc
	Test        Expression // The expression that should be tested
	Consequence *BlockStatementNode
	Alternate   *BlockStatementNode
}

func (ifs *IfStatementNode) StatementNode() {}
func (ifs *IfStatementNode) NodeStr() string {
	var out bytes.Buffer
	out.WriteString("if (")
	out.WriteString(ifs.Test.NodeStr())
	out.WriteString(") {")
	out.WriteString(ifs.Consequence.NodeStr())
	out.WriteString("}")
	if ifs.Alternate != nil {
		out.WriteString(" else {")
		out.WriteString(ifs.Alternate.NodeStr())
		out.WriteString("}")
	}
	return out.String()
}

/** Switch Case Statements **/
// default is also case but with a null testExpr and a null Conseqeunce
type SwitchCase struct {
	NodeLoc
	TestExpr    Expression // The expression for that case i.e. case '<expr>':
	Consequence *BlockStatementNode
}

func (sc *SwitchCase) StatementNode() {}
func (sc *SwitchCase) NodeStr() string {
	var out bytes.Buffer
	out.WriteString("{")
	if sc.TestExpr == nil {
		out.WriteString("default ")
	} else {
		out.WriteString("case ")
	}
	if sc.TestExpr != nil {
		out.WriteString(sc.TestExpr.NodeStr())
	}
	out.WriteString(":")
	if sc.Consequence != nil {
		out.WriteString("{")
		out.WriteString(sc.Consequence.NodeStr())
		out.WriteString("}")
	}
	out.WriteString("}")
	return out.String()
}

type SwitchStatement struct {
	NodeLoc
	TestExpr Expression   // The expression that ie being tested against i.e. switch(<expr>)
	Cases    []SwitchCase // The case statements
}

func (ss *SwitchStatement) StatementNode() {}
func (ss *SwitchStatement) NodeStr() string {
	var out bytes.Buffer
	out.WriteString("switch ")
	out.WriteString("(")
	out.WriteString(ss.TestExpr.NodeStr())
	out.WriteString(")")
	out.WriteString(" {")
	for _, _case := range ss.Cases {
		out.WriteString(_case.NodeStr())
	}
	return out.String()
}

type BreakStatement struct {
	NodeLoc
}

func (br *BreakStatement) StatementNode() {}
func (br *BreakStatement) NodeStr() string {
	return "break;"
}

/** ForLoops **/
type ForLoopStatement struct {
	NodeLoc
	Init   Statement           // An ExpressionStatement that is evaluated only once at the beginig of the loop
	Test   Expression          // An ExpressionStatement that is evaluated before every loop
	Update Expression          // An expressionStatement that is evaluated at the end of every loop
	Body   *BlockStatementNode // The body of the for-loop
}

func (fl *ForLoopStatement) StatementNode() {}
func (fl *ForLoopStatement) NodeStr() string {
	var out bytes.Buffer
	out.WriteString("for (")
	if fl.Init != nil {
		out.WriteString(fl.Init.NodeStr())
	}
	out.WriteString(";")
	if fl.Test != nil {
		out.WriteString(fl.Test.NodeStr())
	}
	out.WriteString(";")
	if fl.Update != nil {
		out.WriteString(fl.Update.NodeStr())
	}
	out.WriteString(") { ")
	for _, stmt := range fl.Body.Statements {
		out.WriteString(stmt.NodeStr())
	}
	out.WriteString(" }")
	return out.String()
}
