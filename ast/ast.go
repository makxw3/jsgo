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
