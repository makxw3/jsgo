// package ast

// /** This file contains the template struct for all the nodes **/

// // Every Node in the ast extends the Node struct
// type Node struct {
// 	_type string // The type of node as a string
// 	start int    // The index of start char of the first token
// 	end   int    // The index of the last char of the last token
// }

// // The Statement Node
// type Statement struct {
// }

// // The Root of the ast is the Program Node
// type Program struct {
// 	Node
// 	Body []Statement // The body is the entire program which is an array of Statements
// }

// // Node -> StatementNode -> ExpressionNode
// // StatementNode -> This is a node that holds the statements, Every program contains a series of statements
// // ExpressionNodes -> Expression nodes are those nodes that are used inside statements so as to create a Statement or
// // an ExpressionStatement which is a Statement that only contains one expresion
package ast
