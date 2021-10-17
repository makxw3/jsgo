package lexer

import "testing"

func TestLineColumnCount(t *testing.T) {
	inp := `hello
	world`
	lx := Get(inp)

	if lx.lineCount != 0 {
		t.Errorf("Error! Wrong lineCount. Expected %d but got %d", 0, lx.lineCount)
	}
	if lx.columnCount != 0 {
		t.Errorf("Error! Wrong columnCount. Expected %d but got %d", 0, lx.columnCount)
	}
	// Read all the charcaters until lx.char = 'w' in world
	for a := 0; a < 6; a++ {
		lx.nextChar()
	}
	if lx.lineCount != 1 {
		t.Errorf("Error! Wrong lineCount. Expected %d but got %d", 1, lx.lineCount)
	}
	if lx.columnCount != 0 {
		t.Errorf("Error! Wrong columnCount. Expected %d but got %d", 0, lx.columnCount)
	}
}
