package tree_sitter_fasm_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_fasm "github.com/tree-sitter/tree-sitter-fasm/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_fasm.Language())
	if language == nil {
		t.Errorf("Error loading FASM grammar")
	}
}
