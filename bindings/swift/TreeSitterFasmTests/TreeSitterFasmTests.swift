import XCTest
import SwiftTreeSitter
import TreeSitterFasm

final class TreeSitterFasmTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_fasm())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading FASM grammar")
    }
}
