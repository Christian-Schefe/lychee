package lexer

interface IToken {}

enum class CharToken(val char: Char) : IToken {
    SEMICOLON(';'), OPEN_BRACE('{'), CLOSE_BRACE('}'), OPEN_PAREN('('), CLOSE_PAREN(')'), PLUS('+'), MINUS('-'), ASTERISK(
        '*'
    ),
    SLASH('/'), BITWISE_NOT('~'), LOGICAL_NOT('!'),
}

enum class KeywordToken(val keyword: String) : IToken {
    INT("int"), RETURN("return");

    companion object {
        val keywordMap = entries.associateBy { it.keyword }
    }
}

data class IdentifierToken(val name: String) : IToken {}

data class IntegerToken(val value: Int) : IToken {}