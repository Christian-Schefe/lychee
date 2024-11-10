package lexer

data class TokenSource(val token: IToken, val line: Int, val column: Int)

interface IToken {
    override fun toString(): String
}

enum class SymbolToken(val str: String, val shouldContinue: Boolean) : IToken {
    SEMICOLON(";", false), OPEN_BRACE("{", false), CLOSE_BRACE("}", false), OPEN_PAREN(
        "(", false
    ),
    CLOSE_PAREN(
        ")", false
    ),
    COLON(":", false), QUESTION_MARK(
        "?", false
    ),
    ASSIGN(
        "=", true
    ),
    EQUAL("==", false), NOT_EQUAL("!=", false), LESS_THAN("<", true), LESS_THAN_OR_EQUAL(
        "<=", false
    ),
    GREATER_THAN(">", true), GREATER_THAN_OR_EQUAL(
        ">=", false
    ),
    LOGICAL_AND("&&", false), LOGICAL_OR("||", false), PLUS("+", true), MINUS("-", true), ASTERISK(
        "*", true
    ),
    DIVIDE(
        "/", true
    ),
    BITWISE_NOT("~", false), LOGICAL_NOT("!", false), MODULO("%", true), LEFT_SHIFT("<<", true), RIGHT_SHIFT(
        ">>", true
    ),
    BITWISE_AND(
        "&", true
    ),
    BITWISE_OR("|", true), BITWISE_XOR("^", true), INCREMENT("++", false), DECREMENT("--", false), ADD_ASSIGN(
        "+=", false
    ),
    SUBTRACT_ASSIGN("-=", false), MULTIPLY_ASSIGN(
        "*=", false
    ),
    DIVIDE_ASSIGN("/=", false), MODULO_ASSIGN("%=", false), LEFT_SHIFT_ASSIGN("<<=", false), RIGHT_SHIFT_ASSIGN(
        ">>=", false
    ),
    BITWISE_AND_ASSIGN(
        "&=", false
    ),
    BITWISE_OR_ASSIGN("|=", false), BITWISE_XOR_ASSIGN("^=", false), COMMA(",", false), AMPERSAND(
        "&", true
    ),
    ARROW("->", false);

    override fun toString(): String {
        return str
    }

    companion object {
        val tokenMap = entries.associateBy { it.str }
        val assignOpMap = listOf(
            ASSIGN,
            ADD_ASSIGN,
            SUBTRACT_ASSIGN,
            MULTIPLY_ASSIGN,
            DIVIDE_ASSIGN,
            MODULO_ASSIGN,
            LEFT_SHIFT_ASSIGN,
            RIGHT_SHIFT_ASSIGN,
            BITWISE_AND_ASSIGN,
            BITWISE_OR_ASSIGN,
            BITWISE_XOR_ASSIGN
        ).associateBy { it }
        val incrementOpMap = mapOf(
            INCREMENT to false, DECREMENT to true
        )
    }
}

enum class KeywordToken(val keyword: String) : IToken {
    RETURN("return"), IF("if"), ELSE("else"), WHILE("while"), FOR("for"), DO("do"), BREAK("break"), CONTINUE(
        "continue"
    );

    override fun toString(): String {
        return keyword
    }

    companion object {
        val tokenMap = entries.associateBy { it.keyword }
    }
}

data class IdentifierToken(val name: String) : IToken {
    override fun toString(): String {
        return "ID($name)"
    }
}

data class StringToken(val value: String) : IToken {
    override fun toString(): String {
        return "STRING(\"$value\")"
    }
}

data class IntegerToken(val value: Int) : IToken {
    override fun toString(): String {
        return "INT($value)"
    }
}