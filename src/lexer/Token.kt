package lexer

data class TokenSource(val token: IToken, val line: Int, val column: Int) {
    override fun toString(): String {
        return "$line:$column ['$token']"
    }
}

interface IToken {
    override fun toString(): String
}

enum class CharToken(val char: Char) : IToken {
    SEMICOLON(';'), OPEN_BRACE('{'), CLOSE_BRACE('}'), OPEN_PAREN('('), CLOSE_PAREN(')'), COLON(':'), QUESTION_MARK(
        '?'
    );

    override fun toString(): String {
        return char.toString()
    }

    companion object {
        val tokenMap = entries.associateBy { it.char }
    }
}

enum class OperatorToken(val str: String) : IToken {
    ASSIGN("="), EQUAL("=="), NOT_EQUAL("!="), LESS_THAN("<"), LESS_THAN_OR_EQUAL("<="), GREATER_THAN(">"), GREATER_THAN_OR_EQUAL(
        ">="
    ),
    LOGICAL_AND("&&"), LOGICAL_OR("||"), ADD("+"), SUBTRACT("-"), MULTIPLY(
        "*"
    ),
    DIVIDE("/"), BITWISE_NOT("~"), LOGICAL_NOT("!"), MODULO("%"), LEFT_SHIFT("<<"), RIGHT_SHIFT(">>"), BITWISE_AND(
        "&"
    ),
    BITWISE_OR("|"), BITWISE_XOR("^"), INCREMENT("++"), DECREMENT("--"), ADD_ASSIGN("+="), SUBTRACT_ASSIGN("-="), MULTIPLY_ASSIGN(
        "*="
    ),
    DIVIDE_ASSIGN("/="), MODULO_ASSIGN("%="), LEFT_SHIFT_ASSIGN("<<="), RIGHT_SHIFT_ASSIGN(">>="), BITWISE_AND_ASSIGN(
        "&="
    ),
    BITWISE_OR_ASSIGN("|="), BITWISE_XOR_ASSIGN("^="), COMMA(",");

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
    INT("int"), RETURN("return"), IF("if"), ELSE("else"), WHILE("while"), FOR("for"), DO("do"), BREAK("break"), CONTINUE(
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

data class IntegerToken(val value: Int) : IToken {
    override fun toString(): String {
        return "INT($value)"
    }
}