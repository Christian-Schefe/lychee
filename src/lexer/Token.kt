package lexer

interface IToken {
    override fun toString(): String
}

enum class CharToken(val char: Char) : IToken {
    SEMICOLON(';'), OPEN_BRACE('{'), CLOSE_BRACE('}'), OPEN_PAREN('('), CLOSE_PAREN(')');

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
    BITWISE_OR("|"), BITWISE_XOR("^");

    override fun toString(): String {
        return str
    }

    companion object {
        val tokenMap = entries.associateBy { it.str }
    }
}

enum class KeywordToken(val keyword: String) : IToken {
    INT("int"), RETURN("return");

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