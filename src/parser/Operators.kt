package parser

import lexer.CharToken
import lexer.IToken

enum class UnaryOperator(val token: IToken) {
    LOGICAL_NOT(CharToken.LOGICAL_NOT), BITWISE_NOT(CharToken.BITWISE_NOT), NEGATE(CharToken.MINUS);

    companion object {
        val map = entries.associateBy { it.token }

        fun fromToken(token: IToken): UnaryOperator? {
            return map[token]
        }
    }
}
