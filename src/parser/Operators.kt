package parser

import lexer.CharToken
import lexer.IToken
import lexer.OperatorToken

enum class UnaryOperator(val token: IToken) {
    LOGICAL_NOT(OperatorToken.LOGICAL_NOT), BITWISE_NOT(OperatorToken.BITWISE_NOT), NEGATE(OperatorToken.SUBTRACT), POSITIVE(
        OperatorToken.ADD
    );

    companion object {
        val map = entries.associateBy { it.token }

        fun fromToken(token: IToken): UnaryOperator? {
            return map[token]
        }
    }
}
