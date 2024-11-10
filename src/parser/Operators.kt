package parser

import lexer.IToken
import lexer.SymbolToken

enum class UnaryOperator(val token: IToken) {
    LOGICAL_NOT(SymbolToken.LOGICAL_NOT), BITWISE_NOT(SymbolToken.BITWISE_NOT), NEGATE(SymbolToken.MINUS), POSITIVE(
        SymbolToken.PLUS
    );

    companion object {
        val map = entries.associateBy { it.token }

        fun fromToken(token: IToken): UnaryOperator? {
            return map[token]
        }
    }
}
