package parser

import lexer.CharToken
import lexer.IntegerToken
import lexer.TokenStack

interface IFactorVisitor {
    fun visit(factor: ConstFactor)
    fun visit(factor: UnaryOpFactor)
    fun visit(factor: ParenExprFactor)
}

interface IFactor : INode

fun tryParseFactor(tokens: TokenStack): Result<IFactor> {
    val tokenIndex = tokens.index
    ConstFactor.tryParse(tokens).onSuccess { return Result.success(it) }
    tokens.index = tokenIndex
    UnaryOpFactor.tryParse(tokens).onSuccess { return Result.success(it) }
    tokens.index = tokenIndex
    ParenExprFactor.tryParse(tokens).onSuccess { return Result.success(it) }
    return Result.failure(ParsingException("Expected factor at index $tokenIndex: "))
}

data class ConstFactor(val value: Int) : IFactor {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Result<ConstFactor> {
            val intToken =
                tokens.consumeFn { it as? IntegerToken } ?: return Result.failure(ParsingException("Expected integer"))
            return Result.success(ConstFactor(intToken.value))
        }
    }
}

data class UnaryOpFactor(val operator: UnaryOperator, val factor: IFactor) : IFactor {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Result<UnaryOpFactor> {
            val token = tokens.pop() ?: return Result.failure(ParsingException("Expected token"))
            val operator =
                UnaryOperator.fromToken(token) ?: return Result.failure(ParsingException("Expected unary operator"))
            val expression = tryParseFactor(tokens).getOrElse { return Result.failure(it) }
            return Result.success(UnaryOpFactor(operator, expression))
        }
    }
}

data class ParenExprFactor(val expression: IExpr) : IFactor {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Result<ParenExprFactor> {
            tokens.popMatching { it == CharToken.OPEN_PAREN } ?: return Result.failure(ParsingException("Expected '('"))
            val expression = IExpr.tryParse(tokens).getOrElse { return Result.failure(it) }
            tokens.popMatching { it == CharToken.CLOSE_PAREN }
                ?: return Result.failure(ParsingException("Expected ')'"))
            return Result.success(ParenExprFactor(expression))
        }
    }
}