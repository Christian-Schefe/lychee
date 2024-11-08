package parser

import lexer.OperatorToken
import lexer.TokenStack
import java.util.function.Predicate

interface IExprVisitor {
    fun visit(expr: MultiplicativeExpr)
    fun visit(expr: AdditiveExpr)
    fun visit(expr: RelationalExpr)
    fun visit(expr: EqualityExpr)
    fun visit(expr: LogicalAndExpr)
    fun visit(expr: LogicalAndExpression)
}

fun <TExpr : BaseExpr<TOperand>, TOperand> parseBaseExpr(
    tokens: TokenStack,
    operandParser: (TokenStack) -> Result<TOperand>,
    validToken: Predicate<OperatorToken>,
    constructor: (TOperand, List<Pair<TOperand, OperatorToken>>) -> TExpr
): Result<TExpr> {
    val left = operandParser(tokens).getOrElse { return Result.failure(it) }
    val rightTerms = mutableListOf<Pair<TOperand, OperatorToken>>()
    while (tokens.remaining > 0) {
        val opToken = tokens.peek() as? OperatorToken ?: break
        if (!validToken.test(opToken)) break
        val operand = operandParser(tokens).getOrElse { return Result.failure(it) }
        rightTerms.add(operand to opToken)
    }
    val expr = constructor(left, rightTerms)
    return Result.success(expr)
}

abstract class BaseExpr<TOperand>(val left: TOperand, val right: List<Pair<TOperand, OperatorToken>>) : INode

class MultiplicativeExpr(left: IFactor, right: List<Pair<IFactor, OperatorToken>>) : BaseExpr<IFactor>(left, right) {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Result<MultiplicativeExpr> {
            return parseBaseExpr(tokens,
                ::tryParseFactor,
                { it == OperatorToken.MULTIPLY || it == OperatorToken.DIVIDE },
                { left, right -> MultiplicativeExpr(left, right) })
        }
    }
}

class AdditiveExpr(left: MultiplicativeExpr, right: List<Pair<MultiplicativeExpr, OperatorToken>>) :
    BaseExpr<MultiplicativeExpr>(left, right) {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Result<AdditiveExpr> {
            return parseBaseExpr(tokens,
                MultiplicativeExpr.Companion::tryParse,
                { it == OperatorToken.ADD || it == OperatorToken.SUBTRACT },
                { left, right -> AdditiveExpr(left, right) })
        }
    }
}

class RelationalExpr(left: AdditiveExpr, right: List<Pair<AdditiveExpr, OperatorToken>>) :
    BaseExpr<AdditiveExpr>(left, right) {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Result<RelationalExpr> {
            return parseBaseExpr(tokens,
                AdditiveExpr.Companion::tryParse,
                { it == OperatorToken.LESS_THAN || it == OperatorToken.LESS_THAN_OR_EQUAL || it == OperatorToken.GREATER_THAN || it == OperatorToken.GREATER_THAN_OR_EQUAL },
                { left, right -> RelationalExpr(left, right) })
        }
    }
}

class EqualityExpr(left: RelationalExpr, right: List<Pair<RelationalExpr, OperatorToken>>) :
    BaseExpr<RelationalExpr>(left, right) {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Result<EqualityExpr> {
            return parseBaseExpr(tokens,
                RelationalExpr.Companion::tryParse,
                { it == OperatorToken.EQUAL || it == OperatorToken.NOT_EQUAL },
                { left, right -> EqualityExpr(left, right) })
        }
    }
}

class LogicalAndExpr(left: EqualityExpr, right: List<Pair<EqualityExpr, OperatorToken>>) :
    BaseExpr<EqualityExpr>(left, right) {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Result<LogicalAndExpr> {
            return parseBaseExpr(tokens,
                EqualityExpr.Companion::tryParse,
                { it == OperatorToken.LOGICAL_AND },
                { left, right -> LogicalAndExpr(left, right) })
        }
    }
}

class LogicalAndExpression(left: LogicalAndExpr, right: List<Pair<LogicalAndExpr, OperatorToken>>) :
    BaseExpr<LogicalAndExpr>(left, right) {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Result<LogicalAndExpression> {
            return parseBaseExpr(tokens,
                LogicalAndExpr.Companion::tryParse,
                { it == OperatorToken.LOGICAL_OR },
                { left, right -> LogicalAndExpression(left, right) })
        }
    }
}