package parser

import lexer.OperatorToken
import lexer.TokenStack

interface IExprVisitor {
    fun visit(expr: Expr)
    fun visit(expr: FactorExpr)
}

private fun parseExpr(
    tokens: TokenStack,
    operandParser: (TokenStack) -> Result<IExpr>,
    validTokens: List<OperatorToken>,
    resultType: ExprType
): Result<IExpr> {
    val left = operandParser(tokens).getOrElse { return Result.failure(it) }
    val rightTerms = mutableListOf<Pair<IExpr, OperatorToken>>()
    while (tokens.remaining > 0) {
        val opToken = tokens.peek() as? OperatorToken ?: break
        if (!validTokens.contains(opToken)) break
        tokens.pop()
        val operand = operandParser(tokens).getOrElse { return Result.failure(it) }
        rightTerms.add(operand to opToken)
    }
    return if (rightTerms.isEmpty()) Result.success(left)
    else Result.success(Expr(resultType, left, rightTerms))
}

interface IExpr : INode {
    companion object {
        fun tryParse(tokens: TokenStack): Result<IExpr> {
            return tryParseLogicalOr(tokens)
        }

        private fun tryParseMultiplicative(tokens: TokenStack): Result<IExpr> {
            return parseExpr(
                tokens,
                FactorExpr.Companion::tryParse,
                listOf(OperatorToken.MULTIPLY, OperatorToken.DIVIDE, OperatorToken.MODULO),
                ExprType.MULTIPLICATIVE
            )
        }

        private fun tryParseAdditive(tokens: TokenStack): Result<IExpr> {
            return parseExpr(
                tokens, ::tryParseMultiplicative, listOf(OperatorToken.ADD, OperatorToken.SUBTRACT), ExprType.ADDITIVE
            )
        }

        private fun tryParseShift(tokens: TokenStack): Result<IExpr> {
            return parseExpr(
                tokens, ::tryParseAdditive, listOf(OperatorToken.LEFT_SHIFT, OperatorToken.RIGHT_SHIFT), ExprType.SHIFT
            )
        }

        private fun tryParseRelational(tokens: TokenStack): Result<IExpr> {
            return parseExpr(
                tokens, ::tryParseShift, listOf(
                    OperatorToken.LESS_THAN,
                    OperatorToken.LESS_THAN_OR_EQUAL,
                    OperatorToken.GREATER_THAN,
                    OperatorToken.GREATER_THAN_OR_EQUAL
                ), ExprType.RELATIONAL
            )
        }

        private fun tryParseEquality(tokens: TokenStack): Result<IExpr> {
            return parseExpr(
                tokens, ::tryParseRelational, listOf(OperatorToken.EQUAL, OperatorToken.NOT_EQUAL), ExprType.EQUALITY
            )
        }

        private fun tryParseBitwiseAnd(tokens: TokenStack): Result<IExpr> {
            return parseExpr(tokens, ::tryParseEquality, listOf(OperatorToken.BITWISE_AND), ExprType.BITWISE_AND)
        }

        private fun tryParseBitwiseXor(tokens: TokenStack): Result<IExpr> {
            return parseExpr(tokens, ::tryParseBitwiseAnd, listOf(OperatorToken.BITWISE_XOR), ExprType.BITWISE_XOR)
        }

        private fun tryParseBitwiseOr(tokens: TokenStack): Result<IExpr> {
            return parseExpr(tokens, ::tryParseBitwiseXor, listOf(OperatorToken.BITWISE_OR), ExprType.BITWISE_OR)
        }

        private fun tryParseLogicalAnd(tokens: TokenStack): Result<IExpr> {
            return parseExpr(tokens, ::tryParseBitwiseOr, listOf(OperatorToken.LOGICAL_AND), ExprType.LOGICAL_AND)
        }

        private fun tryParseLogicalOr(tokens: TokenStack): Result<IExpr> {
            return parseExpr(tokens, ::tryParseLogicalAnd, listOf(OperatorToken.LOGICAL_OR), ExprType.LOGICAL_OR)
        }
    }
}

class Expr(val type: ExprType, val left: IExpr, val right: List<Pair<IExpr, OperatorToken>>) : IExpr {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)
}

enum class ExprType {
    ADDITIVE, MULTIPLICATIVE, SHIFT, RELATIONAL, EQUALITY, BITWISE_AND, BITWISE_XOR, BITWISE_OR, LOGICAL_AND, LOGICAL_OR
}

class FactorExpr(val factor: IFactor) : IExpr {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Result<FactorExpr> {
            val factor = tryParseFactor(tokens).getOrElse { return Result.failure(it) }
            return Result.success(FactorExpr(factor))
        }
    }
}