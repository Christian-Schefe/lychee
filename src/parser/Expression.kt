package parser

import lexer.*

interface IExprVisitor {
    fun visit(expr: BinOpExpr)
    fun visit(expr: UnOpExpr)
    fun visit(expr: ConstExpr)
    fun visit(expr: AssignExpr)
    fun visit(expr: VarExpr)
    fun visit(expr: IncrementExpr)
    fun visit(expr: ConditionalExpr)
    fun visit(expr: FunctionCallExpr)
}

private fun parseExpr(
    tokens: TokenStack,
    operandParser: (TokenStack) -> Result<IExpr>,
    validTokens: List<OperatorToken>,
    resultType: BinOpType
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
    else Result.success(BinOpExpr(resultType, left, rightTerms))
}

fun tryParseExpr(tokens: TokenStack): Result<IExpr> {
    return IExpr.tryParseLowestPrecedence(tokens)
}

fun <T> tryParseOneOf(tokens: TokenStack, vararg options: (TokenStack) -> Pair<Result<T>, Boolean>): Result<T> {
    val index = tokens.index
    var lastFailure = Result.failure<T>(ParsingException("Expected one of the provided options"))
    for (option in options) {
        val result = option(tokens)
        if (result.first.isSuccess || result.second) return result.first
        tokens.index = index
        lastFailure = result.first
    }
    return lastFailure
}

interface IExpr : INode {
    companion object {
        fun tryParseLowestPrecedence(tokens: TokenStack): Result<IExpr> {
            return tryParseComma(tokens)
        }

        private fun tryParseHighestPrecedence(tokens: TokenStack): Result<IExpr> {
            return tryParseOneOf(
                tokens,
                ::tryParseUnary,
                ::tryParseParenthesized,
                ::tryParseConst,
                ::tryParseFunctionCall,
                ::tryParseIncrement,
                ::tryParseVar,
            )
        }

        private fun tryParseConst(tokens: TokenStack): Pair<Result<IExpr>, Boolean> {
            val token = tokens.consumeFn { it as? IntegerToken } ?: return Result.failure<IExpr>(
                ParsingException(
                    "Constant", tokens
                )
            ) to false
            return Result.success(ConstExpr(token.value)) to true
        }

        private fun tryParseVar(tokens: TokenStack): Pair<Result<IExpr>, Boolean> {
            val token = tokens.consumeFn { it as? IdentifierToken } ?: return Result.failure<IExpr>(
                ParsingException(
                    "Identifier", tokens
                )
            ) to false
            return Result.success(VarExpr(token.name)) to true
        }

        private fun tryParseParenthesized(tokens: TokenStack): Pair<Result<IExpr>, Boolean> {
            tokens.popMatching { it == CharToken.OPEN_PAREN } ?: return Result.failure<IExpr>(
                ParsingException(
                    CharToken.OPEN_PAREN, tokens
                )
            ) to false
            val expr = tryParseExpr(tokens).getOrElse { return Result.failure<IExpr>(it) to true }
            tokens.popMatching { it == CharToken.CLOSE_PAREN } ?: return Result.failure<IExpr>(
                ParsingException(
                    CharToken.CLOSE_PAREN, tokens
                )
            ) to true
            return Result.success(expr) to true
        }

        private fun tryParseUnary(tokens: TokenStack): Pair<Result<IExpr>, Boolean> {
            val op = tokens.consumeFn { UnaryOperator.fromToken(it) } ?: return Result.failure<IExpr>(
                ParsingException(
                    "Unary Operator", tokens
                )
            ) to false
            val operand = tryParseHighestPrecedence(tokens).getOrElse { return Result.failure<IExpr>(it) to true }
            return Result.success(UnOpExpr(op, operand)) to true
        }

        private fun tryParseFunctionCall(tokens: TokenStack): Pair<Result<IExpr>, Boolean> {
            val idToken = tokens.consumeFn { it as? IdentifierToken } ?: return Result.failure<IExpr>(
                ParsingException(
                    "Identifier", tokens
                )
            ) to false
            tokens.popMatching { it == CharToken.OPEN_PAREN } ?: return Result.failure<IExpr>(
                ParsingException(
                    CharToken.OPEN_PAREN, tokens
                )
            ) to false
            val args = mutableListOf<IExpr>()
            while (tokens.peek() != CharToken.CLOSE_PAREN) {
                val arg = tryParseAssignmentOrLower(tokens).getOrElse { return Result.failure<IExpr>(it) to true }
                args.add(arg)
                if (tokens.peek() == OperatorToken.COMMA) {
                    tokens.pop()
                } else if (tokens.peek() != CharToken.CLOSE_PAREN) {
                    return Result.failure<IExpr>(
                        ParsingException(
                            OperatorToken.COMMA, tokens
                        )
                    ) to true
                }
            }
            tokens.pop()
            return Result.success(FunctionCallExpr(idToken.name, args)) to true
        }

        private fun tryParseIncrement(tokens: TokenStack): Pair<Result<IExpr>, Boolean> {
            var isDecrement = tokens.consumeFn { OperatorToken.incrementOpMap[it] }
            val idToken = tokens.consumeFn { it as? IdentifierToken } ?: return Result.failure<IExpr>(
                ParsingException(
                    "Identifier", tokens
                )
            ) to (isDecrement != null)
            if (isDecrement != null) {
                return Result.success(IncrementExpr(idToken.name, false, isDecrement)) to true
            }
            isDecrement = tokens.consumeFn { OperatorToken.incrementOpMap[it] } ?: return Result.failure<IExpr>(
                ParsingException(
                    "++ or --", tokens
                )
            ) to false
            return Result.success(IncrementExpr(idToken.name, true, isDecrement)) to true
        }

        private fun tryParseMultiplicative(tokens: TokenStack): Result<IExpr> {
            return parseExpr(
                tokens,
                ::tryParseHighestPrecedence,
                listOf(OperatorToken.MULTIPLY, OperatorToken.DIVIDE, OperatorToken.MODULO),
                BinOpType.MULTIPLICATIVE
            )
        }

        private fun tryParseAdditive(tokens: TokenStack): Result<IExpr> {
            return parseExpr(
                tokens, ::tryParseMultiplicative, listOf(OperatorToken.ADD, OperatorToken.SUBTRACT), BinOpType.ADDITIVE
            )
        }

        private fun tryParseShift(tokens: TokenStack): Result<IExpr> {
            return parseExpr(
                tokens, ::tryParseAdditive, listOf(OperatorToken.LEFT_SHIFT, OperatorToken.RIGHT_SHIFT), BinOpType.SHIFT
            )
        }

        private fun tryParseRelational(tokens: TokenStack): Result<IExpr> {
            return parseExpr(
                tokens, ::tryParseShift, listOf(
                    OperatorToken.LESS_THAN,
                    OperatorToken.LESS_THAN_OR_EQUAL,
                    OperatorToken.GREATER_THAN,
                    OperatorToken.GREATER_THAN_OR_EQUAL
                ), BinOpType.RELATIONAL
            )
        }

        private fun tryParseEquality(tokens: TokenStack): Result<IExpr> {
            return parseExpr(
                tokens, ::tryParseRelational, listOf(OperatorToken.EQUAL, OperatorToken.NOT_EQUAL), BinOpType.EQUALITY
            )
        }

        private fun tryParseBitwiseAnd(tokens: TokenStack): Result<IExpr> {
            return parseExpr(tokens, ::tryParseEquality, listOf(OperatorToken.BITWISE_AND), BinOpType.BITWISE_AND)
        }

        private fun tryParseBitwiseXor(tokens: TokenStack): Result<IExpr> {
            return parseExpr(tokens, ::tryParseBitwiseAnd, listOf(OperatorToken.BITWISE_XOR), BinOpType.BITWISE_XOR)
        }

        private fun tryParseBitwiseOr(tokens: TokenStack): Result<IExpr> {
            return parseExpr(tokens, ::tryParseBitwiseXor, listOf(OperatorToken.BITWISE_OR), BinOpType.BITWISE_OR)
        }

        private fun tryParseLogicalAnd(tokens: TokenStack): Result<IExpr> {
            return parseExpr(tokens, ::tryParseBitwiseOr, listOf(OperatorToken.LOGICAL_AND), BinOpType.LOGICAL_AND)
        }

        private fun tryParseLogicalOr(tokens: TokenStack): Result<IExpr> {
            return parseExpr(tokens, ::tryParseLogicalAnd, listOf(OperatorToken.LOGICAL_OR), BinOpType.LOGICAL_OR)
        }

        private fun tryParseTernary(tokens: TokenStack): Result<IExpr> {
            val condition = tryParseLogicalOr(tokens).getOrElse { return Result.failure(it) }
            tokens.popMatching { it == CharToken.QUESTION_MARK } ?: return Result.success(condition)
            val trueBranch = tryParseExpr(tokens).getOrElse { return Result.failure(it) }
            tokens.popMatching { it == CharToken.COLON } ?: return Result.failure(
                ParsingException(
                    CharToken.SEMICOLON, tokens
                )
            )
            val falseBranch = tryParseTernary(tokens).getOrElse { return Result.failure(it) }
            return Result.success(ConditionalExpr(condition, trueBranch, falseBranch))
        }

        private fun tryParseAssignmentOrLower(tokens: TokenStack): Result<IExpr> {
            val index = tokens.index
            tryParseAssignment(tokens).onSuccess { return Result.success(it) }
            tokens.index = index
            return tryParseTernary(tokens)
        }

        private fun tryParseAssignment(tokens: TokenStack): Result<IExpr> {
            val idToken = tokens.consumeFn { it as? IdentifierToken } ?: return Result.failure(
                ParsingException(
                    "Identifier", tokens
                )
            )
            val op = tokens.consumeFn { OperatorToken.assignOpMap[it] } ?: return Result.failure(
                ParsingException(
                    "Assignment Operator", tokens
                )
            )
            val tokenIndex = tokens.index
            tryParseAssignment(tokens).onSuccess { return Result.success(AssignExpr(idToken.name, it, op)) }
            tokens.index = tokenIndex
            val lowerExpr = tryParseTernary(tokens).getOrElse { return Result.failure(it) }
            return Result.success(AssignExpr(idToken.name, lowerExpr, op))
        }

        private fun tryParseComma(tokens: TokenStack): Result<IExpr> {
            return parseExpr(tokens, ::tryParseAssignmentOrLower, listOf(OperatorToken.COMMA), BinOpType.COMMA)
        }
    }
}

enum class BinOpType {
    ADDITIVE, MULTIPLICATIVE, SHIFT, RELATIONAL, EQUALITY, BITWISE_AND, BITWISE_XOR, BITWISE_OR, LOGICAL_AND, LOGICAL_OR, COMMA;
}

class BinOpExpr(val type: BinOpType, val left: IExpr, val right: List<Pair<IExpr, OperatorToken>>) : IExpr {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)
}

class ConstExpr(val value: Int) : IExpr {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)
}

class UnOpExpr(val operator: UnaryOperator, val right: IExpr) : IExpr {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)
}

class AssignExpr(val name: String, val expr: IExpr, val op: OperatorToken) : IExpr {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)
}

class VarExpr(val name: String) : IExpr {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)
}

class IncrementExpr(val name: String, val isPostfix: Boolean, val isDecrement: Boolean) : IExpr {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)
}

class ConditionalExpr(val condition: IExpr, val trueBranch: IExpr, val falseBranch: IExpr) : IExpr {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)
}

class FunctionCallExpr(val name: String, val args: List<IExpr>) : IExpr {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)
}