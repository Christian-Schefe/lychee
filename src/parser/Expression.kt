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
    validTokens: List<SymbolToken>,
    resultType: BinOpType
): Result<IExpr> {
    val left = operandParser(tokens).getOrElse { return fail(it) }
    val rightTerms = mutableListOf<Pair<IExpr, SymbolToken>>()
    while (tokens.remaining > 0) {
        val opToken = tokens.peek() as? SymbolToken ?: break
        if (!validTokens.contains(opToken)) break
        tokens.pop()
        val operand = operandParser(tokens).getOrElse { return fail(it) }
        rightTerms.add(operand to opToken)
    }
    return if (rightTerms.isEmpty()) succeed(left)
    else succeed(BinOpExpr(resultType, left, rightTerms))
}

fun tryParseExpr(tokens: TokenStack): Result<IExpr> {
    return IExpr.tryParseLowestPrecedence(tokens)
}

fun <T> tryParseOneOf(tokens: TokenStack, vararg options: (TokenStack) -> Pair<Result<T>, Boolean>): Result<T> {
    val index = tokens.index
    var lastFailure = fail<T>(ParsingException("Expected one of the provided options"))
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
            val token = tokens.consumeFn { it as? IntegerToken } ?: return fail("Integer", tokens, false)
            return succeedTrue(ConstExpr(token.value))
        }

        private fun tryParseVar(tokens: TokenStack): Pair<Result<IExpr>, Boolean> {
            val token = tokens.consumeFn { it as? IdentifierToken } ?: return fail("Identifier", tokens, false)
            return succeedTrue(VarExpr(token.name))
        }

        private fun tryParseParenthesized(tokens: TokenStack): Pair<Result<IExpr>, Boolean> {
            tokens.popMatching { it == SymbolToken.OPEN_PAREN } ?: return fail(SymbolToken.OPEN_PAREN, tokens, false)
            val expr = tryParseExpr(tokens).getOrElse { return fail(it, true) }
            tokens.popMatching { it == SymbolToken.CLOSE_PAREN } ?: return fail(SymbolToken.CLOSE_PAREN, tokens, false)
            return succeedTrue(expr)
        }

        private fun tryParseUnary(tokens: TokenStack): Pair<Result<IExpr>, Boolean> {
            val op = tokens.consumeFn { UnaryOperator.fromToken(it) } ?: return fail("Unary Operator", tokens, false)
            val operand = tryParseHighestPrecedence(tokens).getOrElse { return fail(it, true) }
            return succeedTrue(UnOpExpr(op, operand))
        }

        private fun tryParseFunctionCall(tokens: TokenStack): Pair<Result<IExpr>, Boolean> {
            val idToken = tokens.consumeFn { it as? IdentifierToken } ?: return fail("Identifier", tokens, false)
            tokens.popMatching { it == SymbolToken.OPEN_PAREN } ?: return fail(SymbolToken.OPEN_PAREN, tokens, false)
            val args = mutableListOf<IExpr>()
            while (tokens.peek() != SymbolToken.CLOSE_PAREN) {
                val arg = tryParseAssignmentOrLower(tokens).getOrElse { return fail(it, true) }
                args.add(arg)
                if (tokens.peek() == SymbolToken.COMMA) {
                    tokens.pop()
                } else if (tokens.peek() != SymbolToken.CLOSE_PAREN) {
                    return fail("',' or ')' in function call arguments", tokens, true)
                }
            }
            tokens.pop()
            return succeedTrue(FunctionCallExpr(idToken.name, args))
        }

        private fun tryParseIncrement(tokens: TokenStack): Pair<Result<IExpr>, Boolean> {
            var isDecrement = tokens.consumeFn { SymbolToken.incrementOpMap[it] }
            val idToken =
                tokens.consumeFn { it as? IdentifierToken } ?: return fail("Identifier", tokens, (isDecrement != null))
            if (isDecrement != null) {
                return succeedTrue(IncrementExpr(idToken.name, false, isDecrement))
            }
            isDecrement = tokens.consumeFn { SymbolToken.incrementOpMap[it] } ?: return fail(
                "++ or --", tokens, false
            )
            return succeedTrue(IncrementExpr(idToken.name, true, isDecrement))
        }

        private fun tryParseMultiplicative(tokens: TokenStack): Result<IExpr> {
            return parseExpr(
                tokens,
                ::tryParseHighestPrecedence,
                listOf(SymbolToken.ASTERISK, SymbolToken.DIVIDE, SymbolToken.MODULO),
                BinOpType.MULTIPLICATIVE
            )
        }

        private fun tryParseAdditive(tokens: TokenStack): Result<IExpr> {
            return parseExpr(
                tokens, ::tryParseMultiplicative, listOf(SymbolToken.PLUS, SymbolToken.MINUS), BinOpType.ADDITIVE
            )
        }

        private fun tryParseShift(tokens: TokenStack): Result<IExpr> {
            return parseExpr(
                tokens, ::tryParseAdditive, listOf(SymbolToken.LEFT_SHIFT, SymbolToken.RIGHT_SHIFT), BinOpType.SHIFT
            )
        }

        private fun tryParseRelational(tokens: TokenStack): Result<IExpr> {
            return parseExpr(
                tokens, ::tryParseShift, listOf(
                    SymbolToken.LESS_THAN,
                    SymbolToken.LESS_THAN_OR_EQUAL,
                    SymbolToken.GREATER_THAN,
                    SymbolToken.GREATER_THAN_OR_EQUAL
                ), BinOpType.RELATIONAL
            )
        }

        private fun tryParseEquality(tokens: TokenStack): Result<IExpr> {
            return parseExpr(
                tokens, ::tryParseRelational, listOf(SymbolToken.EQUAL, SymbolToken.NOT_EQUAL), BinOpType.EQUALITY
            )
        }

        private fun tryParseBitwiseAnd(tokens: TokenStack): Result<IExpr> {
            return parseExpr(tokens, ::tryParseEquality, listOf(SymbolToken.BITWISE_AND), BinOpType.BITWISE_AND)
        }

        private fun tryParseBitwiseXor(tokens: TokenStack): Result<IExpr> {
            return parseExpr(tokens, ::tryParseBitwiseAnd, listOf(SymbolToken.BITWISE_XOR), BinOpType.BITWISE_XOR)
        }

        private fun tryParseBitwiseOr(tokens: TokenStack): Result<IExpr> {
            return parseExpr(tokens, ::tryParseBitwiseXor, listOf(SymbolToken.BITWISE_OR), BinOpType.BITWISE_OR)
        }

        private fun tryParseLogicalAnd(tokens: TokenStack): Result<IExpr> {
            return parseExpr(tokens, ::tryParseBitwiseOr, listOf(SymbolToken.LOGICAL_AND), BinOpType.LOGICAL_AND)
        }

        private fun tryParseLogicalOr(tokens: TokenStack): Result<IExpr> {
            return parseExpr(tokens, ::tryParseLogicalAnd, listOf(SymbolToken.LOGICAL_OR), BinOpType.LOGICAL_OR)
        }

        private fun tryParseTernary(tokens: TokenStack): Result<IExpr> {
            val condition = tryParseLogicalOr(tokens).getOrElse { return fail(it) }
            tokens.popMatching { it == SymbolToken.QUESTION_MARK } ?: return succeed(condition)
            val trueBranch = tryParseExpr(tokens).getOrElse { return fail(it) }
            tokens.popMatching { it == SymbolToken.COLON } ?: return fail(SymbolToken.COLON, tokens)
            val falseBranch = tryParseTernary(tokens).getOrElse { return fail(it) }
            return succeed(ConditionalExpr(condition, trueBranch, falseBranch))
        }

        private fun tryParseAssignmentOrLower(tokens: TokenStack): Result<IExpr> {
            val index = tokens.index
            tryParseAssignment(tokens).onSuccess { return succeed(it) }
            tokens.index = index
            return tryParseTernary(tokens)
        }

        private fun tryParseAssignment(tokens: TokenStack): Result<IExpr> {
            val idToken = tokens.consumeFn { it as? IdentifierToken } ?: return fail("Identifier", tokens)
            val op = tokens.consumeFn { SymbolToken.assignOpMap[it] } ?: return fail("Assignment Operator", tokens)
            val tokenIndex = tokens.index
            tryParseAssignment(tokens).onSuccess { return succeed(AssignExpr(idToken.name, it, op)) }
            tokens.index = tokenIndex
            val lowerExpr = tryParseTernary(tokens).getOrElse { return fail(it) }
            return succeed(AssignExpr(idToken.name, lowerExpr, op))
        }

        private fun tryParseComma(tokens: TokenStack): Result<IExpr> {
            return parseExpr(tokens, ::tryParseAssignmentOrLower, listOf(SymbolToken.COMMA), BinOpType.COMMA)
        }
    }
}

enum class BinOpType {
    ADDITIVE, MULTIPLICATIVE, SHIFT, RELATIONAL, EQUALITY, BITWISE_AND, BITWISE_XOR, BITWISE_OR, LOGICAL_AND, LOGICAL_OR, COMMA;
}

class BinOpExpr(val type: BinOpType, val left: IExpr, val right: List<Pair<IExpr, SymbolToken>>) : IExpr {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)
}

class ConstExpr(val value: Int) : IExpr {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)
}

class UnOpExpr(val operator: UnaryOperator, val right: IExpr) : IExpr {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)
}

class AssignExpr(val name: String, val expr: IExpr, val op: SymbolToken) : IExpr {
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