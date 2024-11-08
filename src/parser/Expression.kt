package parser

import lexer.CharToken
import lexer.IntegerToken
import lexer.TokenStack


class Expression(val leftTerm: Term, val rightTerms: List<Pair<Boolean, Term>>) : INode {
    override fun toString(): String {
        return "Expression(leftTerm=$leftTerm, rightTerms=$rightTerms)"
    }

    override fun accept(visitor: INodeVisitor) {
        visitor.visit(this)
    }

    companion object {
        fun tryParse(tokens: TokenStack): Result<Expression> {
            val leftTerm = Term.tryParse(tokens).getOrElse { return Result.failure(it) }
            val rightTerms = mutableListOf<Pair<Boolean, Term>>()
            while (tokens.remaining > 0) {
                val token = tokens.peek()
                val isAddition = when (token) {
                    CharToken.PLUS -> true
                    CharToken.MINUS -> false
                    else -> break
                }
                tokens.pop()
                val term = Term.tryParse(tokens).getOrElse { return Result.failure(it) }
                rightTerms.add(isAddition to term)
            }
            return Result.success(Expression(leftTerm, rightTerms))
        }
    }
}

class Term(val leftFactor: IFactor, val rightFactors: List<Pair<Boolean, IFactor>>) : INode {
    override fun toString(): String {
        return "Term(leftFactor=$leftFactor, rightFactors=$rightFactors)"
    }

    override fun accept(visitor: INodeVisitor) {
        visitor.visit(this)
    }

    companion object {
        fun tryParse(tokens: TokenStack): Result<Term> {
            val leftFactor = IFactor.tryParse(tokens).getOrElse { return Result.failure(it) }
            val rightTerms = mutableListOf<Pair<Boolean, IFactor>>()
            while (tokens.remaining > 0) {
                val token = tokens.peek()
                val isMultiplication = when (token) {
                    CharToken.ASTERISK -> true
                    CharToken.SLASH -> false
                    else -> break
                }
                tokens.pop()
                val factor = IFactor.tryParse(tokens).getOrElse { return Result.failure(it) }
                rightTerms.add(isMultiplication to factor)
            }
            return Result.success(Term(leftFactor, rightTerms))
        }
    }
}

interface IFactor : INode {
    companion object {
        fun tryParse(tokens: TokenStack): Result<IFactor> {
            val tokenIndex = tokens.index
            ConstFactor.tryParse(tokens).onSuccess { return Result.success(it) }
            tokens.index = tokenIndex
            UnaryOpFactor.tryParse(tokens).onSuccess { return Result.success(it) }
            tokens.index = tokenIndex
            ParenExprFactor.tryParse(tokens).onSuccess { return Result.success(it) }
            return Result.failure(ParsingException("Expected factor"))
        }
    }
}

class ConstFactor(val value: Int) : IFactor {
    override fun toString(): String {
        return "IntExpression(value=$value)"
    }

    override fun accept(visitor: INodeVisitor) {
        visitor.visit(this)
    }

    companion object {
        fun tryParse(tokens: TokenStack): Result<ConstFactor> {
            val intToken =
                tokens.consumeFn { it as? IntegerToken } ?: return Result.failure(ParsingException("Expected integer"))
            return Result.success(ConstFactor(intToken.value))
        }
    }
}

class UnaryOpFactor(val operator: UnaryOperator, val factor: IFactor) : IFactor {
    override fun toString(): String {
        return "UnaryExpression(operator=$operator, factor=$factor)"
    }

    override fun accept(visitor: INodeVisitor) {
        visitor.visit(this)
    }

    companion object {
        fun tryParse(tokens: TokenStack): Result<UnaryOpFactor> {
            val token = tokens.pop() ?: return Result.failure(ParsingException("Expected token"))
            val operator =
                UnaryOperator.fromToken(token) ?: return Result.failure(ParsingException("Expected unary operator"))
            val expression = IFactor.tryParse(tokens).getOrElse { return Result.failure(it) }
            return Result.success(UnaryOpFactor(operator, expression))
        }
    }
}

class ParenExprFactor(val expression: Expression) : IFactor {
    override fun toString(): String {
        return "ParenthesizedExpression(expression=$expression)"
    }

    override fun accept(visitor: INodeVisitor) {
        visitor.visit(this)
    }

    companion object {
        fun tryParse(tokens: TokenStack): Result<ParenExprFactor> {
            tokens.popMatching { it == CharToken.OPEN_PAREN } ?: return Result.failure(ParsingException("Expected '('"))
            val expression = Expression.tryParse(tokens).getOrElse { return Result.failure(it) }
            tokens.popMatching { it == CharToken.CLOSE_PAREN }
                ?: return Result.failure(ParsingException("Expected ')'"))
            return Result.success(ParenExprFactor(expression))
        }
    }
}