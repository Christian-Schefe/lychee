package parser

import lexer.*

class ParsingException(message: String) : Exception(message)

interface INode {
    fun accept(visitor: INodeVisitor)
}

interface INodeVisitor {
    fun visit(node: Program)
    fun visit(node: Function)
    fun visit(node: Statement)
    fun visit(node: Expression)
    fun visit(node: Term)
    fun visit(node: ConstFactor)
    fun visit(node: UnaryOpFactor)
    fun visit(node: ParenExprFactor)
}

class Program(val function: Function) : INode {
    override fun toString(): String {
        return "Program(function=$function)"
    }

    override fun accept(visitor: INodeVisitor) {
        visitor.visit(this)
    }

    companion object {
        fun parse(tokens: TokenStack): Program {
            val function = Function.parse(tokens)
            return Program(function)
        }
    }
}

class Function(val name: String, val statement: Statement) : INode {
    override fun toString(): String {
        return "Function(statement=$statement)"
    }

    override fun accept(visitor: INodeVisitor) {
        visitor.visit(this)
    }

    companion object {
        fun parse(tokens: TokenStack): Function {
            tokens.popMatching { it == KeywordToken.INT } ?: throw ParsingException("Expected 'int'")
            val identifier = tokens.consumeFn { it as? IdentifierToken } ?: throw ParsingException("Expected 'main'")
            tokens.popMatching { it == CharToken.OPEN_PAREN } ?: throw ParsingException("Expected '('")
            tokens.popMatching { it == CharToken.CLOSE_PAREN } ?: throw ParsingException("Expected ')'")
            tokens.popMatching { it == CharToken.OPEN_BRACE } ?: throw ParsingException("Expected '{'")
            val statement = Statement.parse(tokens)
            tokens.popMatching { it == CharToken.CLOSE_BRACE } ?: throw ParsingException("Expected '}'")
            return Function(identifier.name, statement)
        }
    }
}

class Statement(val expression: Expression) : INode {
    override fun toString(): String {
        return "Statement(expression=$expression)"
    }

    override fun accept(visitor: INodeVisitor) {
        visitor.visit(this)
    }

    companion object {
        fun parse(tokens: TokenStack): Statement {
            tokens.popMatching { it == KeywordToken.RETURN } ?: throw ParsingException("Expected 'return'")
            val expr = Expression.tryParse(tokens).getOrElse { throw it }
            tokens.popMatching { it == CharToken.SEMICOLON } ?: throw ParsingException("Expected ';'")
            return Statement(expr)
        }
    }
}
