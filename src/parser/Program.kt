package parser

import lexer.*

class ParsingException(message: String) : Exception(message) {
    constructor(
        token: IToken, tokens: TokenStack
    ) : this(token.toString(), tokens)

    constructor(
        token: String, tokens: TokenStack
    ) : this(tokens.curLocation?.let {
        "At Ln. ${it.line}, Col. ${it.column} - Expected '$token', found '${it.token}'"
    } ?: "Unexpected EOF")
}

interface INode {
    fun accept(visitor: INodeVisitor)
}

interface INodeVisitor : IExprVisitor, IBlockItemVisitor {
    fun visit(node: Program)
    fun visit(node: Function)
}

data class Program(val functions: List<Function>) : INode {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun parse(tokens: TokenStack): Program {
            val functions = mutableListOf<Function>()
            while (tokens.peek() != null) {
                val function = Function.parse(tokens)
                functions.add(function)
            }
            return Program(functions)
        }
    }
}

data class Function(val name: String, val blockItems: List<IBlockItem>) : INode {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun parse(tokens: TokenStack): Function {
            tokens.popMatching { it == KeywordToken.INT }
                ?: throw ParsingException("Expected 'int' at ${tokens.curLocation}")
            val identifier = tokens.consumeFn { it as? IdentifierToken }
                ?: throw ParsingException("Expected 'main' at ${tokens.curLocation}")
            tokens.popMatching { it == CharToken.OPEN_PAREN }
                ?: throw ParsingException("Expected '(' at ${tokens.curLocation}")
            tokens.popMatching { it == CharToken.CLOSE_PAREN }
                ?: throw ParsingException("Expected ')' at ${tokens.curLocation}")
            tokens.popMatching { it == CharToken.OPEN_BRACE }
                ?: throw ParsingException("Expected '{' at ${tokens.curLocation}")
            val blockItems = mutableListOf<IBlockItem>()
            while (tokens.peek() != CharToken.CLOSE_BRACE) {
                val blockItem = tryParseBlockItem(tokens).getOrThrow()
                blockItems.add(blockItem)
            }
            tokens.pop()
            return Function(identifier.name, blockItems)
        }
    }
}
