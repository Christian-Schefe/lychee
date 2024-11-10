package parser

import lexer.*

interface INode {
    fun accept(visitor: INodeVisitor)
}

interface INodeVisitor : IExprVisitor, IBlockItemVisitor {
    fun visit(node: Program)
    fun visit(node: Function)
}

class Program(val functions: List<Function>) : INode {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun parse(tokens: TokenStack): Result<Program> {
            val functions = mutableListOf<Function>()
            while (tokens.peek() != null) {
                val function = Function.parse(tokens).getOrElse { return fail(it) }
                functions.add(function)
            }
            return succeed(Program(functions))
        }
    }
}

class Function(
    val name: String, val returnType: IDataType, val params: List<Parameter>, val blockItems: List<IBlockItem>
) : INode {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun parse(tokens: TokenStack): Result<Function> {
            val typeId = tokens.consumeFn { it as? IdentifierToken } ?: return fail("Function Return Type", tokens)
            val identifier = tokens.consumeFn { it as? IdentifierToken } ?: return fail("Function Name", tokens)
            tokens.popMatching { it == SymbolToken.OPEN_PAREN } ?: return fail(SymbolToken.OPEN_PAREN, tokens)
            val params = mutableListOf<Parameter>()
            while (tokens.peek() != SymbolToken.CLOSE_PAREN) {
                val param = Parameter.parse(tokens).getOrElse { return fail(it) }
                params.add(param)
                if (tokens.peek() == SymbolToken.COMMA) {
                    tokens.pop()
                } else if (tokens.peek() != SymbolToken.CLOSE_PAREN) {
                    return fail("',' or ')' in parameter list", tokens)
                }
            }
            tokens.popMatching { it == SymbolToken.CLOSE_PAREN } ?: return fail(SymbolToken.CLOSE_PAREN, tokens)
            tokens.popMatching { it == SymbolToken.OPEN_BRACE } ?: return fail(SymbolToken.OPEN_BRACE, tokens)
            val blockItems = mutableListOf<IBlockItem>()
            while (tokens.peek() != SymbolToken.CLOSE_BRACE) {
                val blockItem = tryParseBlockItem(tokens).getOrElse { return fail(it) }
                blockItems.add(blockItem)
                if (tokens.peek() == null) {
                    return fail(SymbolToken.CLOSE_BRACE, tokens)
                }
            }
            tokens.pop()
            val returnType = IDataType.fromString(typeId.name)
            return succeed(Function(identifier.name, returnType, params, blockItems))
        }
    }
}

data class Parameter(val name: String, val dataType: IDataType) {
    companion object {
        fun parse(tokens: TokenStack): Result<Parameter> {
            val typeId = tokens.consumeFn { it as? IdentifierToken } ?: return fail("Type Name", tokens)
            val nameId = tokens.consumeFn { it as? IdentifierToken } ?: return fail("Parameter Name", tokens)
            return succeed(Parameter(nameId.name, IDataType.fromString(typeId.name)))
        }
    }
}
