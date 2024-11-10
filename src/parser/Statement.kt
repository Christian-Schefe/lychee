package parser

import lexer.*
import java.util.Optional

interface IBlockItem : INode

interface IStatement : IBlockItem

interface IBlockItemVisitor {
    fun visit(statement: ReturnStatement)
    fun visit(statement: ExprStatement)
    fun visit(statement: DeclareBlockItem)
    fun visit(statement: IfStatement)
    fun visit(statement: CompoundStatement)
    fun visit(statement: ForLoopStatement)
    fun visit(statement: WhileLoopStatement)
    fun visit(statement: DoWhileLoopStatement)
    fun visit(statement: LoopControlStatement)
    fun visit(statement: EmptyStatement)
}

fun tryParseBlockItem(tokens: TokenStack): Result<IBlockItem> {
    val tokenIndex = tokens.index
    DeclareBlockItem.tryParse(tokens).onSuccess { return succeed(it) }
    tokens.index = tokenIndex
    return tryParseStatement(tokens).onSuccess { return succeed(it) }
}

fun tryParseStatement(tokens: TokenStack): Result<IStatement> {
    return tryParseOneOf(
        tokens,
        EmptyStatement.Companion::tryParse,
        ReturnStatement.Companion::tryParse,
        IfStatement.Companion::tryParse,
        CompoundStatement.Companion::tryParse,
        ForLoopStatement.Companion::tryParse,
        WhileLoopStatement.Companion::tryParse,
        DoWhileLoopStatement.Companion::tryParse,
        LoopControlStatement.Companion::tryParse,
        ExprStatement.Companion::tryParse
    )
}

class EmptyStatement : IStatement {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Pair<Result<IStatement>, Boolean> {
            tokens.popMatching { it == SymbolToken.SEMICOLON } ?: return fail(SymbolToken.SEMICOLON, tokens, false)
            return succeedTrue(EmptyStatement())
        }
    }
}

class ReturnStatement(val expression: IExpr) : IStatement {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Pair<Result<IStatement>, Boolean> {
            tokens.popMatching { it == KeywordToken.RETURN } ?: return fail(KeywordToken.RETURN, tokens, false)
            val expr = tryParseExpr(tokens).getOrElse { return fail(it, true) }
            tokens.popMatching { it == SymbolToken.SEMICOLON } ?: return fail(SymbolToken.SEMICOLON, tokens, true)
            return succeedTrue(ReturnStatement(expr))
        }
    }
}

class ExprStatement(val expression: IExpr) : IStatement {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Pair<Result<IStatement>, Boolean> {
            val expr = tryParseExpr(tokens).getOrElse { return fail(it, false) }
            tokens.popMatching { it == SymbolToken.SEMICOLON } ?: return fail(SymbolToken.SEMICOLON, tokens, true)
            return succeedTrue(ExprStatement(expr))
        }
    }
}

class DeclareBlockItem(val name: String, val type: IDataType, val initializer: Optional<IExpr>) : IBlockItem {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Result<DeclareBlockItem> {
            val typeToken = tokens.consumeFn { it as? IdentifierToken } ?: return fail("Type Name", tokens)
            val type = IDataType.fromString(typeToken.name)
            val idToken = tokens.consumeFn { it as? IdentifierToken } ?: return fail("Variable Name", tokens)
            val semicolon = tokens.popMatching { it == SymbolToken.SEMICOLON }
            if (semicolon != null) {
                return succeed(DeclareBlockItem(idToken.name, type, Optional.empty()))
            }
            tokens.popMatching { it == SymbolToken.ASSIGN } ?: return fail(SymbolToken.ASSIGN, tokens)
            val expr = tryParseExpr(tokens).getOrElse { return fail(it) }
            tokens.popMatching { it == SymbolToken.SEMICOLON } ?: return fail(SymbolToken.SEMICOLON, tokens)
            return succeed(DeclareBlockItem(idToken.name, type, Optional.of(expr)))
        }
    }
}

class IfStatement(val expr: IExpr, val ifBranch: IStatement, val elseBranch: Optional<IStatement>) : IStatement {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Pair<Result<IStatement>, Boolean> {
            tokens.popMatching { it == KeywordToken.IF } ?: return fail(KeywordToken.IF, tokens, false)
            tokens.popMatching { it == SymbolToken.OPEN_PAREN } ?: return fail(SymbolToken.OPEN_PAREN, tokens, true)
            val expr = tryParseExpr(tokens).getOrElse { return fail(it, true) }
            tokens.popMatching { it == SymbolToken.CLOSE_PAREN } ?: return fail(SymbolToken.CLOSE_PAREN, tokens, true)
            val ifBranch = tryParseStatement(tokens).getOrElse { return fail(it, true) }
            if (tokens.peek() == KeywordToken.ELSE) {
                tokens.pop()
            } else {
                return succeed(IfStatement(expr, ifBranch, Optional.empty())) to true
            }
            val elseBranch = tryParseStatement(tokens).getOrElse { return fail(it, true) }
            return succeed(IfStatement(expr, ifBranch, Optional.of(elseBranch))) to true
        }
    }
}

class CompoundStatement(val blockItems: List<IBlockItem>) : IStatement {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Pair<Result<IStatement>, Boolean> {
            tokens.popMatching { it == SymbolToken.OPEN_BRACE } ?: return fail(SymbolToken.OPEN_BRACE, tokens, false)
            val statements = mutableListOf<IBlockItem>()
            while (tokens.peek() != SymbolToken.CLOSE_BRACE) {
                val statement = tryParseBlockItem(tokens).getOrElse { return fail(it, true) }
                statements.add(statement)
            }
            tokens.pop()
            return succeed(CompoundStatement(statements)) to true
        }
    }
}

class ForLoopStatement(
    val init: INode, val condition: IExpr, val update: Optional<IExpr>, val body: IStatement
) : IStatement {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Pair<Result<IStatement>, Boolean> {
            tokens.popMatching { it == KeywordToken.FOR } ?: return fail(KeywordToken.FOR, tokens, false)
            tokens.popMatching { it == SymbolToken.OPEN_PAREN } ?: return fail(SymbolToken.OPEN_PAREN, tokens, true)
            val index = tokens.index
            val declare = DeclareBlockItem.tryParse(tokens).getOrNull()
            val init = declare ?: run {
                tokens.index = index
                tokens.popMatching { it == SymbolToken.SEMICOLON }?.let { return@run EmptyStatement() }
                val expr = tryParseExpr(tokens).getOrNull() ?: return fail("Declare or Expr", tokens, true)
                tokens.popMatching { it == SymbolToken.SEMICOLON } ?: return fail(SymbolToken.SEMICOLON, tokens, true)
                expr
            }
            val condition = tokens.popMatching { it == SymbolToken.SEMICOLON }?.run { ConstExpr(1) } ?: run {
                val expr = tryParseExpr(
                    tokens
                ).getOrElse { return@tryParse fail(it, true) }
                tokens.popMatching { it == SymbolToken.SEMICOLON } ?: return@tryParse fail(
                    SymbolToken.SEMICOLON, tokens, true
                )
                expr
            }
            val update = tokens.popMatching { it == SymbolToken.CLOSE_PAREN }?.run { Optional.empty<IExpr>() } ?: run {
                val expr = tryParseExpr(tokens).getOrElse { return@tryParse fail(it, true) }
                tokens.popMatching { it == SymbolToken.CLOSE_PAREN } ?: return@tryParse fail(
                    SymbolToken.CLOSE_PAREN, tokens, true
                )
                Optional.of(expr)
            }
            val body = tryParseStatement(tokens).getOrElse { return fail(it, true) }
            return succeed(ForLoopStatement(init, condition, update, body)) to true
        }
    }
}

class WhileLoopStatement(val condition: IExpr, val body: IStatement) : IStatement {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Pair<Result<IStatement>, Boolean> {
            tokens.popMatching { it == KeywordToken.WHILE } ?: return fail(KeywordToken.WHILE, tokens, false)
            tokens.popMatching { it == SymbolToken.OPEN_PAREN } ?: return fail(SymbolToken.OPEN_PAREN, tokens, true)
            val condition = tryParseExpr(tokens).getOrElse { return fail(it, true) }
            tokens.popMatching { it == SymbolToken.CLOSE_PAREN } ?: return fail(SymbolToken.CLOSE_PAREN, tokens, true)
            val body = tryParseStatement(tokens).getOrElse { return fail(it, true) }
            return succeed(WhileLoopStatement(condition, body)) to true
        }
    }
}

class DoWhileLoopStatement(val body: IStatement, val condition: IExpr) : IStatement {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Pair<Result<IStatement>, Boolean> {
            tokens.popMatching { it == KeywordToken.DO } ?: return fail(KeywordToken.DO, tokens, false)
            val body = tryParseStatement(tokens).getOrElse { return fail(it, true) }
            tokens.popMatching { it == KeywordToken.WHILE } ?: return fail(KeywordToken.WHILE, tokens, true)
            tokens.popMatching { it == SymbolToken.OPEN_PAREN } ?: return fail(SymbolToken.OPEN_PAREN, tokens, true)
            val condition = tryParseExpr(tokens).getOrElse { return fail(it, true) }
            tokens.popMatching { it == SymbolToken.CLOSE_PAREN } ?: return fail(SymbolToken.CLOSE_PAREN, tokens, true)
            tokens.popMatching { it == SymbolToken.SEMICOLON } ?: return fail(SymbolToken.SEMICOLON, tokens, true)
            return succeed(DoWhileLoopStatement(body, condition)) to true
        }
    }
}

class LoopControlStatement(val type: LoopControlType) : IStatement {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Pair<Result<IStatement>, Boolean> {
            val type = when (tokens.pop()) {
                KeywordToken.BREAK -> LoopControlType.BREAK
                KeywordToken.CONTINUE -> LoopControlType.CONTINUE
                else -> return Result.failure<IStatement>(
                    ParsingException(
                        "Break or Continue", tokens
                    )
                ) to false
            }
            tokens.popMatching { it == SymbolToken.SEMICOLON } ?: return fail(SymbolToken.SEMICOLON, tokens, true)
            return succeed(LoopControlStatement(type)) to true
        }
    }
}

enum class LoopControlType {
    BREAK, CONTINUE
}

interface IDataType {
    companion object {
        fun fromString(type: String): IDataType {
            BuiltinType.idMap[type]?.let { return it }
            return CustomType(type)
        }
    }
}

enum class BuiltinType(val id: String, val size: Int) : IDataType {
    INT("int", 4), LONG("long", 8);

    companion object {
        val idMap = entries.associateBy { it.id }
    }
}

class PointerType(val type: IDataType) : IDataType
class CustomType(val name: String) : IDataType