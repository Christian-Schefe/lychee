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
    DeclareBlockItem.tryParse(tokens).onSuccess { return Result.success(it) }
    tokens.index = tokenIndex
    return tryParseStatement(tokens).onSuccess { return Result.success(it) }
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
            tokens.popMatching { it == CharToken.SEMICOLON } ?: return Result.failure<IStatement>(
                ParsingException(
                    CharToken.SEMICOLON, tokens
                )
            ) to false
            return Result.success(EmptyStatement()) to true
        }
    }
}

class ReturnStatement(val expression: IExpr) : IStatement {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Pair<Result<IStatement>, Boolean> {
            tokens.popMatching { it == KeywordToken.RETURN } ?: return Result.failure<IStatement>(
                ParsingException(
                    KeywordToken.RETURN, tokens
                )
            ) to false
            val expr = tryParseExpr(tokens).getOrElse { return Result.failure<IStatement>(it) to true }
            tokens.popMatching { it == CharToken.SEMICOLON } ?: return Result.failure<IStatement>(
                ParsingException(
                    CharToken.SEMICOLON, tokens
                )
            ) to true
            return Result.success(ReturnStatement(expr)) to true
        }
    }
}

class ExprStatement(val expression: IExpr) : IStatement {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Pair<Result<IStatement>, Boolean> {
            val expr = tryParseExpr(tokens).getOrElse { return Result.failure<IStatement>(it) to false }
            tokens.popMatching { it == CharToken.SEMICOLON } ?: return Result.failure<IStatement>(
                ParsingException(
                    CharToken.SEMICOLON, tokens
                )
            ) to true
            return Result.success(ExprStatement(expr)) to true
        }
    }
}

class DeclareBlockItem(val name: String, val initializer: Optional<IExpr>) : IBlockItem {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Result<DeclareBlockItem> {
            tokens.popMatching { it == KeywordToken.INT } ?: return Result.failure(
                ParsingException(
                    KeywordToken.INT, tokens
                )
            )
            val idToken = tokens.consumeFn { it as? IdentifierToken } ?: return Result.failure(
                ParsingException(
                    "Identifier", tokens
                )
            )
            val semicolon = tokens.popMatching { it == CharToken.SEMICOLON }
            if (semicolon != null) {
                return Result.success(DeclareBlockItem(idToken.name, Optional.empty()))
            }
            tokens.popMatching { it == OperatorToken.ASSIGN } ?: return Result.failure(
                ParsingException(
                    OperatorToken.ASSIGN, tokens
                )
            )
            val expr = tryParseExpr(tokens).getOrElse { return Result.failure(it) }
            tokens.popMatching { it == CharToken.SEMICOLON } ?: return Result.failure(
                ParsingException(
                    CharToken.SEMICOLON, tokens
                )
            )
            return Result.success(DeclareBlockItem(idToken.name, Optional.of(expr)))
        }
    }
}

class IfStatement(val expr: IExpr, val ifBranch: IStatement, val elseBranch: Optional<IStatement>) : IStatement {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Pair<Result<IStatement>, Boolean> {
            tokens.popMatching { it == KeywordToken.IF } ?: return Result.failure<IStatement>(
                ParsingException(
                    KeywordToken.IF, tokens
                )
            ) to false
            tokens.popMatching { it == CharToken.OPEN_PAREN } ?: return Result.failure<IStatement>(
                ParsingException(
                    CharToken.OPEN_PAREN, tokens
                )
            ) to true
            val expr = tryParseExpr(tokens).getOrElse { return Result.failure<IStatement>(it) to true }
            tokens.popMatching { it == CharToken.CLOSE_PAREN } ?: return Result.failure<IStatement>(
                ParsingException(
                    CharToken.CLOSE_PAREN, tokens
                )
            ) to true
            val ifBranch = tryParseStatement(tokens).getOrElse { return Result.failure<IStatement>(it) to true }
            if (tokens.peek() == KeywordToken.ELSE) {
                tokens.pop()
            } else {
                return Result.success(IfStatement(expr, ifBranch, Optional.empty())) to true
            }
            val elseBranch = tryParseStatement(tokens).getOrElse { return Result.failure<IStatement>(it) to true }
            return Result.success(IfStatement(expr, ifBranch, Optional.of(elseBranch))) to true
        }
    }
}

class CompoundStatement(val blockItems: List<IBlockItem>) : IStatement {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Pair<Result<IStatement>, Boolean> {
            tokens.popMatching { it == CharToken.OPEN_BRACE } ?: return Result.failure<IStatement>(
                ParsingException(
                    CharToken.OPEN_BRACE, tokens
                )
            ) to false
            val statements = mutableListOf<IBlockItem>()
            while (tokens.peek() != CharToken.CLOSE_BRACE) {
                val statement = tryParseBlockItem(tokens).getOrElse { return Result.failure<IStatement>(it) to true }
                statements.add(statement)
            }
            tokens.pop()
            return Result.success(CompoundStatement(statements)) to true
        }
    }
}

class ForLoopStatement(
    val init: INode, val condition: IExpr, val update: Optional<IExpr>, val body: IStatement
) : IStatement {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Pair<Result<IStatement>, Boolean> {
            tokens.popMatching { it == KeywordToken.FOR } ?: return Result.failure<IStatement>(
                ParsingException(
                    KeywordToken.FOR, tokens
                )
            ) to false
            tokens.popMatching { it == CharToken.OPEN_PAREN } ?: return Result.failure<IStatement>(
                ParsingException(
                    CharToken.OPEN_PAREN, tokens
                )
            ) to true
            val index = tokens.index
            val declare = DeclareBlockItem.tryParse(tokens).getOrNull()
            val init = declare ?: run {
                tokens.index = index
                tokens.popMatching { it == CharToken.SEMICOLON }?.let { return@run EmptyStatement() }
                val expr = tryParseExpr(tokens).getOrNull() ?: return Result.failure<IStatement>(
                    ParsingException(
                        "Expression", tokens
                    )
                ) to true
                tokens.popMatching { it == CharToken.SEMICOLON } ?: return Result.failure<IStatement>(
                    ParsingException(
                        CharToken.SEMICOLON, tokens
                    )
                ) to true
                expr
            }
            val condition = tokens.popMatching { it == CharToken.SEMICOLON }?.run { ConstExpr(1) } ?: run {
                val expr = tryParseExpr(
                    tokens
                ).getOrElse { return@tryParse Result.failure<IStatement>(it) to true }
                tokens.popMatching { it == CharToken.SEMICOLON } ?: return@tryParse Result.failure<IStatement>(
                    ParsingException(
                        CharToken.SEMICOLON, tokens
                    )
                ) to true
                expr
            }
            val update = tokens.popMatching { it == CharToken.CLOSE_PAREN }?.run { Optional.empty<IExpr>() } ?: run {
                val expr = tryParseExpr(tokens).getOrElse { return@tryParse Result.failure<IStatement>(it) to true }
                tokens.popMatching { it == CharToken.CLOSE_PAREN } ?: return@tryParse Result.failure<IStatement>(
                    ParsingException(
                        CharToken.CLOSE_PAREN, tokens
                    )
                ) to true
                Optional.of(expr)
            }
            val body = tryParseStatement(tokens).getOrElse { return Result.failure<IStatement>(it) to true }
            return Result.success(ForLoopStatement(init, condition, update, body)) to true
        }
    }
}

class WhileLoopStatement(val condition: IExpr, val body: IStatement) : IStatement {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Pair<Result<IStatement>, Boolean> {
            tokens.popMatching { it == KeywordToken.WHILE } ?: return Result.failure<IStatement>(
                ParsingException(
                    KeywordToken.WHILE, tokens
                )
            ) to false
            tokens.popMatching { it == CharToken.OPEN_PAREN } ?: return Result.failure<IStatement>(
                ParsingException(
                    CharToken.OPEN_PAREN, tokens
                )
            ) to true
            val condition = tryParseExpr(tokens).getOrElse { return Result.failure<IStatement>(it) to true }
            tokens.popMatching { it == CharToken.CLOSE_PAREN } ?: return Result.failure<IStatement>(
                ParsingException(
                    CharToken.CLOSE_PAREN, tokens
                )
            ) to true
            val body = tryParseStatement(tokens).getOrElse { return Result.failure<IStatement>(it) to true }
            return Result.success(WhileLoopStatement(condition, body)) to true
        }
    }
}

class DoWhileLoopStatement(val body: IStatement, val condition: IExpr) : IStatement {
    override fun accept(visitor: INodeVisitor) = visitor.visit(this)

    companion object {
        fun tryParse(tokens: TokenStack): Pair<Result<IStatement>, Boolean> {
            tokens.popMatching { it == KeywordToken.DO } ?: return Result.failure<IStatement>(
                ParsingException(
                    KeywordToken.DO, tokens
                )
            ) to false
            val body = tryParseStatement(tokens).getOrElse { return Result.failure<IStatement>(it) to true }
            tokens.popMatching { it == KeywordToken.WHILE } ?: return Result.failure<IStatement>(
                ParsingException(
                    KeywordToken.WHILE, tokens
                )
            ) to true
            tokens.popMatching { it == CharToken.OPEN_PAREN } ?: return Result.failure<IStatement>(
                ParsingException(
                    CharToken.OPEN_PAREN, tokens
                )
            ) to true
            val condition = tryParseExpr(tokens).getOrElse { return Result.failure<IStatement>(it) to true }
            tokens.popMatching { it == CharToken.CLOSE_PAREN } ?: return Result.failure<IStatement>(
                ParsingException(
                    CharToken.CLOSE_PAREN, tokens
                )
            ) to true
            tokens.popMatching { it == CharToken.SEMICOLON } ?: return Result.failure<IStatement>(
                ParsingException(
                    CharToken.SEMICOLON, tokens
                )
            ) to true
            return Result.success(DoWhileLoopStatement(body, condition)) to true
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
            tokens.popMatching { it == CharToken.SEMICOLON } ?: return Result.failure<IStatement>(
                ParsingException(
                    CharToken.SEMICOLON, tokens
                )
            ) to true
            return Result.success(LoopControlStatement(type)) to true
        }
    }
}

enum class LoopControlType {
    BREAK, CONTINUE
}
