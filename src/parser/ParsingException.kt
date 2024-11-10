package parser

import lexer.IToken
import lexer.TokenStack

class ParsingException(message: String) : Exception(message) {
    constructor(
        token: String, tokens: TokenStack
    ) : this(tokens.curLocation?.let {
        "At Ln. ${it.line}, Col. ${it.column} - Expected '$token', found '${it.token}'"
    } ?: "Expected '$token', found EOF")
}

fun <T> succeed(value: T): Result<T> = Result.success(value)
fun <T> succeedTrue(value: T): Pair<Result<T>, Boolean> = Result.success(value) to true

fun <T> fail(value: Throwable): Result<T> = Result.failure(value)
fun <T> fail(value: Throwable, propagate: Boolean): Pair<Result<T>, Boolean> = Result.failure<T>(value) to propagate
fun <T> fail(expected: IToken, tokens: TokenStack): Result<T> =
    Result.failure(ParsingException(expected.toString(), tokens))

fun <T> fail(expected: String, tokens: TokenStack): Result<T> = Result.failure(ParsingException(expected, tokens))
fun <T> fail(expected: IToken, tokens: TokenStack, propagate: Boolean): Pair<Result<T>, Boolean> =
    fail(expected.toString(), tokens, propagate)

fun <T> fail(expected: String, tokens: TokenStack, propagate: Boolean): Pair<Result<T>, Boolean> =
    Result.failure<T>(ParsingException(expected, tokens)) to propagate