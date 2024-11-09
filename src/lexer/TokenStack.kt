package lexer

import java.util.function.Function
import java.util.function.Predicate

class TokenStack(val tokens: List<TokenSource>) {
    val curLocation get() = tokens.getOrNull(index)
    var index = 0
    val count = tokens.size
    val remaining: Int
        get() = count - index

    fun pop(): IToken? {
        val token = peek() ?: return null
        index++
        return token
    }

    fun peek(): IToken? {
        return tokens.getOrNull(index)?.token
    }

    fun at(index: Int): IToken? {
        return tokens.getOrNull(index)?.token
    }

    fun popMatching(predicate: Predicate<IToken>): IToken? {
        val token = peek() ?: return null
        if (predicate.test(token)) {
            index++
            return token
        }
        return null
    }

    fun peekMatching(predicate: Predicate<IToken>): IToken? {
        val token = peek() ?: return null
        if (predicate.test(token)) {
            return token
        }
        return null
    }

    fun <T> peekTransformMatching(func: Function<IToken, T?>): T? {
        val token = peek() ?: return null
        return func.apply(token)
    }

    fun <T> consumeFn(func: Function<IToken, T?>): T? {
        val token = peek() ?: return null
        val result = func.apply(token) ?: return null
        index++
        return result
    }
}