package lexer

class LexerException(message: String) : Exception(message)

fun lex(input: String): TokenStack {
    val tokens = mutableListOf<IToken>()
    var freeIndex = 0

    while (freeIndex < input.length) {
        if (input[freeIndex].isWhitespace()) {
            freeIndex++
            continue
        }
        val (token, newIndex) = readToken(input, freeIndex) ?: throw LexerException("Invalid token at index $freeIndex")
        tokens.add(token)
        freeIndex = newIndex + 1
    }
    return TokenStack(tokens)
}

fun readToken(input: String, freeIndex: Int): Pair<IToken, Int>? {
    var lookaheadIndex = freeIndex
    var lastValid: Pair<IToken, Int>? = null

    while (lookaheadIndex < input.length) {
        if (input[lookaheadIndex].isWhitespace()) {
            break
        }
        val str = input.substring(freeIndex, lookaheadIndex + 1)
        val token = tryGetToken(str)
        if (token != null) {
            lastValid = token to lookaheadIndex
        }
        lookaheadIndex++
    }
    return lastValid
}

fun isValidIdentifierChar(char: Char) = char.isLetterOrDigit() || char == '_' || char == '$'

fun tryGetToken(input: String): IToken? {
    KeywordToken.tokenMap[input]?.let { return it }
    OperatorToken.tokenMap[input]?.let { return it }
    if (input.length == 1) CharToken.tokenMap[input[0]]?.let { return it }
    if (input.all { it.isDigit() }) return IntegerToken(input.toInt())
    if (input.all { isValidIdentifierChar(it) } && !input[0].isDigit()) return IdentifierToken(input)
    return null
}