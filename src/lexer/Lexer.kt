package lexer

class LexerException(message: String) : Exception(message)

fun lex(input: String): TokenStack {
    val tokens = mutableListOf<TokenSource>()
    var freeIndex = 0
    var lineCounter = 1
    var columnCounter = 1

    while (freeIndex < input.length) {
        if (input[freeIndex].isWhitespace()) {
            if (input[freeIndex] == '\n') {
                lineCounter++
                columnCounter = 1
            } else {
                columnCounter++
            }
            freeIndex++
            continue
        }
        val (token, newIndex) = if (input[freeIndex] == '"') {
            readString(input, freeIndex)
        } else {
            readToken(input, freeIndex) ?: throw LexerException("Invalid token at ln $lineCounter col $columnCounter")
        }
        val tokenSource = TokenSource(token, lineCounter, columnCounter)
        tokens.add(tokenSource)
        columnCounter += newIndex + 1 - freeIndex
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
        if (token is SymbolToken && !token.shouldContinue) {
            break
        }
        lookaheadIndex++
    }
    return lastValid
}

fun readString(input: String, freeIndex: Int): Pair<StringToken, Int> {
    var lookaheadIndex = freeIndex + 1
    while (lookaheadIndex < input.length && input[lookaheadIndex] != '"') {
        if (input[lookaheadIndex] == '\\') {
            lookaheadIndex++
        }
        lookaheadIndex++
    }
    if (lookaheadIndex == input.length) {
        throw LexerException("Unterminated string at ln ${freeIndex + 1}")
    }
    return StringToken(input.substring(freeIndex + 1, lookaheadIndex)) to lookaheadIndex
}

fun isValidIdentifierChar(char: Char) = char.isLetterOrDigit() || char == '_' || char == '$'

fun tryGetToken(input: String): IToken? {
    KeywordToken.tokenMap[input]?.let { return it }
    SymbolToken.tokenMap[input]?.let { return it }
    if (input.all { it.isDigit() }) return IntegerToken(input.toInt())
    if (input.all { isValidIdentifierChar(it) } && !input[0].isDigit()) return IdentifierToken(input)
    return null
}