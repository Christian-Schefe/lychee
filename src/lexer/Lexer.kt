package lexer

fun lex(input: String): TokenStack {
    val tokens = mutableListOf<IToken>()
    var i = 0
    while (i < input.length) {
        val char = input[i]
        when {
            char.isWhitespace() -> {
                i++
            }

            char in CharToken.entries.map { it.char } -> {
                tokens.add(CharToken.entries.first { it.char == char })
                i++
            }

            char.isDigit() -> {
                val value = input.substring(i).takeWhile { it.isDigit() }.toInt()
                tokens.add(IntegerToken(value))
                i += value.toString().length
            }

            char.isLetter() -> {
                val name = input.substring(i).takeWhile { it.isLetter() || it.isDigit() || it == '_' }
                tokens.add(KeywordToken.keywordMap[name] ?: IdentifierToken(name))
                i += name.length
            }

            else -> {
                i++
            }
        }
    }
    return TokenStack(tokens)
}