package parser

import lexer.IToken
import lexer.TokenStack

fun parse(tokens:TokenStack): Program {
    return Program.parse(tokens)
}