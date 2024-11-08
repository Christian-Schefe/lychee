package parser

import lexer.TokenStack

fun parse(tokens:TokenStack): Program {
    return Program.parse(tokens)
}