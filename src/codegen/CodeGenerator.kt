package codegen

import parser.Program

fun genAssembly(ast: Program): String {
    val assembly = GeneratorVisitor().generateCode(ast)
    return assembly + "\n"
}