import assembler.assemble
import codegen.genAssembly
import frontend.getInputData
import lexer.lex
import parser.parse

fun main(args: Array<String>) {
    val inputData = getInputData(args)
    val tokens = lex(inputData.text)
    println(tokens.tokens)
    val ast = parse(tokens)
    println(ast)
    val assembly = genAssembly(ast)
    println(assembly)
    assemble(assembly, inputData.outputPath)
}