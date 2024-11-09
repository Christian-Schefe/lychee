import backend.assemble
import codegen.genAssembly
import frontend.getInputData
import lexer.lex
import parser.PrintVisitor
import parser.parse

fun main(args: Array<String>) {
    val inputData = getInputData(args)
    println(inputData)
    val tokens = lex(inputData.readInputText())
    println(tokens.tokens.joinToString("\n"))
    val ast = parse(tokens)
    PrintVisitor().visit(ast)
    val assembly = genAssembly(ast)
    println(assembly)
    assemble(assembly, inputData.outputPath)
}