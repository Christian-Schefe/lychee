package frontend

fun getInputData(args: Array<String>): InputData {
    val filePath = args.firstOrNull() ?: readln()
    val outputPath = args.getOrNull(1) ?: "a.out"
    return InputData(filePath, outputPath)
}