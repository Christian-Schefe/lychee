package frontend

class InputData(val filePath: String, val outputPath: String) {
    val text: String by lazy {
        java.io.File(filePath).readText()
    }
}