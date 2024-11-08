package frontend

import util.withReplacedFileExtension
import java.nio.file.Path
import kotlin.io.path.exists

fun getInputData(args: Array<String>): InputData {
    val filePath = args.firstOrNull() ?: readln()
    val path = Path.of(filePath)
    if (!path.exists()) {
        throw IllegalArgumentException("File not found: $filePath")
    }
    Path.of("", "")
    val outputPath = args.getOrNull(1)?.let { Path.of(it) } ?: path.withReplacedFileExtension("exe")
    return InputData(path, outputPath)
}