package frontend

import java.nio.file.Path
import kotlin.io.path.readText

data class InputData(val filePath: Path, val outputPath: Path) {
    fun readInputText(): String = filePath.readText()
}