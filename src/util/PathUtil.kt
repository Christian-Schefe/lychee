package util

import java.nio.file.Path
import kotlin.io.path.nameWithoutExtension


fun Path.withReplacedFileExtension(newExtension: String): Path {
    return this.resolveSibling("${this.nameWithoutExtension}.$newExtension")
}