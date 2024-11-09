package backend

import util.withReplacedFileExtension
import java.io.File
import java.io.IOException
import java.nio.file.Path
import java.util.concurrent.TimeUnit
import kotlin.io.path.absolutePathString
import kotlin.io.path.writeText

fun assemble(assembly: String, outputPath: Path) {
    val assemblyPath = outputPath.withReplacedFileExtension("s")

    println("Writing assembly to: ${assemblyPath.absolutePathString()}")
    assemblyPath.writeText(assembly)
    invokeGcc(assemblyPath, outputPath)
}

fun invokeGcc(assemblyPath: Path, outputPath: Path) {
    val command = arrayOf(
        "C:/cygwin64/bin/x86_64-w64-mingw32-gcc.exe",
        "-o",
        outputPath.absolutePathString(),
        assemblyPath.absolutePathString()
    )
    println("Running: ${command.joinToString(" ")}")
    exec(command, workingDir = File("C:/cygwin64/bin"))
    runExecutable(outputPath)
}

fun runExecutable(executablePath: Path) {
    val command = arrayOf(executablePath.absolutePathString())
    println("Running: ${command.joinToString(" ")}")
    val exitCode = exec(command)?.second
    println("Exit code: $exitCode")
}

fun exec(
    cmd: Array<String>, stdIn: String = "", captureOutput: Boolean = false, workingDir: File = File(".")
): Pair<String, Int>? {
    try {
        val process = ProcessBuilder(*cmd).directory(workingDir)
            .redirectOutput(if (captureOutput) ProcessBuilder.Redirect.PIPE else ProcessBuilder.Redirect.INHERIT)
            .redirectError(if (captureOutput) ProcessBuilder.Redirect.PIPE else ProcessBuilder.Redirect.INHERIT).start()
            .apply {
                if (stdIn != "") {
                    outputStream.bufferedWriter().apply {
                        write(stdIn)
                        flush()
                        close()
                    }
                }
                waitFor(60, TimeUnit.SECONDS)
            }
        val exitCode = process.exitValue()
        if (captureOutput) {
            return process.inputStream.bufferedReader().readText() to exitCode
        }
        return "" to exitCode
    } catch (e: IOException) {
        e.printStackTrace()
    }
    return null
}