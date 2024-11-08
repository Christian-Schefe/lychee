package assembler

import java.io.File
import java.io.IOException
import java.util.concurrent.TimeUnit

fun assemble(assembly: String, outputPath: String) {
    val assemblyOutputPath = outputPath + ".s"
    writeAssemblyFile(assembly, assemblyOutputPath)
    invokeGcc(assemblyOutputPath, outputPath)
}

fun writeAssemblyFile(assembly: String, outputPath: String) {
    val workingDirPath = System.getProperty("user.dir")
    val absOutputPath = workingDirPath + "/" + outputPath
    File(absOutputPath).writeText(assembly)
}

fun invokeGcc(inputFilePath: String, outputFilePath: String) {
    val workingDirPath = System.getProperty("user.dir")
    val absInputPath = workingDirPath + "/" + inputFilePath
    val absOutputPath = workingDirPath + "/" + outputFilePath
    val command = "C:/cygwin64/bin/x86_64-w64-mingw32-gcc.exe -o \"$absOutputPath\" \"$absInputPath\""
    exec(command, workingDir = File("C:/cygwin64/bin"))
}

fun exec(cmd: String, stdIn: String = "", captureOutput: Boolean = false, workingDir: File = File(".")): String? {
    try {
        val process = ProcessBuilder(*cmd.split("\\s".toRegex()).toTypedArray()).directory(workingDir)
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
        if (captureOutput) {
            return process.inputStream.bufferedReader().readText()
        }
    } catch (e: IOException) {
        e.printStackTrace()
    }
    return null
}