package codegen

interface IOperand {
    override fun toString(): String
}

enum class DataSize(val postfix: String) {
    BYTE("b"), WORD("w"), DWORD("l"), QWORD("q");

    override fun toString(): String {
        return postfix
    }
}

enum class Register(val regNames: Map<DataSize, String>) {
    AX(
        mapOf(DataSize.QWORD to "%rax", DataSize.DWORD to "%eax", DataSize.WORD to "ax", DataSize.BYTE to "al")
    ),
    BX(
        mapOf(DataSize.QWORD to "%rbx", DataSize.DWORD to "%ebx", DataSize.WORD to "bx", DataSize.BYTE to "bl")
    ),
    CX(
        mapOf(DataSize.QWORD to "%rcx", DataSize.DWORD to "%ecx", DataSize.WORD to "cx", DataSize.BYTE to "cl")
    ),
    DX(
        mapOf(DataSize.QWORD to "%rdx", DataSize.DWORD to "%edx", DataSize.WORD to "dx", DataSize.BYTE to "dl")
    ),
    SI(
        mapOf(DataSize.QWORD to "%rsi", DataSize.DWORD to "%esi", DataSize.WORD to "si", DataSize.BYTE to "sil")
    ),
    DI(
        mapOf(DataSize.QWORD to "%rdi", DataSize.DWORD to "%edi", DataSize.WORD to "di", DataSize.BYTE to "dil")
    ),
    BP(
        mapOf(DataSize.QWORD to "%rbp", DataSize.DWORD to "%ebp", DataSize.WORD to "bp", DataSize.BYTE to "bpl")
    ),
    SP(
        mapOf(DataSize.QWORD to "%rsp", DataSize.DWORD to "%esp", DataSize.WORD to "sp", DataSize.BYTE to "spl")
    );

    fun getStr(size: DataSize): String {
        return regNames[size] ?: error("Invalid size")
    }
}

data class RegOp(val register: Register, val size: DataSize) : IOperand {
    override fun toString(): String {
        return register.getStr(size)
    }

    companion object {
        val RAX = RegOp(Register.AX, DataSize.QWORD)
        val EAX = RegOp(Register.AX, DataSize.DWORD)
        val AX = RegOp(Register.AX, DataSize.WORD)
        val AL = RegOp(Register.AX, DataSize.BYTE)
        val RBX = RegOp(Register.BX, DataSize.QWORD)
        val EBX = RegOp(Register.BX, DataSize.DWORD)
        val BX = RegOp(Register.BX, DataSize.WORD)
        val BL = RegOp(Register.BX, DataSize.BYTE)
        val RCX = RegOp(Register.CX, DataSize.QWORD)
        val ECX = RegOp(Register.CX, DataSize.DWORD)
        val CX = RegOp(Register.CX, DataSize.WORD)
        val CL = RegOp(Register.CX, DataSize.BYTE)
        val RDX = RegOp(Register.DX, DataSize.QWORD)
        val EDX = RegOp(Register.DX, DataSize.DWORD)
        val DX = RegOp(Register.DX, DataSize.WORD)
        val DL = RegOp(Register.DX, DataSize.BYTE)
        val RSI = RegOp(Register.SI, DataSize.QWORD)
        val ESI = RegOp(Register.SI, DataSize.DWORD)
        val SI = RegOp(Register.SI, DataSize.WORD)
        val SIL = RegOp(Register.SI, DataSize.BYTE)
        val RDI = RegOp(Register.DI, DataSize.QWORD)
        val EDI = RegOp(Register.DI, DataSize.DWORD)
        val DI = RegOp(Register.DI, DataSize.WORD)
        val DIL = RegOp(Register.DI, DataSize.BYTE)
        val RBP = RegOp(Register.BP, DataSize.QWORD)
        val EBP = RegOp(Register.BP, DataSize.DWORD)
        val BP = RegOp(Register.BP, DataSize.WORD)
        val BPL = RegOp(Register.BP, DataSize.BYTE)
        val RSP = RegOp(Register.SP, DataSize.QWORD)
        val ESP = RegOp(Register.SP, DataSize.DWORD)
        val SP = RegOp(Register.SP, DataSize.WORD)
        val SPL = RegOp(Register.SP, DataSize.BYTE)
    }
}

data class StackLocation(val offset: Int) : IOperand {
    override fun toString(): String {
        return "${-offset}(%rbp)"
    }
}

open class ConstantOperand<T>(val value: T) : IOperand {
    override fun toString(): String {
        return "$value"
    }

    class Byte(val byteValue: kotlin.Byte) : ConstantOperand<kotlin.Byte>(byteValue)
    class Short(val shortValue: kotlin.Short) : ConstantOperand<kotlin.Short>(shortValue)
    class Int(val intValue: kotlin.Int) : ConstantOperand<kotlin.Int>(intValue)
    class Long(val longValue: kotlin.Long) : ConstantOperand<kotlin.Long>(longValue)
}

interface IInstruction {
    override fun toString(): String
}

class GlobalInstruction(val name: String) : IInstruction {
    override fun toString(): String {
        return ".globl $name"
    }
}

class LabelInstruction(val name: String) : IInstruction {
    override fun toString(): String {
        return "$name:"
    }
}

class PushInstruction(val source: IOperand, val size: DataSize) : IInstruction {
    override fun toString(): String {
        return "push$size $source"
    }
}

class PopInstruction(val dest: IOperand, val size: DataSize) : IInstruction {
    override fun toString(): String {
        return "pop$size $dest"
    }
}

class RetInstruction : IInstruction {
    override fun toString(): String {
        return "ret"
    }
}

class IncInstruction(val dest: IOperand, val size: DataSize) : IInstruction {
    override fun toString(): String {
        return "inc$size $dest"
    }
}

class DecInstruction(val dest: IOperand, val size: DataSize) : IInstruction {
    override fun toString(): String {
        return "dec$size $dest"
    }
}

class NegInstruction(val dest: IOperand, val size: DataSize) : IInstruction {
    override fun toString(): String {
        return "neg$size $dest"
    }
}

class NotInstruction(val dest: IOperand, val size: DataSize) : IInstruction {
    override fun toString(): String {
        return "not$size $dest"
    }
}

class CallInstruction(val name: String) : IInstruction {
    override fun toString(): String {
        return "call $name"
    }
}

class MoveInstruction(val source: IOperand, val dest: IOperand, val size: DataSize) : IInstruction {
    override fun toString(): String {
        return "mov$size $source, $dest"
    }
}

class SetInstruction(val condition: String, val dest: IOperand) : IInstruction {
    override fun toString(): String {
        return "set$condition $dest"
    }
}

class AddInstruction(val source: IOperand, val dest: IOperand, val size: DataSize) : IInstruction {
    override fun toString(): String {
        return "add$size $source, $dest"
    }
}

class SubInstruction(val source: IOperand, val dest: IOperand, val size: DataSize) : IInstruction {
    override fun toString(): String {
        return "sub$size $source, $dest"
    }
}

class MulInstruction(val source: IOperand, val dest: IOperand, val size: DataSize) : IInstruction {
    override fun toString(): String {
        return "imul$size $source, $dest"
    }
}

class DivInstruction(val source: IOperand, val size: DataSize) : IInstruction {
    override fun toString(): String {
        return "idiv$size $source"
    }
}

class CmpInstruction(val source: IOperand, val dest: IOperand, val size: DataSize) : IInstruction {
    override fun toString(): String {
        return "cmp$size $source, $dest"
    }
}

class JumpInstruction(val condition: String?, val label: String) : IInstruction {
    override fun toString(): String {
        val conditionStr = condition ?: "mp"
        return "j$conditionStr $label"
    }
}

class AndInstruction(val source: IOperand, val dest: IOperand, val size: DataSize) : IInstruction {
    override fun toString(): String {
        return "and$size $source, $dest"
    }
}

class OrInstruction(val source: IOperand, val dest: IOperand, val size: DataSize) : IInstruction {
    override fun toString(): String {
        return "or$size $source, $dest"
    }
}

class XorInstruction(val source: IOperand, val dest: IOperand, val size: DataSize) : IInstruction {
    override fun toString(): String {
        return "xor$size $source, $dest"
    }
}

class CqtoInstruction(val size: DataSize) : IInstruction {
    override fun toString(): String {
        return when (size) {
            DataSize.QWORD -> "cqto"
            DataSize.DWORD -> "cltd"
            DataSize.WORD -> "cwtl"
            else -> error("Invalid size")
        }
    }
}

class ShiftLeftInstruction(val source: IOperand, val dest: IOperand, val size: DataSize) : IInstruction {
    override fun toString(): String {
        return "shl$size $source, $dest"
    }
}

class ShiftRightInstruction(val source: IOperand, val dest: IOperand, val size: DataSize) : IInstruction {
    override fun toString(): String {
        return "sar$size $source, $dest"
    }
}