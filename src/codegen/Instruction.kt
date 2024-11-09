package codegen

interface IOperand {
    override fun toString(): String
}

enum class Register(val regName: String) : IOperand {
    RAX("%rax"), RBX("%rbx"), RCX("%rcx"), RDX("%rdx"), RSI("%rsi"), RDI("%rdi"), RSP("%rsp"), RBP("%rbp"), R8("%r8"), R9(
        "%r9"
    ),
    R10("%r10"), R11("%r11"), R12("%r12"), R13("%r13"), R14("%r14"), R15("%r15"), AL("%al"), CL("%cl");

    override fun toString(): String {
        return regName
    }
}

data class StackLocation(val offset: Int) : IOperand {
    override fun toString(): String {
        return "-$offset(%rbp)"
    }
}

data class ConstantOperand(val value: Int) : IOperand {
    override fun toString(): String {
        return "$$value"
    }
}

interface IInstruction {
    override fun toString(): String
}

class EntryPointInstruction(val name: String) : IInstruction {
    override fun toString(): String {
        return ".globl $name"
    }
}

class LabelInstruction(val name: String) : IInstruction {
    override fun toString(): String {
        return "$name:"
    }
}

class PushInstruction(val source: IOperand) : IInstruction {
    override fun toString(): String {
        return "pushq $source"
    }
}

class PopInstruction(val dest: IOperand) : IInstruction {
    override fun toString(): String {
        return "popq $dest"
    }
}

class RetInstruction : IInstruction {
    override fun toString(): String {
        return "ret"
    }
}

class IncInstruction(val dest: IOperand) : IInstruction {
    override fun toString(): String {
        return "incq $dest"
    }
}

class DecInstruction(val dest: IOperand) : IInstruction {
    override fun toString(): String {
        return "decq $dest"
    }
}

class NegInstruction(val dest: IOperand) : IInstruction {
    override fun toString(): String {
        return "negq $dest"
    }
}

class NotInstruction(val dest: IOperand) : IInstruction {
    override fun toString(): String {
        return "notq $dest"
    }
}

class MoveInstruction(val source: IOperand, val dest: IOperand) : IInstruction {
    override fun toString(): String {
        return "movq $source, $dest"
    }
}

class SetInstruction(val condition: String, val dest: IOperand) : IInstruction {
    override fun toString(): String {
        return "set$condition $dest"
    }
}

class AddInstruction(val source: IOperand, val dest: IOperand) : IInstruction {
    override fun toString(): String {
        return "addq $source, $dest"
    }
}

class SubInstruction(val source: IOperand, val dest: IOperand) : IInstruction {
    override fun toString(): String {
        return "subq $source, $dest"
    }
}

class MulInstruction(val source: IOperand, val dest: IOperand) : IInstruction {
    override fun toString(): String {
        return "imulq $source, $dest"
    }
}

class DivInstruction(val source: IOperand) : IInstruction {
    override fun toString(): String {
        return "idivq $source"
    }
}

class CmpInstruction(val source: IOperand, val dest: IOperand) : IInstruction {
    override fun toString(): String {
        return "cmpq $source, $dest"
    }
}

class JumpInstruction(val condition: String?, val label: String) : IInstruction {
    override fun toString(): String {
        val conditionStr = condition ?: "mp"
        return "j$conditionStr $label"
    }
}

class AndInstruction(val source: IOperand, val dest: IOperand) : IInstruction {
    override fun toString(): String {
        return "andq $source, $dest"
    }
}

class OrInstruction(val source: IOperand, val dest: IOperand) : IInstruction {
    override fun toString(): String {
        return "orq $source, $dest"
    }
}

class XorInstruction(val source: IOperand, val dest: IOperand) : IInstruction {
    override fun toString(): String {
        return "xorq $source, $dest"
    }
}

class CqtoInstruction : IInstruction {
    override fun toString(): String {
        return "cqto"
    }
}

class ShiftLeftInstruction(val source: IOperand, val dest: IOperand) : IInstruction {
    override fun toString(): String {
        return "shlq $source, $dest"
    }
}

class ShiftRightInstruction(val source: IOperand, val dest: IOperand) : IInstruction {
    override fun toString(): String {
        return "sarq $source, $dest"
    }
}