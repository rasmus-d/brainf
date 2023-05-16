module Asm where
    import Types
    import Control.Monad.State
    
    asm :: CFG -> String
    asm (blocks) 
        = "section .text\nglobal main\nextern putchar\nextern getchar\nextern calloc\nextern free\n\n" 
        ++ unlines (map asmblock blocks)
    asmblock :: IRBlock -> String 
    asmblock (IRBlock lbl (irstmts, be)) =
        lbl ++ ":\n" ++ unlines (map asmstmt irstmts) ++ asmblockend be
    asmstmt :: IRStmt -> String
    asmstmt (IAllocArr) = "\tmov rdi, 30000\n\tmov rsi, 1\n\tcall calloc\n\tpush rbp\n\tmov rbp, rax\n\tpush rbp"
    asmstmt (IMoveN n) = "\tadd rbp, " ++ show n
    asmstmt (ISumN n) = "\tadd byte [rbp], " ++ show n
    asmstmt (IPutC) = "\tmov dil, byte [rbp]\n\tcall putchar"
    asmstmt (IGetC) = "\tcall getchar\n\tmov byte [rbp], al"
    asmblockend :: IRBlockend -> String
    asmblockend (Ret) = "\tpop rbp\n\tmov rdi, rbp\n\tcall free\n\tpop rbp\n\txor rax, rax\n\tret"
    asmblockend (Jmp label) = "\tjmp " ++ label
    asmblockend (Branch l1 l2) = "\tcmp byte [rbp], 0\n\tjne " ++ l1 ++ "\n\tjmp " ++ l2
