module Parse(parse, ir) where
    import Control.Monad.State ( evalState, State, gets, MonadState(put, get) ) 
    import Types
        ( IRStmt(IAllocArr, IMoveN, ISumN, IPutC, IGetC)
        ,  CFGState(..)
        ,  CFG
        ,  Token(..)
        ,  AST
        ,  ErrorInfo
        ,  IRBlockend(Jmp, Branch, Ret)
        ,  IRBlock(IRBlock)
        ,  Label
        ,  Stmt(Rep, MoveR, MoveL, Inc, Dec, PutC, GetC) ) 
    
    parse :: [Token] -> Either ErrorInfo AST
    parse toks = do
        (ast, leftover) <- parse' toks []
        case leftover of 
            [] -> return ast
            TokRBrack (l,c) : _ -> Left $ ("unmatched brackets",l,c)  
            _ -> Left $ ("Unknown parse error", 0, 0)

    parse' :: [Token] -> AST -> Either ErrorInfo (AST, [Token])
    parse' [] ast = Right $ (reverse ast, [])
    parse' (TokPLeft _:toks) ast = parse' toks (MoveL : ast)
    parse' (TokPRight _:toks) ast = parse' toks (MoveR : ast)
    parse' (TokInc _:toks) ast = parse' toks (Inc : ast)
    parse' (TokDec _:toks) ast = parse' toks (Dec : ast)
    parse' (TokPoint _:toks) ast = parse' toks (PutC : ast)
    parse' (TokComma _:toks) ast = parse' toks (GetC : ast)
    parse' (TokLBrack (l,c):toks) ast = do
        (inner, leftover) <- parse' toks []
        case leftover of
            TokRBrack _:toks' -> parse' toks' (Rep inner : ast)
            _ -> Left $ ("unmatched brackets", l, c)
    parse' toks@(TokRBrack _:_) ast = Right $ (reverse ast, toks) 
        
    newLabel :: String -> State CFGState Label
    newLabel str = do
        --s <- get
        cnt <- gets cfgCnt
        --put s {cfgCnt = cnt + 1}
        return (str ++ show cnt)

    ir' :: AST -> State CFGState (CFG)
    ir' stmts@(MoveL:_)= do
        s <- get
        irstmts <- gets cfgIrStmts
        let (movs,rest) = span (`elem` [MoveL, MoveR]) stmts
        let movsum = foldl (\x y -> if y == MoveL then x-1 else x+1) 0 movs
        put s { cfgIrStmts = IMoveN movsum : irstmts}
        ir' rest
    ir' stmts@(MoveR:_)= do
        s <- get
        irstmts <- gets cfgIrStmts
        let (movs,rest) = span (`elem` [MoveL, MoveR]) stmts
        let movsum = foldl (\x y -> if y == MoveL then x-1 else x+1) 0 movs
        put s { cfgIrStmts = IMoveN movsum : irstmts}
        ir' rest
    ir' stmts@(Inc:_) = do
        s <- get
        irstmts <- gets cfgIrStmts
        let (incdecs,rest) = span (`elem` [Inc, Dec]) stmts
        let sums = foldl (\x y -> if y == Dec then x-1 else x+1) 0 incdecs
        put s { cfgIrStmts = ISumN sums : irstmts}
        ir' rest 
    ir' stmts@(Dec:_) = do
        s <- get
        irstmts <- gets cfgIrStmts
        let (incdecs,rest) = span (`elem` [Inc, Dec]) stmts
        let sums = foldl (\x y -> if y == Dec then x-1 else x+1) 0 incdecs
        put s { cfgIrStmts = ISumN sums : irstmts}
        ir' rest
    ir' (PutC:t) = do 
        s <- get
        irstmts <- gets cfgIrStmts
        put s {cfgIrStmts = IPutC : irstmts}
        ir' t
    ir' (GetC:t) = do
        s <- get
        irstmts <- gets cfgIrStmts
        put s {cfgIrStmts = IGetC : irstmts}
        ir' t
    ir' (Rep ast : t) = do
        irstmts <- gets cfgIrStmts
        label <- gets cfgLabel
        blocks <- gets cfgBlocks
        loopLabel <- newLabel "loop"
        loopBody <- newLabel "loopbody"
        afterLoop <- newLabel "afterloop"
        let block = IRBlock label (reverse irstmts, Jmp loopLabel)
        let loopblock = IRBlock loopLabel ([], Branch loopBody afterLoop)
        s <- get
        put s 
            { cfgCnt = cfgCnt s + 1
            , cfgBlocks = blocks ++ [block, loopblock]
            , cfgLabel = loopBody
            , cfgRetLabel = Just loopLabel
            , cfgIrStmts = [] }
        innerBlock <- ir' ast
        s' <- get
        put s'
            { cfgCnt = cfgCnt s'
            , cfgBlocks = innerBlock
            , cfgLabel = afterLoop
            , cfgRetLabel = cfgRetLabel s
            , cfgIrStmts = [] }
        ir' t
    ir' [] = do 
        irstmts <- gets cfgIrStmts
        label <- gets cfgLabel
        retLabel <- gets cfgRetLabel
        blocks <- gets cfgBlocks
        let blockend = case retLabel of
                Nothing -> Ret
                Just looplbl -> Jmp looplbl
        return $ case blocks of 
            --(IRBlock "main" (irs,be)):t -> ((IRBlock "main" (IPushStack:irs,be)):t) ++ [IRBlock label (reverse irstmts, blockend)]
            _ -> blocks ++ [IRBlock label (reverse irstmts, blockend)]

        
    
    ir :: AST -> CFG 
    ir ast = evalState (ir' ast) (CFGState 0 [IAllocArr] [] "main" Nothing)
            

