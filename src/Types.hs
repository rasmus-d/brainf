module Types 
    ( IRStmt(IAllocArr, IMoveN, ISumN, IPutC, IGetC)
        ,  CFGState(..)
        ,  CFG
        ,  Token(..)
        ,  AST
        ,  ErrorInfo
        ,  IRBlockend(Jmp, Branch, Ret)
        ,  IRBlock(IRBlock)
        ,  Label
        ,  Stmt(Rep, MoveR, MoveL, Inc, Dec, PutC, GetC) )  where
    
    type ErrorInfo = (String, Int, Int) 

    data Token 
        = TokPLeft (Int, Int)
        | TokPRight (Int, Int)
        | TokInc (Int, Int)
        | TokDec (Int, Int)
        | TokPoint (Int, Int)
        | TokComma (Int, Int)
        | TokLBrack (Int, Int)
        | TokRBrack (Int, Int) 
        deriving (Eq, Show)

    type AST = [Stmt]
    
    data Stmt 
        = MoveL
        | MoveR
        | Inc 
        | Dec
        | PutC
        | GetC
        | Rep AST 
        deriving (Eq, Show)

    type Label = String

    type CFG = [IRBlock] 

    data IRBlock
        = IRBlock Label ([IRStmt], IRBlockend) 
        deriving (Eq, Show)

    data IRStmt
        = IMoveN Int
        | ISumN Int
        | IPutC
        | IGetC 
        | IAllocArr
        deriving (Eq, Show)

    data IRBlockend
        = Jmp Label
        | Branch Label Label 
        | Ret
        deriving (Eq, Show)

    data CFGState = CFGState 
        { cfgCnt :: Int
        , cfgIrStmts :: [IRStmt]
        , cfgBlocks :: [IRBlock]
        , cfgLabel :: Label 
        , cfgRetLabel :: Maybe Label }
        deriving (Eq, Show)
