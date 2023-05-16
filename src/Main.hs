module Main (main) where
    import Lex 
    import Types
    import Asm
    import Parse
    import System.Environment (getArgs)
    import System.Exit (exitFailure, exitWith, ExitCode (ExitFailure))
    import Control.Monad.State (execState, evalState)
    import System.Process
    
    main :: IO ()
    main = do
        args <- getArgs
        fileContent <- readFile $ head args
        ast <- case lexs fileContent >>= parse of
                Right a -> return a
                Left (s,l,c) -> putStrLn (s ++ show l ++ show c) >> exitWith (ExitFailure 1) 
        writeFile "tests/a.asm" $ asm (ir ast)
        _ <- spawnProcess "nasm" ["-felf64", "tests/a.asm"]
        _ <- spawnProcess "gcc" ["-no-pie", "tests/a.o"]
        return ()

