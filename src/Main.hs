module Main (main) where
    import Lex ( lexs ) 
    import Asm ( asm )
    import Parse ( ir, parse )
    import System.Environment (getArgs)
    import System.Exit (exitWith, ExitCode (ExitFailure))
    import System.Process ( spawnProcess )
    
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

