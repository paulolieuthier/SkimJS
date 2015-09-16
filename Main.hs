import Language.ECMAScript3.Parser
import Language.ECMAScript3.Syntax

main :: IO ()
main = do
    js <- parseFromFile "Main.js"
    putStrLn $ show $ unJavaScript js
