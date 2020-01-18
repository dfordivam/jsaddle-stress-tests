import qualified Tests
import Language.Javascript.JSaddle.Wasm (run)
import Reflex.Dom.Core

main :: IO ()
main = run 0 $ mainWidget Tests.main
