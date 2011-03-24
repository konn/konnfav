import Controller (withKonnFav)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = withKonnFav $ run 3000
