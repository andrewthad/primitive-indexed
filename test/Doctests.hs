import Test.DocTest

main :: IO ()
main = doctest
  [ "src/Data/Primitive/Indexed/Array.hs"
  ]

