curl  -v localhost:8000/haskell/add -d "Haskell 1"
curl  -v localhost:8000/haskell/add -d "Haskell 2"
curl  -v localhost:8000/haskell/add -d "Haskell 3"

curl  -v localhost:8000/rust/add -d "Rust 1"
curl  -v localhost:8000/rust/add -d "Rust 2"
curl  -v localhost:8000/rust/add -d "Rust 3"

curl  -v localhost:8000/python/add -d "Python 1"
curl  -v localhost:8000/python/add -d "Python 2"
curl  -v localhost:8000/python/add -d "Python 3"

curl  -v localhost:8000/python/view
curl  -v localhost:8000/haskell/view

curl  -v localhost:8000/list
