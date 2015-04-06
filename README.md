# sucks-rocks
a web application for gathering feedback on small snippets of code

### backend
the backend is a REST-ish api implemented in haskell using the following libraries:
+ Scotty for REST interface declaration
+ Persistent (with the sqlite implementation) for the database access
+ Aeson for serialization

### frontend
the frontend is a single page application implemented in purescript using the following libraries:
+ purescript-thermite as a wrapper for ReactJS
+ purescrpt-transformers for the beautiful `ContT` monad transformer
