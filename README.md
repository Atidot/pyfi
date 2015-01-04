<h2>Installation</h2>

    cabal update && cabal install pyfi
    
<h2>Purpose</h2>

Pyfi (PYthon Function Interface) lets you call python from haskell without writing C API code. Just define any python function inline, tell haskell the type signature, and let pyfi figure out the rest. The library uses json serialization for basic types and pointers for references to more complex python objects. It's great for either wrapping your existing python projects with haskell or using a python library from a haskell script. At this time, Pyfi only supports single threaded haskell applications.

<h2>Tutorial</h2>
First, let's see an example

    -- Square.hs
    import Python
    
    square :: Int -> IO Int
    square = defVV "export = lambda x: x * x"
    
    main = do
      x <- square 7
      print x
  
  <p></p>
  
    $ runhaskell Square.hs
    49
  
  The `defVV` function above has type `(ToJSON a, FromJSON b) => String -> (a -> IO b)`. The type signature for `square` further constrains `a` to be of type `Int`, and `b` to be of type `Int` as well. `defVV` takes a string of python code defining a function with the name `export`, and returns a high level interface to that function that can be called from haskell. We could have just as well written:
      
      square :: Int -> IO Int
      square = defVV "def export(x): return x * x"
      
  or 
  
      square :: Int -> IO Int
      square = defVV "from numpy import square as export"
      
  What's imporant is that the python code defines a function `export` of type `Int -> IO Int`. If the python returns a value that cannot be interpreted as an `Int`, pyfi will raise a haskell exception. Any exceptions raised by pyfi can be handled with `Control.Exception.catch`. Of course, we could have also defined a function of type `String -> IO String`:
  
      guess_type :: String -> IO String
      guess_type = defVV "from mimetypes import guess_type; export = lambda x: guess_type(x)[0]"
  
  or a function of type `Float -> Float -> IO Float`:
  
      sin_product :: Float -> Float -> IO Float
      sin_product = defVVV "def export(x, y): from math import sin; return sin(x*y)
      
  Where `defVVV` is used to denote that the python function will take two arguments by Value and return a Value. But while `defVV` and `defVVV` let us easily wrap python functions, they come at a cost. Any variables that are passed to and from the python function must be serialized into a json string and then deserialized. At some level, this cost is inevitable. Using haskell data in python is going to require instantiating python objects, which will require replicating memory. But if one is passing the same data into python again and again, it shouldn't be necessary to repeatedly serialize. Also, not every python data structure will implement json encoding and decoding. That's why pyfi supports passing around direct references to python objects:
  
    {-# LANGUAGE QuasiQuotes #-}
    -- ShortestPaths.hs
    
    import Python
    
    data Graph
    
    getGraph :: [(Int, Int)] -> IO (PyObject Graph)
    getGraph = defVO [str|
    import networkx
    def export(xs):
        g = networkx.Graph()
        for edge in xs:
            g.add_edge(edge[0], edge[1])
        return g
    |]
    
    shortestPath :: (Int, Int) -> PyObject Graph -> IO Int
    shortestPath = defVOV [str|
    import networkx
    def export(x, g):
        return int(networkx.shortest_path_length(g, x[0], x[1]))
    |] 
    
    main = do
      g <- getGraph [(1,2),(2,3),(3,4)]
      distance <- shortestPath (1,4) g
      print distance
  
  <p></p>
  
    $ runhaskell ShortestPaths.hs
    3
  
  Notice the function `defVO` above returns a graph object from the networkx library. It would be quite tricky to make sure that a json serialization of this object encoded all the necessary properties, but we don't have to. We just store a reference to that object. We may not be able to manipulate the object directly in haskell, but we <i>can</i> pass it to other functions wrapped with pyfi, such as the `shortestPath` function defined above. Even better, the object passing in pyfi is implemented with Foreign Pointers, so when `g` has no remaining references, it will decrement the python reference count to the object, causing it to be garbage collected in python. Even though pyfi manipulates python with the C API, you don't have to do any manual memory management. In fact, pyfi should never cause a SegFault (if it does, please report it as a bug).
  
  By now, you may have noticed a pattern in the naming convention of the `def` functions. There is one letter for each of the arguments and for the return type. A `V` indicates that the type will be passed as a json value. An `O` indicates that the type will be passed as a pointer to a python object. You can use any combination of `O` and `V` describing python functions with up to three arguments. So `defV`, `defVOOV`, and `defOOVV` are all available to you after you write `import Python`.
  
  The technology for catching python exceptions and bubbling them up into haskell exceptions isn't finalized, and it hasn't been tested for thread safety with concurrent IO. Nonetheless, I'm really excited to see what people can do with it.
