-- ------------------------------------------------------------

{- |
   Module     : Network.Server.Janus.DynamicLoader
   Copyright  : Copyright (C) 2006 Christian Uhlig
   License    : MIT

   Maintainer : Christian Uhlig (uhl\@fh-wedel.de)
   Stability  : experimental
   Portability: portable
   Version    : $Id: DynamicLoader.hs, v1.0 2006/11/02 00:00:00 janus Exp $

   Janus Dynamic Loader for Haskell values
   
   This module defines a type called Repository to store values of arbitrary Haskell types. It provides an interface to load 
   these values dynamically into the respective Repository from object files and to access the values by means of keys. Of 
   course there are further functions to manually add values to a Repository or to remove them (based on their keys).
-}

-- ------------------------------------------------------------

{-# OPTIONS -fglasgow-exts -farrows #-}

module Network.Server.Janus.DynamicLoader
    (
    -- data types
      Repository
      
    -- repository interface
    , newRepository
    , newRepositoryA
    , loadComponent
    , addComponent
    , delComponent
    , listComponents
    , countComponents
    , getComponent
    , getComponentDef
    )
where

import Data.Map
import System.Plugins
import Text.XML.HXT.Arrow

import Network.Server.Janus.XmlHelper

type ModuleName     = String
type ObjectName     = String
type RepositoryKey  = String
data Repository a   = Rep RepositoryKey (Map RepositoryKey a)

instance Show (Repository a) where
    show (Rep typ mapping) = typ ++ " " ++ show (keys mapping)          

{- |
Creates a new Repository. This function is to hide the representation of Repository values.
-}
newRepository :: String -> Repository a
newRepository typ = 
    (Rep typ empty)

{- |
Creates a new Repository by means of an Arrow. This Arrow is to hide the representation of Repository values.
-}
newRepositoryA :: String -> JanusArrow s a (Repository b)
newRepositoryA typ = 
    constA (Rep typ empty)

{- |
Loads a new value into a Repository. The first argument denotes the key under which the loaded value shall be stored, the second argument 
denotes the module where the value is defined and the third argument denotes the name of the value in the module. The module shall be 
defined by its fully qualified name. The qualified name is internally extended to the object file's name. If the value loading is 
successful, the extended Repository is returned. Otherwise a textual error message is issued and the Arrow fails. If there already 
is a value stored with the given key it is replaced. 
-}
loadComponent :: RepositoryKey -> ModuleName -> ObjectName -> JanusArrow s (Repository a) (Repository a)
loadComponent repkey modname objname = 
    proc (Rep typ mapping) -> do
        let modname' = (Prelude.map (\x -> if x == '.' then '/' else x) modname) ++ ".o"
        
        -- type safe loading: wrapper <- pdynload modname' ["."] [] typ objname 
        wrapper <- arrIO0 $ load modname' ["."] [] objname         -<< ()
        case wrapper of
            LoadSuccess _ f -> do
                returnA             -< (Rep typ (insert repkey f mapping))
            LoadFailure _ -> 
                zeroArrow           -< () 
                
{- |
Adds a value to a Repository and stores it at a given key (first argument). If there already is a value stored with the given key 
it is replaced.
-}
addComponent :: RepositoryKey -> a -> JanusArrow s (Repository a) (Repository a)
addComponent repkey component = 
    proc (Rep typ mapping) -> do
        returnA -< (Rep typ (insert repkey component mapping))

{- |
Removes a value from a Repository based on a given key.
-}
delComponent :: RepositoryKey -> JanusArrow s (Repository a) (Repository a)
delComponent repkey = 
    proc (Rep typ mapping) -> do
        returnA -< (Rep typ (delete repkey mapping))

{- |
Lists the keys of all values stored in a given Repository. A non-deterministic Arrow is delivered.  
-}
listComponents :: JanusArrow s (Repository a) String
listComponents =
    proc (Rep _ mapping) -> do
        constL $ keys mapping -<< ()
        
{- |
TODO
-}
countComponents :: JanusArrow s (Repository a) Int
countComponents =
    (listA listComponents) >>> arr length

{- |
Returns a value from a Repository based on its key. If the key is not present in the Repository, the Arrow fails.
-}
getComponent :: RepositoryKey -> JanusArrow s (Repository a) a
getComponent repkey = 
    proc (Rep _ mapping) -> do
        if member repkey mapping
            then returnA    -< (mapping ! repkey) 
            else zeroArrow  -< ()    

{- |
Returns a value from a Repository based on its key. If the key is not present in the Repository, a default value (second argument)
is delivered.
-}
getComponentDef :: RepositoryKey -> a -> JanusArrow s (Repository a) a
getComponentDef repkey def = 
    proc (Rep _ mapping) -> do
        returnA -< (if member repkey mapping
                        then (mapping ! repkey) 
                        else (def)
                        )