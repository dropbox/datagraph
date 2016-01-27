{-# LANGUAGE OverloadedStrings #-}

module StarWarsData where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import StarWarsModel

starWarsCharacters :: HashMap CharacterID Character
starWarsCharacters = HashMap.fromList
  [ ("1000", Character
      { cName = "Luke Skywalker"
      , cFriends = [ "1002", "1003", "2000", "2001" ]
      , cAppearsIn = [ NewHope, Empire, Jedi ]
      , cType = Human $ Just "Tatooine"
      })
  , ("1001", Character
      { cName = "Darth Vader"
      , cFriends = [ "1004" ]
      , cAppearsIn = [ NewHope, Empire, Jedi ]
      , cType = Human $ Just "Tatooine"
      })
  , ("1002", Character
      { cName = "Han Solo"
      , cFriends = [ "1000", "1003", "2001" ]
      , cAppearsIn = [ NewHope, Empire, Jedi ]
      , cType = Human $ Just "Corellia"
      })
  , ("1003", Character
      { cName = "Leia Organa"
      , cFriends = [ "1000", "1002", "2000", "2001" ]
      , cAppearsIn = [ NewHope, Empire, Jedi ]
      , cType = Human $ Just "Alderaan"
      })
  , ("1004", Character
      { cName = "Wilhuff Tarkin"
      , cFriends = [ "1001" ]
      , cAppearsIn = [ NewHope ]
      , cType = Human Nothing
      })
  , ("2000", Character
      { cName = "C-3PO"
      , cFriends = [ "1000", "1002", "1003", "2001" ]
      , cAppearsIn = [ NewHope, Empire, Jedi ]
      , cType = Droid "Protocol"
      })
  , ("2001", Character
      { cName = "R2-D2"
      , cFriends = [ "1000", "1002", "1003" ]
      , cAppearsIn = [ NewHope, Empire, Jedi ]
      , cType = Droid "Astromech"
      })
  ]

starWarsEpisodes :: HashMap EpisodeID Episode
starWarsEpisodes = HashMap.fromList
  [ (NewHope, Episode
      { eName = "Star Wars Episode IV: A New Hope"
      , eReleaseYear = 1977
      })
  , (Empire, Episode
      { eName = "Star Wars Episode V: The Empire Strikes Back"
      , eReleaseYear = 1980
      })
  , (NewHope, Episode
      { eName = "Star Wars Episode VI: Return of the Jedi"
      , eReleaseYear = 1983
      })
  ]

-- TODO: move this into the episode database, replacing calls to this with calls
-- to eHero <$> getEpisode
getHero :: EpisodeID -> IO CharacterID
getHero Empire = return "1002" -- Luke is hero of Episode V.
getHero _ = return "2001" -- R2-D2 is hero of everything else.
