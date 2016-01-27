import redis
import json

NewHope, Empire, Jedi = range(4, 7)

starWarsCharacters = {
    "1000": {
        "name": "Luke Skywalker",
        "friends": [ "1002", "1003", "2000", "2001" ],
        "appearsIn": [ NewHope, Empire, Jedi ],
        "homePlanet": "Tatooine",
    },
    "1001": {
        "name": "Darth Vader",
        "friends": [ "1004" ],
        "appearsIn": [ NewHope, Empire, Jedi ],
        "homePlanet": "Tatooine",
    },
    "1002": {
        "name": "Han Solo",
        "friends": [ "1000", "1003", "2001" ],
        "appearsIn": [ NewHope, Empire, Jedi ],
        "homePlanet": "Corellia",
    },
    "1003": {
        "name": "Leia Organa",
        "friends": [ "1000", "1002", "2000", "2001" ],
        "appearsIn": [ NewHope, Empire, Jedi ],
        "homePlanet": "Alderaan",
    },
    "1004": {
        "name": "Wilhuff Tarkin",
        "friends": [ "1001" ],
        "appearsIn": [ NewHope ],
        "homePlanet": None,
    },
    "2000": {
        "name": "C-3PO",
        "friends": [ "1000", "1002", "1003", "2001" ],
        "appearsIn": [ NewHope, Empire, Jedi ],
        "primaryFunction": "Protocol",
    },
    "2001": {
        "name": "R2-D2",
        "friends": [ "1000", "1002", "1003" ],
        "appearsIn": [ NewHope, Empire, Jedi ],
        "primaryFunction": "Astromech",
    },
}

starWarsEpisodes = {
    NewHope: {
        "name": "Star Wars Episode IV: A New Hope",
        "releaseYear": 1977,
        "hero": "2001",
    },
    Empire: {
        "name": "Star Wars Episode V: The Empire Strikes Back",
        "releaseYear": 1980,
        "hero": "1000",
    },
    Jedi: {
        "name": "Star Wars Episode VI: Return of the Jedi",
        "releaseYear": 1983,
        "hero": "2001",
    },
}

r = redis.StrictRedis(host='localhost', port=6379, db=0)

for characterID, character in starWarsCharacters.items():
    r.set("CHARACTER:" + characterID, json.dumps(character))

for episodeID, episode in starWarsEpisodes.items():
    r.set("EPISODE:" + str(episodeID), json.dumps(episode))
