wrk.scheme = "http"
wrk.port = 8080
wrk.path = "/graphql"
wrk.method = "POST"
wrk.body = "query HeroNameQuery { newhope_hero: hero(episode: NEWHOPE) { name } empire_hero: hero(episode: EMPIRE) { name } jedi_hero: hero(episode: JEDI) { name } } query EpisodeQuery { episode(id: NEWHOPE) { name releaseYear } }"
wrk.headers["Content-Type"] = "application/json"
