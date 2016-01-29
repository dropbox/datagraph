# DataGraph

An experimental project to explore what a GraphQL server with optimal IO concurrency and batching would look like.

# Resources

## Haxl

- Useful Haxl tutorial: http://gelisam.blogspot.com/2015/01/haxl-anti-tutorial.html
- How to write a data source: https://gist.github.com/gelisam/0549eb2a292f86ca2574/4ef520bd40e2d9a2b660e0fb7f4baddabcaa0eed
- Another Haxl tutorial: https://simonmar.github.io/posts/2015-10-20-Fun-With-Haxl-1.html
- Example Data Source: https://github.com/simonmar/haskell-eXchange-2015/blob/3ae0e34a051201eb77721bee2e940ec1f764a0df/BlogDataSource.hs
- Haxl.Prelude: https://hackage.haskell.org/package/haxl-0.3.1.0/docs/Haxl-Prelude.html
- Haxl.Core: https://hackage.haskell.org/package/haxl-0.3.1.0/docs/Haxl-Core.html

## GraphQL

- The GraphQL spec: https://facebook.github.io/graphql/
- Star Wars schema stuff: https://github.com/graphql/graphql-js/tree/master/src/__tests__

# WORKING

- queries
- redis backend
- backend IO batching
- benchmarks
- mutations, incl. sequencing

# TODO

- nullability
- input coercion
- eliminate as much haxl / graphql boilerplate as possible
- errors (overall envelope: {data: {<query>: <response>}, errors: [...]}
- query tests from https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsQueryTests.js
- type checking
