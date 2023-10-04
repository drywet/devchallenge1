### Starting the app

```docker compose up```

After successful start the app should log something like that:

```
devchallenge1_jkhdfbnsk3  | [info] welcome to sbt 1.9.6 (GraalVM Community Java 17.0.8)
devchallenge1_jkhdfbnsk3  | [info] loading project definition from /root/project
devchallenge1_jkhdfbnsk3  | [info] loading settings for project root from build.sbt ...
devchallenge1_jkhdfbnsk3  | [info] set current project to devchallenge1scala (in build file:/root/)
devchallenge1_jkhdfbnsk3  | [info] running pkg.Main 
```

May need to wait a few extra seconds for the HTTP server to start.

Request example:

```
curl -X POST 'http://localhost:8080/api/v1/sheet1/CeLl3' -d '{"value":"123"}'

{"value":"123","result":"123"}
```

### Running tests

After the container was run once using `docker compose up`, the container would be created and unit tests could be run
using this command:

```docker exec -it devchallenge1_jkhdfbnsk3 sbt test```

### Description

Performance characteristics:

- write complexity: tree traversals are N+M
    - where N is the number of cells using this cell recursively
    - and M is the number of cells this cell refers to recursively
- read complexity: 1

Corner cases covered:

- very long formulas, like a million entries of `1+1+1+...`
- very long cell dependency chains, also in order of a million is quite ok
- a lot of performance tests
- more cases are described in unit tests in `src/test/scala` dir

Future improvements:

- it may be possible to change the core data structure for storing data to find better balance between write-heavy and
  read-heavy load
- an optimiser like relational databases have can be introduced, gathering query statistics and analysing data
  distribution
