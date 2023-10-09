This is a solution for
the [backend devchallenge 2023](https://app.devchallenge.it/tasks/online-round-85f3a4b4-f7a2-4176-8ff8-350051ce576b),
see the [task description](#task-description) below.

## Solution

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

## Task description

### Description

We all know there is no better software in the world than Excel
The powerful idea behind the cells and formulas allows many of us to understand programming.
Today is your time to pay respect to spreadsheets and implement backend API for this fantastic tool.

### Description of input data

As a user, I want to have API service with exact endpoints:

```
POST /api/v1/:sheet_id/:cell_id accept params {"value": "1"} implements UPSERT strategy (update or insert) for both sheet_id and cell_id
1) 201 if the value is OK
2) 422 if the value is not OK e.g. new value leads to dependent formula ERROR compilation
   Examples:
   POST /api/v1/devchallenge-xx/var1 with {"value:": "0"}
   Response: {"value:": "0", "result": "0"}
   POST /api/v1/devchallenge-xx/var1 with {"value:": "1"}
   Response: {"value:": "1", "result": "1"}
   POST /api/v1/devchallenge-xx/var2 with {"value": "2"}
   Response: {"value:": "2", "result": "2"}
   POST /api/v1/devchallenge-xx/var3 with {"value": "=var1+var2"}
   Response: {"value": "=var1+var2", "result": "3"}
   POST /api/v1/devchallenge-xx/var4 with {"value": "=var3+var4"}
   Response: {"value": "=var3+var4", "result": "ERROR"}
```

```
GET  /api/v1/:sheet_id/:cell_id
1) 200 if the value present
2) 404 if the value is missing
   Examples:
   GET /api/v1/devchallenge-xx/var1
   Response: {"value": "1", result: "1"}
   GET /api/v1/devchallenge-xx/var1
   Response: {"value": "2", result: "2"}
   GET /api/v1/devchallenge-xx/var3
   Response: {"value": "=var1+var2", result: "3"}
```

```
GET /api/v1/:sheet_id
1) 200 if the sheet is present
2) 404 if the sheet is missing
   Response:
   {
     "var1": {"value": "1", "result": "1"},
     "var2": {"value": "2", "result": "2"},
     "var3": {"value": "=var1+var2", "result": "3"}
   }
```

### Requirements

1. Supports basic data types: string, integer, float
2. Support basic math operations like +, -, /, * and () as well.
3. :sheet_id - should be any URL-compatible text that represents the namespace and can be generated on the client
4. :cell_id - should be any URL-compatible text that represents a cell (variable) and can be generated on the client
5. :sheet_id and :cell_id are case-insensitive
6. Be creative and cover as many corner cases as you can. Please do not forget to mention them in the README.md
7. Data should be persisted and available between docker containers restarts

### Format of presentation of results

Upload the source on the platform in one file archive with the name in the format FileName.zip.  
☝️ Please note that the .git directory should not be present in the archive.  
☝️ Please note that the name of the archive and file names inside the archive should not contain your first or last
name. The size of the solution archive should not exceed 10 MB.

The Organizers and Judges reserve the right to disqualify the Participant's work if the work:

- Contains any reference to the Participant's name, surname, e-mail address, company, address, or other personal data;
- Completed in a different format than specified in the task;
- Performed with the help of third parties, and not by the Participant personally.

The archive should contain

1. 'docker-compose.yml' file in the root, which starts a server with the given endpoints on port 8080, available from
   localhost as 'http://localhost:8080/'.
2. README.md, where you wrote instructions on how to start service and tests and some thoughts about your choices during
   performing this task and the next steps to make your service better.
3. Your test suite should also run inside docker with a single command line. Please leave instructions in the README.md.

### Submission Deadline

October 5, at 00:00 (GMT+3) — after the time runs out, the possibility of uploading works to the platform will be
automatically blocked. The participants who have moved on to the Final will be announced on October 16, 2023.  
☝️ Set your time zone in the settings on the DEV Challenge platform to more conveniently track the time of the deadline.

### Evaluation Criteria

Technical assessment

1. Result Correctness — 90 points.
2. Following API Format — 40 points.
3. Performance — 80 points.
4. Code quality — 40 points.
5. Test — 80 points.
6. Corner cases — 54 points.

☝️ Participants in the Backend nomination will be divided into Hard and Lite levels in the Final.

### Contacts

Questions and clarifications regarding the content of tasks:  
Slack channel: [#nomination-backend](https://devchallengehq.slack.com/archives/C01AU5BJNMQ).  
☝️ Judges will ignore questions that do not relate to the tasks of the Championship.

Organizational questions:  
Contact us via e-mail at hello@devchallenge.it or Slack
channel [#02-ask-the-organizers](https://devchallengehq.slack.com/archives/C0431P42S0L).
