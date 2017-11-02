'use strict';

var express = require('express');
var graphqlHTTP = require('express-graphql');
var buildSchema = require('graphql').buildSchema;

var fs = require('fs');

var schemaContent = fs.readFileSync(__dirname + '/schema.graphql', 'utf-8');
var schema = buildSchema(schemaContent);


var root = {
  person: (args) => ({
    id: args.id,
    name: "David",
    friends: ()=>[
      {
        id: 999,
        name: "Ana"
      }
    ]
  })
};

var app = express();
app.use('/graphql', graphqlHTTP({
  schema: schema,
  rootValue: root,
  graphiql: true
}));

const PORT = 4000;

app.listen(PORT, () => console.log(`Now browse to localhost:${PORT}/graphql`));
