#!/usr/bin/env node

var library = require('@fortawesome/fontawesome-svg-core').library;
var faicon = require('@fortawesome/fontawesome-svg-core').icon;
var fas = require('@fortawesome/free-solid-svg-icons').fas;
var far = require('@fortawesome/free-regular-svg-icons').far;
var fab = require('@fortawesome/free-brands-svg-icons').fab

library.add(fas,fab,far);

var program = require('commander');
program
  .arguments("<pack> <icon>")
  .action(function(pack, icon) {
    var result = faicon({ prefix: process.argv[2], iconName: process.argv[3] });
    if (result) {
      process.stdout.write(result.html + "\n");
    } else {
      process.stdout.write("Undefined icon.\n");
    }
  })
  .parse(process.argv);
