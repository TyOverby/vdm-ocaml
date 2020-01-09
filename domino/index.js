var domino = require('domino');
var Element = domino.impl.Element; // etc

global.window = domino.createWindow('<h1>Hello world</h1>', 'http://example.com');
global.document = window.document;
