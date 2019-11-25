var {JSDOM} = require("jsdom");

global.__refresh_dom_node = function () {
 if (global.__dom) {
   global.__dom.window.close();
 }

 global.__dom = new JSDOM("");
 global.window = dom.window;
 global.document = dom.window.document
};
