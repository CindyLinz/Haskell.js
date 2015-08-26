var util = require('./ULC-util.js');

var code = '';
process.stdin.on('data', function(chunk){
  code += chunk;
});
process.stdin.on('end', function(){
  console.log(util.pretty(JSON.parse(code), 0));
  process.exit();
});
