var util = require('./ULC-util.js');
var pretty = util.pretty;
var pretty_closure = util.pretty_closure;

var fs = require('fs');

function clone_env(env){
  var ctor = new Function;
  ctor.prototype = env;
  return new ctor;
}

function clone_array(array){
  var out = new Array(array.length), i;
  for(i=0; i<array.length; ++i)
    out[i] = array[i];
  return out;
}

function weak_normal_form(closure, cb){
  //console.log('weak_normal_form');
  //console.log(pretty_closure(closure, 1));
  var lam, env2, new_lam, new_closure, app_cont;
  switch(closure.expr[0]){
    case 'var':
      if( closure.env[closure.expr[1]] ){
        new_closure = weak_normal_form(closure.env[closure.expr[1]], cb);
        if( !new_closure )
          return;
        closure.env = new_closure.env;
        closure.expr = new_closure.expr;
      }
      return closure;

    case 'lam':
      return closure;

    case 'app':
      app_cont = function(lam){
        if( lam.expr[0]==='int' ){
          new_lam = {
            expr: clone_array(lam.expr),
            env: lam.env
          };
          --new_lam.expr[2];
          new_lam.expr.push({expr: closure.expr[2], env: closure.env});
          new_closure = weak_normal_form(new_lam, cb);
          if( !new_closure )
            return;
        }
        else if( lam.expr[0]==='lam' ){
          env2 = clone_env(lam.env);
          env2[lam.expr[1]] = {expr: closure.expr[2], env: closure.env};
          new_closure = weak_normal_form({expr: lam.expr[2], env: env2}, cb);
          if( !new_closure )
            return;
        }
        else{
          throw 'non-appable app: ' + JSON.stringify(lam);
        }
        closure.env = new_closure.env;
        closure.expr = new_closure.expr;
        return closure;
      };

      lam = weak_normal_form({expr: closure.expr[1], env: closure.env}, function(lam){
        var res = app_cont(lam);
        if( res )
          cb(res);
      });
      if( lam )
        return app_cont(lam);
      else
        return;

    case 'int':
      if( closure.expr[2]===0 ){
        new_closure = closure.expr[3].apply(closure.env, closure.expr.slice(4).concat([cb]));
        if( !new_closure )
          return;
        closure.env = new_closure.env;
        closure.expr = new_closure.expr;
      }
      return closure;
  }
}

function lc_char_to_js_char(lc_char){
  if( lc_char.length==4 ){
    switch(lc_char.charAt(2)){
      case 'n': return '\n';
      case 's': return ' ';
      default: return lc_char.charAt(2);
    }
  }
  else
    return lc_char.charAt(1);
}
function js_str_to_lc_str(js_str){
  var gen = function(i){
    if( i<js_str.length )
      return {expr: ['lam', 'is-cons', ['lam', 'is-nil', ['app', ['app', ['var', 'is-cons'], ['var', "'"+js_str.charAt(i)+"'"]], ['int', 'js_str_to_lc_str', 0, gen, i+1]]]], env: {}};
    else
      return {expr: ['lam', 'is-cons', ['lam', 'is-nil', ['var', 'is-nil']]], env: {}};
  };
  return {expr: ['int', 'js_str_to_lc_str', 0, gen, 0], env: {}};
}
function lc_str_to_js_str(lc_str){
  var out = '';
  var take_cons = function(a, as){
    out += lc_char_to_js_char(weak_normal_form(a).expr[1]);
    weak_normal_form({
      expr: ['app', ['app', as.expr, ['int', 'lc_str_to_js_str-cons', 2, take_cons]], ['int', 'lc_str_to_js_str-nil', 0, take_nil]],
      env: as.env
    });
  };
  var take_nil = function(){};
  weak_normal_form({
    expr: ['app', ['app', lc_str.expr, ['int', 'lc_str_to_js_str-cons', 2, take_cons]], ['int', 'lc_str_to_js_str-nil', 0, take_nil]],
    env: lc_str.env
  });
  return out;
}

var input_buffer = '';
var input_eof = false;
var input_wait = null;

function run(expr, done){
  var i;
  var env = {
    '+': {
      expr: ['int', '+', 2, function(a, b){
        return {expr: ['var', (weak_normal_form(a).expr[1]|0) + (weak_normal_form(b).expr[1]|0)], env: {}};
      }],
      env: {}
    },

    '-': {
      expr: ['int', '-', 2, function(a, b){
        return {expr: ['var', (weak_normal_form(a).expr[1]|0) - (weak_normal_form(b).expr[1]|0)], env: {}};
      }],
      env: {}
    },

    '*': {
      expr: ['int', '*', 2, function(a, b){
        return {expr: ['var', (weak_normal_form(a).expr[1]|0) * (weak_normal_form(b).expr[1]|0)], env: {}};
      }],
      env: {}
    },

    '%': {
      expr: ['int', '%', 2, function(a, b){
        return {expr: ['var', (weak_normal_form(a).expr[1]|0) % (weak_normal_form(b).expr[1]|0)], env: {}};
      }],
      env: {}
    },

    '<=': {
      expr: ['int', '<=', 2, function(a, b){
        return {expr: ['lam', 'a', ['lam', 'b', ['var', (weak_normal_form(a).expr[1]|0) <= (weak_normal_form(b).expr[1]|0) ? 'a' : 'b']]], env: {}};
      }],
      env: {}
    },

    '==': {
      expr: ['int', '<=', 2, function(a, b){
        return {expr: ['lam', 'a', ['lam', 'b', ['var', (weak_normal_form(a).expr[1]|0) === (weak_normal_form(b).expr[1]|0) ? 'a' : 'b']]], env: {}};
      }],
      env: {}
    },

    /*
    'runIO': {
      expr: ['int', 'runIO', 1, function(ioAct){
        return {expr: ['app', ioAct.expr, null], env: ioAct.env};
      }],
      env: {}
    },
    */

    'show-int': {
      expr: ['int', 'show-int', 1, function(num){
        num = weak_normal_form(num);
        return weak_normal_form(js_str_to_lc_str(''+num.expr[1]));
      }],
      env: {}
    },

    'read-int': {
      expr: ['int', 'read-int', 1, function(str){
        return {expr: ['var', lc_str_to_js_str(str) | 0], env: {}};
      }],
      env: {}
    },

    'putChar': {
      expr: ['int', 'putChar', 2, function(ch, s){
        process.stdout.write(lc_char_to_js_char(weak_normal_form(ch).expr[1]));
        return {expr: ['lam', 'p', ['app', ['app', ['var', 'p'], s], ['lam', 'a', ['var', 'a']]]], env: {}};
      }],
      env: {}
    },

    'getLine': {
      expr: ['int', 'getLine', 1, function(s, cb){
        var go = function(){
          var take, match;
          if( match = input_buffer.match(/(^[^\n]*)\n((?:a|[^a])*)$/) ){
            take = match[1];
            input_buffer = match[2];
          }
          else if( input_eof ){
            if( input_buffer.length ){
              take = input_buffer;
              input_buffer = '';
            }
            else
              throw 'getLine: eof';
          }
          else{
            input_wait = function(){
              var res = go();
              if( res ){
                input_wait = null;
                cb(res);
              }
            };
            return;
          }
          var res = js_str_to_lc_str(take);
          return {expr: ['lam', 'p', ['app', ['app', ['var', 'p'], s], res.expr]], env: res.env};
        };

        return go();
      }],
      env: {}
    },
  };

  var cont = function(res){
    res = weak_normal_form(res, cont);
    if( res )
      done(res);
  };
  return weak_normal_form({expr: ['app', expr, null], env: env}, cont);
}

if( process.argv.length > 2 ){
  fs.readFile(process.argv[2], 'utf8', function(err, code){
    if( err ){
      console.log('read stdin failed: ' + err);
      process.exit(1);
    }

    process.stdin.on('data', function(chunk){
      input_buffer += chunk;
      if( input_wait )
        input_wait();
    });
    process.stdin.on('end', function(){
      input_eof = true;
      if( input_wait )
        input_wait();
    });

    if( run(JSON.parse(code), function(){ process.exit() }) )
      process.exit();
  });
}
else{
  var code = '';
  process.stdin.on('data', function(chunk){
    code += chunk;
  });
  process.stdin.on('end', function(){
    //console.log(JSON.stringify(run(JSON.parse(code)).expr));
    if( run(JSON.parse(code), function(){ process.exit() }) )
      process.exit();
  });
}
