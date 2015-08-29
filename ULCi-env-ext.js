var util = require('./ULC-util.js');
var pretty = util.pretty;
var pretty_closure = util.pretty_closure;

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

function weak_normal_form(closure){
  //console.log('weak_normal_form');
  //console.log(pretty_closure(closure, 1));
  var lam, env2, new_lam;
  switch(closure.expr[0]){
    case 'var':
      if( closure.env[closure.expr[1]] )
        return weak_normal_form(closure.env[closure.expr[1]]);
      else
        return closure;

    case 'lam':
      return closure;

    case 'app':
      lam = weak_normal_form({expr: closure.expr[1], env: closure.env});
      if( lam.expr[0]==='int' ){
        new_lam = {
          expr: clone_array(lam.expr),
          env: lam.env
        };
        --new_lam.expr[2];
        new_lam.expr.push({expr: closure.expr[2], env: closure.env});
        return weak_normal_form(new_lam);
      }
      if( lam.expr[0]!='lam' )
        return {expr: ['app', lam.expr, normal_form({expr: closure.expr[2], env: closure.env}).expr], env: closure.env};
      env2 = clone_env(lam.env);
      env2[lam.expr[1]] = {expr: closure.expr[2], env: closure.env};
      return weak_normal_form({expr: lam.expr[2], env: env2});

    case 'int':
      if( closure.expr[2]===0 )
        return weak_normal_form(closure.expr[3].apply(closure.env, closure.expr.slice(4)));
      else
        return closure;
  }
}

function normal_form(closure){
  //console.log('normal_form');
  //console.log(pretty_closure(closure, 1));
  var lam, env2, new_lam;
  switch(closure.expr[0]){
    case 'var':
      if( closure.env[closure.expr[1]] )
        return normal_form(closure.env[closure.expr[1]]);
      else
        return closure;

    case 'lam':
      env2 = clone_env(closure.env);
      env2[closure.expr[1]] = false;
      return {expr: ['lam', closure.expr[1], normal_form({expr: closure.expr[2], env: env2}).expr], env: closure.env};

    case 'app':
      lam = weak_normal_form({expr: closure.expr[1], env: closure.env});
      if( lam.expr[0]==='int' ){
        new_lam = {
          expr: clone_array(lam.expr),
          env: lam.env
        };
        --new_lam.expr[2];
        new_lam.expr.push({expr: closure.expr[2], env: closure.env});
        return normal_form(new_lam);
      }
      if( lam.expr[0]!='lam' )
        return {expr: ['app', lam.expr, normal_form({expr: closure.expr[2], env: closure.env}).expr], env: closure.env};
      env2 = clone_env(lam.env);
      env2[lam.expr[1]] = {expr: closure.expr[2], env: closure.env};
      return normal_form({expr: lam.expr[2], env: env2});

    case 'int':
      if( closure.expr[2]===0 )
        return normal_form(closure.expr[3].apply(closure.env, closure.expr.slice(4)));
      else
        return closure;
  }
}

function run(expr){
  var i;
  var env = {
    '+': {
      expr: ['int', '+', 2, function(a, b){
        return {expr: ['var', (normal_form(a).expr[1]|0) + (normal_form(b).expr[1]|0)], env: this};
      }],
      env: {}
    },

    '-': {
      expr: ['int', '-', 2, function(a, b){
        return {expr: ['var', (normal_form(a).expr[1]|0) - (normal_form(b).expr[1]|0)], env: this};
      }],
      env: {}
    },

    '*': {
      expr: ['int', '*', 2, function(a, b){
        return {expr: ['var', (normal_form(a).expr[1]|0) * (normal_form(b).expr[1]|0)], env: this};
      }],
      env: {}
    },

    '<=': {
      expr: ['int', '<=', 2, function(a, b){
        return {expr: ['lam', 'a', ['lam', 'b', ['var', (normal_form(a).expr[1]|0) <= (normal_form(b).expr[1]|0) ? 'a' : 'b']]], env: this};
      }],
      env: {}
    },
  };
  return normal_form({expr: expr, env: env});
}

var code = '';
process.stdin.on('data', function(chunk){
  code += chunk;
});
process.stdin.on('end', function(){
  console.log(JSON.stringify(run(JSON.parse(code)).expr));
  process.exit();
});
