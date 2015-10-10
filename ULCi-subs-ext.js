var util = require('./ULC-util.js');
var pretty = util.pretty;

var last_fresh_symbol = 0;
function fresh_symbol(){
  ++last_fresh_symbol;
  return '_' + last_fresh_symbol;
}

function clone_array(array){
  var out = new Array(array.length), i;
  for(i=0; i<array.length; ++i)
    out[i] = array[i];
  return out;
}

function scan(expr, symbol){
  var i;
  switch(expr[0]){
    case 'var':
      return expr[1]===symbol;

    case 'lam':
      if( expr[1]===symbol )
        return false;
      else
        return scan(expr[2], symbol);

    case 'app':
      return scan(expr[1], symbol) || scan(expr[2], symbol);

    case 'int':
      return false;
  }
}
function _subs(expr, symbol, value){
  var i, out, new_symbol;
  switch(expr[0]){
    case 'var':
      if( expr[1]===symbol )
        return value;
      else
        return expr;

    case 'lam':
      if( expr[1]===symbol )
        return expr;
      else{
        if( scan(value, expr[1]) ){
          new_symbol = fresh_symbol();
          expr[2] = subs(expr[2], expr[1], ['var', new_symbol]);
          expr[1] = new_symbol;
        }
        return ['lam', expr[1], subs(expr[2], symbol, value)];
      }

    case 'app':
      return ['app', subs(expr[1], symbol, value), subs(expr[2], symbol, value)];

    case 'int':
      out = clone_array(expr);
      for(i=4; i<out.length; ++i)
        out[i] = subs(expr[i], symbol, value);
      return out;
  }
}
function subs(expr, symbol, value){
  return _subs(expr, symbol, value);

  console.log('');
  console.log('subs:');
  console.log('  ' + pretty(expr));
  console.log('  [' + symbol + ']');
  console.log('  ' + pretty(value));
  var new_expr = _subs(expr, symbol, value);
  console.log('  ==');
  console.log('  ' + pretty(expr));
  console.log('  --');
  console.log('  ' + pretty(new_expr));
  return new_expr;
}

function _weak_normal_form(expr){
  var lam, out, new_let, need, new_lam;
  switch(expr[0]){
    case 'var':
      return expr;

    case 'lam':
      return expr;

    case 'app':
      lam = weak_normal_form(expr[1]);
      if( lam[0]==='lam' )
        return weak_normal_form(subs(lam[2], lam[1], expr[2]));
      else if( lam[0]==='int' ){
        new_lam = clone_array(lam);
        --new_lam[2];
        new_lam.push(expr[2]);
        return weak_normal_form(new_lam);
      }
      else
        return ['app', lam, expr[2]];

    case 'int':
      if(expr[2]===0)
        return weak_normal_form(expr[3].apply(null, expr.slice(4)));
      else
        return expr;
  }
}
function weak_normal_form(expr){
  return _weak_normal_form(expr);

  console.log('');
  console.log('weak_normal_form:');
  console.log('  ' + pretty(expr));
  return _weak_normal_form(expr);
}

function _normal_form(expr){
  var lam, out, new_let, need, new_lam, i;
  switch(expr[0]){
    case 'var':
      return expr;

    case 'lam':
      return ['lam', expr[1], normal_form(expr[2])];

    case 'app':
      lam = weak_normal_form(expr[1]);
      if( lam[0]==='lam' )
        return normal_form(subs(lam[2], lam[1], expr[2]));
      else if( lam[0]==='int' ){
        new_lam = clone_array(lam);
        --new_lam[2];
        new_lam.push(expr[2]);
        return normal_form(new_lam);
      }
      else
        return ['app', normal_form(lam), normal_form(expr[2])];

    case 'int':
      if(expr[2]===0)
        return normal_form(expr[3].apply(null, expr.slice(4)));
      else
        return expr;
  }
}
function normal_form(expr){
  return _normal_form(expr);

  console.log('');
  console.log('normal_form:');
  console.log('  ' + pretty(expr));
  return _normal_form(expr);
}

function run(expr){
  var i;
  var ints = {
    '+':
      ['int', '+', 2, function(a, b){
        return ['var', (normal_form(a)[1]|0) + (normal_form(b)[1]|0)];
      }],

    '-':
      ['int', '-', 2, function(a, b){
        return ['var', (normal_form(a)[1]|0) - (normal_form(b)[1]|0)];
      }],

    '*':
      ['int', '*', 2, function(a, b){
        return ['var', (normal_form(a)[1]|0) * (normal_form(b)[1]|0)];
      }],

    'div': {
      expr: ['int', 'div', 2, function(a, b){
        return {expr: ['var', (normal_form(a).expr[1]|0) / (normal_form(b).expr[1]|0) | 0], env: this};
      }],
      env: {}
    },

    'mod': {
      expr: ['int', 'mod', 2, function(a, b){
        return {expr: ['var', (normal_form(a).expr[1]|0) % (normal_form(b).expr[1]|0)], env: this};
      }],
      env: {}
    },

    '<=':
      ['int', '<=', 2, function(a, b){
        return ['lam', 'a', ['lam', 'b', ['var', (normal_form(a)[1]|0) <= (normal_form(b)[1]|0) ? 'a' : 'b']]];
      }],
  };
  for(i in ints){
    //console.log(i);
    //console.log(ints[i]);
    expr = subs(expr, i, clone_array(ints[i]));
  }
  //console.log(pretty(expr));
  return normal_form(expr);
}

var code = '';
process.stdin.on('data', function(chunk){
  code += chunk;
});
process.stdin.on('end', function(){
  console.log(JSON.stringify(run(JSON.parse(code))));
  process.exit();
});
