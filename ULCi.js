function str_times(str, n){
  var out = '', i;
  for(i=0; i<n; ++i)
    out += str;
  return out;
}

function pretty(expr, indent){
  var out = '', i;
  switch(expr[0]){
    case 'lam':
      return '\\' + expr[1] + ' ' + pretty(expr[2], indent);

    case 'var':
      return expr[1];

    case 'app':
      return (expr[1][0]==='var' ? '' : '(') + pretty(expr[1], indent) + (expr[1][0]==='var' ? '' : ')') + ' ' + (expr[2][0]==='var' ? '' : '(') + pretty(expr[2], indent) + (expr[2][0]==='var' ? '' : ')');
  }
}

var last_fresh_symbol = 0;
function fresh_symbol(){
  ++last_fresh_symbol;
  return '_' + last_fresh_symbol;
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
  }
}
function subs(expr, symbol, value){
  return _subs(expr, symbol, value);

  console.log('');
  console.log('subs:');
  console.log('  ' + pretty(expr, 1));
  console.log('  [' + symbol + ']');
  console.log('  ' + pretty(value, 1));
  var new_expr = _subs(expr, symbol, value);
  console.log('  ==');
  console.log('  ' + pretty(expr, 1));
  console.log('  --');
  console.log('  ' + pretty(new_expr, 1));
  return new_expr;
}

function _weak_normal_form(expr){
  var lam, out, new_let, need;
  switch(expr[0]){
    case 'var':
      return expr;

    case 'lam':
      return expr;

    case 'app':
      lam = weak_normal_form(expr[1]);
      if( lam[0]==='lam' )
        return weak_normal_form(subs(lam[2], lam[1], expr[2]));
      else
        return ['app', lam, expr[2]];
  }
}
function weak_normal_form(expr){
  return _weak_normal_form(expr);

  console.log('');
  console.log('weak_normal_form:');
  console.log('  ' + pretty(expr, 1));
  return _weak_normal_form(expr);
}

function _normal_form(expr){
  var lam, out, new_let, need;
  switch(expr[0]){
    case 'var':
      return expr;

    case 'lam':
      return ['lam', expr[1], normal_form(expr[2])];

    case 'app':
      lam = weak_normal_form(expr[1]);
      if( lam[0]==='lam' )
        return normal_form(subs(lam[2], lam[1], expr[2]));
      else
        return ['app', normal_form(lam), normal_form(expr[2])];
  }
}
function normal_form(expr){
  return _normal_form(expr);

  console.log('');
  console.log('normal_form:');
  console.log('  ' + pretty(expr, 1));
  return _normal_form(expr);
}

var prog =
  ['app', ['lam', 'true',
  ['app', ['lam', 'false',
  ['app', ['lam', 'and',
  ['app', ['lam', 'or',
  ['app', ['lam', 'not',
  ['app', ['lam', 'if',

  ['app', ['lam', 'fix',
  ['app', ['lam', 'bottom',

  ['app', ['lam', '0',
  ['app', ['lam', '+1',
  ['app', ['lam', '-1',
  ['app', ['lam', '+',
  ['app', ['lam', '-',
  ['app', ['lam', '*2',
  ['app', ['lam', '1',
  ['app', ['lam', '2',
  ['app', ['lam', '3',
  ['app', ['lam', '4',
  ['app', ['lam', '5',
  ['app', ['lam', '6',

  ['app', ['lam', 'nil',
  ['app', ['lam', 'cons',
  ['app', ['lam', 'head',
  ['app', ['lam', 'tail',
  ['app', ['lam', 'take',
  ['app', ['lam', 'drop',
  ['app', ['lam', 'map',
  ['app', ['lam', 'zip-with',

  ['app', ['lam', 'list-0-1-', // [0,1,2,3,...]
  ['app', ['lam', 'fibs', // [1,1,2,3,5,8,...]

    // ['app', ['app', ['var', 'take'], ['var', '3']], ['app', ['app', ['var', 'drop'], ['var', '2']], ['var', 'list-0-1-']]] // take 3 (drop 2 [0,1,2..])
    // ['app', ['app', ['var', 'take'], ['var', '6']], ['var', 'fibs']] // take 6 fibs
    ['app', ['app', ['var', 'take'], ['app', ['var', '*2'], ['app', ['var', '*2'], ['var', '6']]]], ['app', ['app', ['var', 'map'], ['var', '*2']], ['var', 'list-0-1-']]] // take 6 (map (*2) [0,1,2..])

  ], ['app', ['var', 'fix'], ['lam', 'unfix-fibs', ['app', ['lam', 'fibs', ['app', ['app', ['var', 'cons'], ['var', '1']], ['app', ['app', ['var', 'cons'], ['var', '1']], ['app', ['app', ['app', ['var', 'zip-with'], ['var', '+']], ['var', 'fibs']], ['app', ['var', 'tail'], ['var', 'fibs']]]]]], ['app', ['var', 'unfix-fibs'], ['var', 'unfix-fibs']]]]]] // fibs
  ], ['app', ['app', ['var', 'fix'], ['lam', 'gen', ['lam', 'i', ['app', ['app', ['var', 'cons'], ['var', 'i']], ['app', ['app', ['var', 'gen'], ['var', 'gen']], ['app', ['var', '+1'], ['var', 'i']]]]]]], ['var', '0']]], // list-0-1-

  ], ['lam', 'f', ['app', ['var', 'fix'], ['lam', 'zip-with', ['lam', 'as', ['lam', 'bs', ['app', ['app', ['var', 'as'], ['lam', 'a', ['lam', 'as-', ['app', ['app', ['var', 'bs'], ['lam', 'b', ['lam', 'bs-', ['app', ['app', ['var', 'cons'], ['app', ['app', ['var', 'f'], ['var', 'a']], ['var', 'b']]], ['app', ['app', ['app', ['var', 'zip-with'], ['var', 'zip-with']], ['var', 'as-']], ['var', 'bs-']]]]]], ['var', 'nil']]]]], ['var', 'nil']]]]]]]] // zip-with
  ], ['lam', 'f', ['app', ['var', 'fix'], ['lam', 'unfix-map', ['lam', 'ls', ['app', ['app', ['var', 'ls'], ['lam', 'a', ['lam', 'as', ['app', ['app', ['var', 'cons'], ['app', ['var', 'f'], ['var', 'a']]], ['app', ['app', ['var', 'unfix-map'], ['var', 'unfix-map']], ['var', 'as']]]]]], ['var', 'nil']]]]]]] // map
  ], ['app', ['var', 'fix'], ['lam', 'drop', ['lam', 'n', ['lam', 'ls', ['app', ['app', ['var', 'n'], ['lam', 'n-', ['app', ['app', ['var', 'ls'], ['lam', 'a', ['lam', 'as', ['app', ['app', ['app', ['var', 'drop'], ['var', 'drop']], ['var', 'n-']], ['var', 'as']]]]], ['var', 'nil']]]], ['var', 'ls']]]]]]] // drop
  ], ['app', ['var', 'fix'], ['lam', 'take', ['lam', 'n', ['lam', 'ls', ['app', ['app', ['var', 'n'], ['lam', 'n-', ['app', ['app', ['var', 'ls'], ['lam', 'a', ['lam', 'as', ['app', ['app', ['var', 'cons'], ['var', 'a']], ['app', ['app', ['app', ['var', 'take'], ['var', 'take']], ['var', 'n-']], ['var', 'as']]]]]], ['var', 'nil']]]], ['var', 'nil']]]]]]] // take
  ], ['lam', 'ls', ['app', ['app', ['var', 'ls'], ['lam', 'a', ['lam', 'as', ['var', 'as']]]], ['var', 'bottom']]]] // tail
  ], ['lam', 'ls', ['app', ['app', ['var', 'ls'], ['lam', 'a', ['lam', 'as', ['var', 'a']]]], ['var', 'bottom']]]] // head
  ], ['lam', 'a', ['lam', 'as', ['lam', 'when-cons', ['lam', 'when-nil', ['app', ['app', ['var', 'when-cons'], ['var', 'a']], ['var', 'as']]]]]]] // cons
  ], ['lam', 'when-cons', ['lam', 'when-nil', ['var', 'when-nil']]]] // nil
  ], ['app', ['var', '+1'], ['var', '5']]] // 6
  ], ['app', ['var', '+1'], ['var', '4']]] // 5
  ], ['app', ['var', '+1'], ['var', '3']]] // 4
  ], ['app', ['var', '+1'], ['var', '2']]] // 3
  ], ['app', ['var', '+1'], ['var', '1']]] // 2
  ], ['app', ['var', '+1'], ['var', '0']]] // 1
  ], ['app', ['var', 'fix'], ['lam', '*2', ['lam', 'n', ['app', ['app', ['var', 'n'], ['lam', 'n-', ['app', ['var', '+1'], ['app', ['var', '+1'], ['app', ['app', ['var', '*2'], ['var', '*2']], ['var', 'n-']]]]]], ['var', '0']]]]]], // *2
  ], ['app', ['var', 'fix'], ['lam', '-', ['lam', 'a', ['lam', 'b', ['app', ['app', ['var', 'b'], ['lam', 'b-', ['app', ['app', ['var', 'a'], ['lam', 'a-', ['app', ['app', ['app', ['var', '-'], ['var', '-']], ['var', 'a-']], ['var', 'b-']]]], ['var', 'bottom']]]], ['var', 'a']]]]]]], // -
  ], ['app', ['var', 'fix'], ['lam', '+', ['lam', 'a', ['lam', 'b', ['app', ['app', ['var', 'a'], ['lam', 'a-', ['app', ['var', '+1'], ['app', ['app', ['app', ['var', '+'], ['var', '+']], ['var', 'a-']], ['var', 'b']]]]], ['var', 'b']]]]]]] // +
  ], ['lam', 'n', ['app', ['app', ['var', 'n'], ['lam', 'n-', ['var', 'n-']]], ['var', 'bottom']]]] // -1
  ], ['lam', 'n', ['lam', 's', ['lam', 'z', ['app', ['var', 's'], ['var', 'n']]]]]] // +1
  ], ['lam', 's', ['lam', 'z', ['var', 'z']]]] // 0

  ], ['app', ['var', 'fix'], ['var', 'fix']]] // bottom
  ], ['lam', 'f', ['app', ['var', 'f'], ['var', 'f']]]] // fix

  ], ['lam', 'cond', ['var', 'cond']]] // if
  ], ['lam', 'a', ['app', ['app', ['var', 'a'], ['var', 'false']], ['var', 'true']]]] // not
  ], ['lam', 'a', ['lam', 'b', ['app', ['app', ['var', 'a'], ['var', 'true']], ['var', 'b']]]]] // or
  ], ['lam', 'a', ['lam', 'b', ['app', ['app', ['var', 'a'], ['var', 'b']], ['var', 'false']]]]] // and
  ], ['lam', 'a', ['lam', 'b', ['var', 'b']]]] // false
  ], ['lam', 'a', ['lam', 'b', ['var', 'a']]]] // true
  ;

console.log(pretty(prog, 0));
console.log(pretty(normal_form(prog, {}), 0));
