var util = require('./ULC-util.js');
var pretty = util.pretty;
var pretty_closure = util.pretty_closure;

function clone_env(env){
  var ctor = new Function;
  ctor.prototype = env;
  return new ctor;
}

function weak_normal_form(closure){
  //console.log('weak_normal_form');
  //console.log(pretty_closure(closure, 1));
  var lam, env2;
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
      if( lam.expr[0]!='lam' )
        return {expr: ['app', lam.expr, normal_form({expr: closure.expr[2], env: closure.env}).expr], env: closure.env};
      env2 = clone_env(lam.env);
      env2[lam.expr[1]] = {expr: closure.expr[2], env: closure.env};
      return weak_normal_form({expr: lam.expr[2], env: env2});
  }
}

function normal_form(closure){
  //console.log('normal_form');
  //console.log(pretty_closure(closure, 1));
  var lam, env2;
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
      if( lam.expr[0]!='lam' )
        return {expr: ['app', lam.expr, normal_form({expr: closure.expr[2], env: closure.env}).expr], env: closure.env};
      env2 = clone_env(lam.env);
      env2[lam.expr[1]] = {expr: closure.expr[2], env: closure.env};
      return normal_form({expr: lam.expr[2], env: env2});
  }
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

    //['app', ['app', ['var', '+'], ['var', '2']], ['var', '1']]
    //['app', ['var', 'head'], ['app', ['var', 'tail'], ['var', 'list-0-1-']]]
    //['app', ['app', ['var', 'take'], ['var', '1']], ['var', 'list-0-1-']]
    //['app', ['app', ['var', 'take'], ['var', '1']], ['app', ['app', ['var', 'cons'], ['var', '1']], ['var', 'nil']]]

    //['app', ['app', ['var', 'take'], ['var', '3']], ['app', ['app', ['var', 'drop'], ['var', '2']], ['var', 'list-0-1-']]] // take 3 (drop 2 [0,1,2..])
    ['app', ['app', ['var', 'take'], ['var', '6']], ['var', 'fibs']] // take 6 fibs
    //['app', ['app', ['var', 'take'], ['app', ['var', '*2'], ['app', ['var', '*2'], ['var', '6']]]], ['app', ['app', ['var', 'map'], ['var', '*2']], ['var', 'list-0-1-']]] // take 6 (map (*2) [0,1,2..])

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

//console.log(pretty(prog, 0));
//console.log(pretty(normal_form({expr: prog, env: {}}).expr, 0));

var code = '';
process.stdin.on('data', function(chunk){
  code += chunk;
});
process.stdin.on('end', function(){
  console.log(JSON.stringify(normal_form({expr: JSON.parse(code), env: {}}).expr));
  process.exit();
});
