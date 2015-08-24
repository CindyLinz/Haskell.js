/*
 * let
 *   0 = \f \x x
 *   1 = \f \x f x
 *   2 = \f \x f (f x)
 * in
 *   let
 *     +1 = \n \f \x f (n f x)
 *     + = \m \n \f \x m f (n f x)
 *   in
 *     + 2 (+1 2)
 *
 * ['let'
 * , '0', ['lam', 'f', ['lam', 'x', ['var', 'x']]]
 * , '1', ['lam', 'f', ['lam', 'x', ['app', ['var', 'f'], ['var', 'x']]]]
 * , '2', ['lam', 'f', ['lam', 'x', ['app', ['var', 'f'], ['app', ['var', 'f'], ['var', 'x']]]]]
 * , ['let'
 *   , '+1', ['lam', 'n', ['lam', 'f', ['lam', 'x', ['app', ['var', 'f'], ['app', ['app', ['var', 'n'], ['var', 'f']], ['var', 'x']]]]]]
 *   , '+', ['lam', 'm', ['lam', 'n', ['lam', 'f', ['lam', 'x', ['app', ['app', ['var', 'm'], ['var', 'f']], ['app', ['app', ['var', 'n'], ['var', 'f']], ['var', 'x']]]]]]]
 *   , ['app', ['app', ['var', '+'], ['var', '2']], ['app', ['var', '+1'], ['var', '2']]]
 *   ]
 * ]
 */

function str_times(str, n){
  var out = '', i;
  for(i=0; i<n; ++i)
    out += str;
  return out;
}

function pretty(expr, indent, in_in){
  var out = '', i;
  switch(expr[0]){
    case 'let':
      out += (in_in ? '  ' : '\n  ' + str_times('  ', indent)) + 'let';
      for(i=1; i+1<expr.length; i+=2)
        out += '\n    ' + str_times('  ', indent) + expr[i] + ' = ' + pretty(expr[i+1], indent+2, false);
      out += '\n  ' + str_times('  ', indent) + 'in\n    ' + str_times('  ', indent) + pretty(expr[expr.length-1], indent+2, true);
      return out;

    case 'lam':
      return '\\' + expr[1] + '. ' + pretty(expr[2], indent, false);

    case 'var':
      return expr[1];

    case 'app':
      return (expr[1][0]==='var' ? '' : '(') + pretty(expr[1], indent, false) + (expr[1][0]==='var' ? '' : ')') + ' ' + (expr[2][0]==='var' ? '' : '(') + pretty(expr[2], indent, false) + (expr[2][0]==='var' ? '' : ')');
  }
}

// function pretty_env(env, indent){
//   var out = '{', i;
//   for(i in env)
//     out += '\n  ' + str_times('  ', indent) + i + ': ' + (env[i].expr ? pretty(env[i].expr, indent+2, false) : '<hole>');
//   out += '\n' + str_times('  ', indent) + '}';
//   return out;
// }

// function clone_env(env){
//   var ctor = new Function;
//   ctor.prototype = env;
//   return new ctor;
// }
// 
// function head_normal_form(expr, env){
//   var lam, env2, i;
//   console.log(pretty_env(env, 0));
//   switch(expr[0]){
//     case 'var':
//       if( env[expr[1]] && env[expr[1]].expr )
//         return head_normal_form(env[expr[1]].expr, env[expr[1]].env);
//       else
//         return expr;
// 
//     case 'lam':
//       return expr;
// 
//     case 'app':
//       lam = head_normal_form(expr[1], env);
//       if( lam[0]==='lam' ){
//         env2 = clone_env(env);
//         env2[lam[1]] = {expr: expr[2], env: env};
//         console.log(pretty_env(env2, 0));
//         return head_normal_form(lam[2], env2);
//       }
//       else{
//         return expr;
//       }
// 
//     case 'let':
//       env2 = clone_env(env);
//       for(i=1; i+1<expr.length; i+=2)
//         env2[expr[i]] = {expr: expr[i+1], env: env2};
//       return head_normal_form(expr[expr.length-1], env2);
//   }
// }
// 
// function normal_form(expr, env){
//   var lam, env2, i;
//   console.log(expr[0]);
//   console.log(pretty_env(env, 0));
//   switch(expr[0]){
//     case 'var':
//       if( env[expr[1]] && env[expr[1]].expr )
//         return normal_form(env[expr[1]].expr, env[expr[1]].env);
//       else
//         return expr;
// 
//     case 'lam':
//       env2 = clone_env(env);
//       env2[expr[1]] = {};
//       return ['lam', expr[1], normal_form(expr[2], env2)];
// 
//     case 'app':
//       lam = head_normal_form(expr[1], env);
//       if( lam[0]==='lam' ){
//         env2 = clone_env(env);
//         env2[lam[1]] = {expr: expr[2], env: env};
//         console.log(pretty_env(env2, 0));
//         return normal_form(lam[2], env2);
//       }
//       else{
//         return ['app', expr[1], normal_form(expr[2], env)];
//       }
// 
//     case 'let':
//       env2 = clone_env(env);
//       for(i=1; i+1<expr.length; i+=2)
//         env2[expr[i]] = {expr: expr[i+1], env: env2};
//       console.log(pretty_env(env2, 0));
//       return normal_form(expr[expr.length-1], env2);
//   }
// }

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

    case 'let':
      for(i=1; i+1<expr.length; i+=2)
        if( expr[i]===symbol )
          return false;

      for(i=1; i+1<expr.length; i+=2)
        if( scan(expr[i+1], symbol) )
          return true;
      return scan(expr[expr.length-1], symbol);
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

    case 'let':
      for(i=1; i+1<expr.length; i+=2)
        if( expr[i]===symbol )
          return expr;

      out = ['let'];
      for(i=1; i+1<expr.length; i+=2){
        out[i] = expr[i];
        out[i+1] = subs(expr[i+1], symbol, value);
      }
      out[expr.length-1] = subs(expr[expr.length-1], symbol, value);
      return out;
  }
}
function subs(expr, symbol, value){
  return _subs(expr, symbol, value);

  console.log('');
  console.log('subs:');
  console.log('  ' + pretty(expr, 1, true));
  console.log('  [' + symbol + ']');
  console.log('  ' + pretty(value, 1, true));
  var new_expr = _subs(expr, symbol, value);
  console.log('  ==');
  console.log('  ' + pretty(expr, 1, true));
  console.log('  --');
  console.log('  ' + pretty(new_expr, 1, true));
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

    case 'let':
      if( expr.length===2 ){
        return weak_normal_form(expr[1]);
      }
      else{
        need = false;
        for(i=1; i+1<expr.length; i+=2){
          if( scan(expr[2], expr[i]) ){
            need = true;
            break;
          }
        }
        if( need ){
          new_let = [];
          for(i=0; i+1<expr.length; ++i)
            new_let[i] = expr[i];
          new_let[expr.length-1] = expr[2];
        }
        else{
          new_let = expr[2];
        }

        out = ['let'];
        for(i=3; i+1<expr.length; i+=2){
          out[i-2] = expr[i];
          out[i-1] = subs(expr[i+1], expr[1], new_let);
        }
        out[expr.length-3] = subs(expr[expr.length-1], expr[1], new_let);
        return weak_normal_form(out);
      }
  }
}
function weak_normal_form(expr){
  //return _weak_normal_form(expr);

  console.log('');
  console.log('weak_normal_form:');
  console.log('  ' + pretty(expr, 1, true));
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

    case 'let':
      if( expr.length===2 ){
        return normal_form(expr[1]);
      }
      else{
        need = false;
        for(i=1; i+1<expr.length; i+=2){
          if( scan(expr[2], expr[i]) ){
            need = true;
            break;
          }
        }
        if( need ){
          new_let = [];
          for(i=0; i+1<expr.length; ++i)
            new_let[i] = expr[i];
          new_let[expr.length-1] = expr[2];
        }
        else{
          new_let = expr[2];
        }

        out = ['let'];
        for(i=3; i+1<expr.length; i+=2){
          out[i-2] = expr[i];
          out[i-1] = subs(expr[i+1], expr[1], new_let);
        }
        out[expr.length-3] = subs(expr[expr.length-1], expr[1], new_let);
        return normal_form(out);
      }
  }
}
function normal_form(expr){
  //return _normal_form(expr);

  console.log('');
  console.log('normal_form:');
  console.log('  ' + pretty(expr, 1, true));
  return _normal_form(expr);
}

// function normal_form(closure){
//   var res, i, name, expr, env;
//   while(true){
//     console.log('normal_form:\n  ' + pretty(closure.expr, 1, true) + '\n  ' + pretty_env(closure.env, 1));
//     switch(closure.expr[0]){
//       case 'var':
//         name = closure.expr[1];
//         if( closure.env[name] ){
//           closure = closure.env[name];
//           break;
//         }
//         else{
//           return closure;
//         }
// 
//       case 'lam':
//         env = clone_env(closure.env);
//         env[closure.expr[1]] = null;
//         res = normal_form({expr: closure.expr[2], env: env});
//         return {expr: ['lam', closure.expr[1], res.expr], env: closure.env};
// 
//       case 'app':
//         res = normal_form({expr: closure.expr[1], env: closure.env});
//         if( res.expr[0]==='lam' ){
//           env = clone_env(closure.env);
//           env[res.expr[1]] = {expr: closure.expr[2], env: closure.env};
//           closure = {expr: res.expr
//           expr = res.expr[2];
//           env = env2;
//           break;
//         }
//         else{
//           return {expr: expr, env: env};
//         }
// 
//       case 'let':
//         env = clone_env(closure.env);
//         for(i=1; i+1<closure.expr.length; i+=2)
//           env[expr[i]] = {expr: expr[i+1], env: env};
//         closure = {expr: expr[expr.length-1], env: env};
//         break;
//     }
//   }
// }

var prog =
  [ 'let'

  , 'true', ['lam', 'a', ['lam', 'b', ['var', 'a']]]
  , 'false', ['lam', 'a', ['lam', 'b', ['var', 'b']]]
  , 'if', ['lam', 'cond', ['var', 'cond']]
  , 'not', ['lam', 'bool', ['lam', 'a', ['lam', 'b', ['app', ['app', ['var', 'bool'], ['var', 'b']], ['var', 'a']]]]]
  , 'and', ['lam', 'a', ['lam', 'b', ['app', ['app', ['var', 'a'], ['var', 'b']], ['var', 'false']]]]
  , 'or', ['lam', 'a', ['lam', 'b', ['app', ['app', ['var', 'a'], ['var', 'true']], ['var', 'b']]]]

  , '0', ['lam', 's', ['lam', 'z', ['var', 'z']]]
  , '+1', ['lam', 'n', ['lam', 's', ['lam', 'z', ['app', ['var', 's'], ['var', 'n']]]]]
  , '-1', ['lam', 'n', ['app', ['app', ['var', 'n'], ['lam', 'n-', ['var', 'n-']]], ['var', 'bottom']]]

  , '1', ['app', ['var', '+1'], ['var', '0']]
  , '2', ['app', ['var', '+1'], ['var', '1']]
  , '3', ['app', ['var', '+1'], ['var', '2']]
  , '4', ['app', ['var', '+1'], ['var', '3']]

  , '<=', ['lam', 'a', ['lam', 'b', ['app', ['app', ['var', 'a'], ['lam', 'a-', ['app', ['app', ['var', 'b'], ['lam', 'b-', ['app', ['app', ['var', '<='], ['var', 'a-']], ['var', 'b-']]]], ['var', 'false']]]], ['var', 'true']]]]

  , 'bottom', ['var', 'bottom']

  , '+', ['lam', 'a', ['lam', 'b', ['app', ['app', ['var', 'a'], ['lam', 'n', ['app', ['var', '+1'], ['app', ['app', ['var', '+'], ['var', 'n']], ['var', 'b']]]]], ['var', 'b']]]]

  , 'cons', ['lam', 'a', ['lam', 'as', ['lam', 'when-cons', ['lam', 'when-nil', ['app', ['app', ['var', 'when-cons'], ['var', 'a']], ['var', 'as']]]]]]
  , 'nil', ['lam', 'when-cons', ['lam', 'when-nil', ['var', 'when-nil']]]

  , 'head', ['lam', 'ls', ['app', ['app', ['var', 'ls'], ['lam', 'a', ['lam', 'as', ['var', 'a']]]], ['var', 'bottom']]]
  , 'tail', ['lam', 'ls', ['app', ['app', ['var', 'ls'], ['lam', 'a', ['lam', 'as', ['var', 'as']]]], ['var', 'bottom']]]
  , 'drop', ['lam', 'n', ['lam', 'ls', ['app', ['app', ['var', 'n'], ['lam', 'n-', ['app', ['app', ['var', 'ls'], ['lam', 'a', ['lam', 'as', ['app', ['app', ['var', 'drop'], ['var', 'n-']], ['var', 'as']]]]], ['var', 'nil']]]], ['var', 'ls']]]]
  , 'take', ['lam', 'n', ['lam', 'ls', ['app', ['app', ['var', 'n'], ['lam', 'n-', ['app', ['app', ['var', 'ls'], ['lam', 'a', ['lam', 'as', ['app', ['app', ['var', 'cons'], ['var', 'a']], ['app', ['app', ['var', 'take'], ['var', 'n-']], ['var', 'as']]]]]], ['var', 'nil']]]], ['var', 'nil']]]]
  , 'length', ['lam', 'ls', ['app', ['app', ['var', 'ls'], ['lam', 'a', ['lam', 'as', ['app', ['var', '+1'], ['app', ['var', 'length'], ['var', 'as']]]]]], ['var', '0']]]
  , 'length-acc', ['let', 'go', ['lam', 'n', ['lam', 'ls', ['app', ['app', ['var', 'ls'], ['lam', 'a', ['lam', 'as', ['app', ['app', ['var', 'go'], ['app', ['var', '+1'], ['var', 'n']]], ['var', 'as']]]]], ['var', 'n']]]], ['app', ['var', 'go'], ['var', '0']]]
  , 'null', ['lam', 'ls', ['app', ['app', ['var', 'ls'], ['lam', 'a', ['lam', 'as', ['var', 'false']]]], ['var', 'true']]]
  , 'zip-with', ['lam', 'f', ['let', 'go', ['lam', 'as', ['lam', 'bs', ['app', ['app', ['var', 'as'], ['lam', 'a', ['lam', 'as', ['app', ['app', ['var', 'bs'], ['lam', 'b', ['lam', 'bs', ['app', ['app', ['var', 'cons'], ['app', ['app', ['var', 'f'], ['var', 'a']], ['var', 'b']]], ['app', ['app', ['var', 'go'], ['var', 'as']], ['var', 'bs']]]]]], ['var', 'nil']]]]], ['var', 'nil']]]], ['var', 'go']]]

  //, '0', ['lam', 'f', ['lam', 'x', ['var', 'x']]]
  //, '1', ['lam', 'f', ['lam', 'x', ['app', ['var', 'f'], ['var', 'x']]]]
  //, '2', ['lam', 'f', ['lam', 'x', ['app', ['var', 'f'], ['app', ['var', 'f'], ['var', 'x']]]]]
  //, '3', ['app', ['var', '+1'], ['var', '2']]
  //, '4', ['app', ['var', '+1'], ['var', '3']]
  //, 'bottom', ['var', 'bottom']
  //, 'bottom', ['var', '0']

  //, '+1', ['lam', 'n', ['lam', 'f', ['lam', 'x', ['app', ['var', 'f'], ['app', ['app', ['var', 'n'], ['var', 'f']], ['var', 'x']]]]]]
  //, '+', ['lam', 'm', ['lam', 'n', ['lam', 'f', ['lam', 'x', ['app', ['app', ['var', 'm'], ['var', 'f']], ['app', ['app', ['var', 'n'], ['var', 'f']], ['var', 'x']]]]]]]

  //, 'cons', ['lam', 'a', ['lam', 'as', ['lam', 'extract', ['app', ['app', ['var', 'extract'], ['var', 'a']], ['var', 'as']]]]]
  //, 'nil', ['lam', 'extract', ['app', ['app', ['var', 'extract'], ['var', 'bottom']], ['var', 'bottom']]]
  //, 'head', ['lam', 'ls', ['app', ['var', 'ls'], ['lam', 'a', ['lam', 'as', ['var', 'a']]]]]
  //, 'tail', ['lam', 'ls', ['app', ['var', 'ls'], ['lam', 'a', ['lam', 'as', ['var', 'as']]]]]
  //, 'drop', ['lam', 'n', ['lam', 'ls', ['app', ['app', ['var', 'n'], ['var', 'tail']], ['var', 'ls']]]]
  //, 'reverse', ['lam', 'ls', [
  //, 'take', ['lam', 'n', ['lam', 'ls', [

  //, 'list-1-2-3', ['app', ['app', ['var', 'cons'], ['var', '1']], ['app', ['app', ['var', 'cons'], ['var', '2']], ['app', ['app', ['var', 'cons'], ['var', '3']], ['var', 'nil']]]]
  , 'list-1-2-3', ['app', ['app', ['var', 'cons'], ['var', '1']], ['app', ['app', ['var', 'cons'], ['var', '2']], ['app', ['app', ['var', 'cons'], ['var', '3']], ['var', 'nil']]]]
  , 'fib', ['app', ['app', ['var', 'cons'], ['var', '1']], ['app', ['app', ['var', 'cons'], ['var', '1']], ['app', ['app', ['app', ['var', 'zip-with'], ['var', '+']], ['var', 'fib']], ['app', ['var', 'tail'], ['var', 'fib']]]]]

  //, ['app', ['app', ['var', '+'], ['var', '2']], ['var', '1']] // 2 + 1
  //, ['app', ['app', ['var', '+'], ['var', '2']], ['app', ['var', '+1'], ['var', '2']]] // 2 + (2 +1)
  //, ['var', '2'] // 2
  //, ['app', ['var', '+1'], ['var', '0']] // 0 +1
  //, ['app', ['app', ['var', '+'], ['var', '0']], ['var', '0']] // 0 + 0
  //, ['app', ['var', 'head'], ['var', 'list-1-2-3']] //  head [1,2,3]
  //, ['app', ['var', 'tail'], ['var', 'list-1-2-3']] //  tail [1,2,3]
  //, ['app', ['app', ['var', 'drop'], ['var', '2']], ['var', 'list-1-2-3']] //  drop 2 [1,2,3]
  //, ['app', ['app', ['var', 'take'], ['var', '1']], ['var', 'list-1-2-3']] //  take 2 [1,2,3]
  //, ['app', ['var', 'length'], ['var', 'list-1-2-3']] // length [1,2,3]
  //, ['app', ['var', 'length-acc'], ['var', 'list-1-2-3']] // length-acc [1,2,3]

  //, ['var', 'list-1-2-3']
  //, ['app', ['var', 'head'], ['var', 'nil']]

  //, ['app', ['var', 'not'], ['var', 'false']]
  //, ['app', ['app', ['var', 'or'], ['var', 'true']], ['var', 'true']]
  //, ['app', ['var', 'null'], ['var', 'list-1-2-3']]
  //, ['app', ['var', 'null'], ['var', 'nil']]
  //, ['app', ['app', ['var', '<='], ['var', '1']], ['var', '3']]
  //, ['app', ['var', '-1'], ['var', '3']]
  , ['app', ['app', ['var', 'take'], ['app', ['app', ['var', '+'], ['var', '3']], ['var', '1']]], ['var', 'fib']]
  //, ['app', ['app', ['app', ['var', 'zip-with'], ['var', '+']], ['var', 'list-1-2-3']], ['var', 'list-1-2-3']]
  //, ['app', ['app', ['app', ['var', 'zip-with'], ['var', '+']], ['app', ['app', ['var', 'drop'], ['var', '2']], ['var', 'list-1-2-3']]], ['var', 'list-1-2-3']]
  ];

var prog2 =
  [ 'let'

  , 'id', ['lam', 'a', ['var', 'a']]
  , 'a', ['app', ['var', 'id'], ['lam', 'x', ['var', 'x']]]

  , ['app', ['var', 'a'], ['var', 'a']]
  ];

console.log(pretty(prog, 0, true));
console.log(pretty(normal_form(prog, {}), 0, true));
