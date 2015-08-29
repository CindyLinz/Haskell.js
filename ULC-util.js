function str_times(str, n){
  var out = '', i;
  for(i=0; i<n; ++i)
    out += str;
  return out;
}

function pretty(expr){
  var out = '', i;
  switch(expr[0]){
    case 'lam':
      return '\\' + expr[1] + ' ' + pretty(expr[2]);

    case 'var':
      return expr[1];

    case 'app':
      return (expr[1][0]==='var' ? '' : '(') + pretty(expr[1]) + (expr[1][0]==='var' ? '' : ')') + ' ' + (expr[2][0]==='var' ? '' : '(') + pretty(expr[2]) + (expr[2][0]==='var' ? '' : ')');

    case 'int':
      out = '[' + expr[1] + ' (' + expr[2] + ')';
      for(i=4; i<expr.length; ++i)
        out += ' ' + pretty(expr[i]);
      out += ']';
      return out;
  }
}

function pretty_closure(closure, indent){
  var out = pretty(closure.expr), i;
  out += '\nwhere';
  for(i in closure.env){
    console.log(i);
    out += '\n  ' + str_times('  ', indent) + i + ': ' + (closure.env[i] ? pretty(closure.env[i].expr, indent+2) : '(undefined)');
  }
  return out;
}

if( module )
  module.exports = {
    pretty: pretty,
    pretty_closure: pretty_closure
  };
