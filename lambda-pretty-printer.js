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

document.getElementById('eval_btn').onclick = function(){
  try{
    document.getElementById('result').value = pretty(JSON.parse(document.getElementById('source').value));
  }
  catch(e){
    document.getElementById('result').value = 'ERROR: ' + e;
  }
};
