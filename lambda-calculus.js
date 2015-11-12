function parse(expr){
  var toker = (function(){
    var token;
    var un_list = [];
    return {
      _get: function(){
        if( un_list.length )
          return un_list.pop();

        expr = expr.replace(/^\s+/, '');
        if( expr.length ){
          switch(expr[0]){
            case '(': case ')': case '\\':
              token = expr[0];
              expr = expr.substr(1);
              return token;

            default:
              expr = expr.replace(/^[^()\\\s]+/, function($0){
                token = $0;
                return '';
              });
              return token;
          }
        }
        else
          return null;
      },
      get: function(){
        var token = this._get();
        //console.log("token: " + token);
        return token;
      },

      unget: function(untoken){
        //console.log("unget: " + untoken);
        if( untoken!==null )
          un_list.push(untoken);
      },

      remain: function(){
        return un_list.reverse().join(' ') + expr;
      }
    };
  })();

  var is_end = function(){
    var token = toker.get();
    toker.unget(token);
    return (token===')' || token===null);
  };

  var take_node1;
  var take_node = function(){
    var token = toker.get();
    var node;
    if( token===null )
      throw 'unexpected end';

    if( token===')' )
      throw 'unexpected ")" near: ' + toker.remain();

    if( token==='(' ){
      node = take_node1();
      token = toker.get();
      if( token!==')' ){
        toker.unget(token);
        throw 'missing ")" near: ' + toker.remain();
      }
      return node;
    }

    if( token==='\\' ){
      token = toker.get();
      if( token===null )
        throw 'unexpected end';

      if( token==='(' || token===')' || token==='\\' )
        throw 'unexpected "' + token + '" near: ' + toker.remain();

      return ['lam', token, take_node1()];
    }

    return ['var', token];
  };
  take_node1 = function(){
    var node = take_node();
    while( !is_end() )
      node = ['app', node, take_node()];
    return node;
  };

  return take_node1();
}

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

function clone_env(env){
  var ctor = new Function;
  ctor.prototype = env;
  return new ctor;
}

function weak_normal_form(closure){
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

var doEval = function(){
  try{
    document.getElementById('result').value = pretty(normal_form({expr: parse(document.getElementById('source').value), env: {}}).expr);
  }
  catch(e){
    document.getElementById('result').value = 'ERROR: ' + e;
  }
};

document.getElementById('source').onkeydown = function(ev){
  if( ev.keyCode == 13 && (ev.ctrlKey || ev.metaKey || ev.shiftKey || ev.altKey) ){
    ev.preventDefault();
    ev.stopPropagation();
    doEval();
  }
};

document.getElementById('eval_btn').onclick = doEval;
