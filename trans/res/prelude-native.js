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

env['Y#'] = {
  expr: ['lam', 'f', ['app', ['lam', 'x', ['app', ['var', 'f'], ['app', ['var', 'x'], ['var', 'x']]]], ['lam', 'x', ['app', ['var', 'f'], ['app', ['var', 'x'], ['var', 'x']]]]]],
  env: env
};

env['[]'] = {
  expr: ['lam', 'is-nil', ['lam', 'is-cons', ['var', 'is-nil']]],
  env: env
};

env['(:)'] = {
  expr: ['lam', 'a', ['lam', 'as', ['lam', 'is-nil', ['lam', 'is-cons', ['app', ['app', ['var', 'is-cons'], ['var', 'a']], ['var', 'as']]]]]],
  env: env
};

env['(+#)'] = {
  expr: ['int', '(+#)', 2, function(a, b){
    return {expr: ['dat', weak_normal_form(a).expr[1] + weak_normal_form(b).expr[1]], env: env};
  }],
  env: env
};

env['(-#)'] = {
  expr: ['int', '(-#)', 2, function(a, b){
    return {expr: ['dat', weak_normal_form(a).expr[1] - weak_normal_form(b).expr[1]], env: env};
  }],
  env: env
};

env['(*#)'] = {
  expr: ['int', '(*#)', 2, function(a, b){
    return {expr: ['dat', weak_normal_form(a).expr[1] * weak_normal_form(b).expr[1]], env: env};
  }],
  env: env
};

env['quotInt#'] = {
  expr: ['int', 'quotInt#', 2, function(a, b){
    return {expr: ['dat', (weak_normal_form(a).expr[1] / weak_normal_form(b).expr[1]) | 0], env: env};
  }],
  env: env
};

env['remInt#'] = {
  expr: ['int', 'remInt#', 2, function(a, b){
    return {expr: ['dat', weak_normal_form(a).expr[1] % weak_normal_form(b).expr[1]], env: env};
  }],
  env: env
};

env['(<=#)'] = {
  expr: ['int', '(<=#)', 2, function(a, b){
    return {expr: ['dat', (weak_normal_form(a).expr[1] <= weak_normal_form(b).expr[1]) | 0], env: env};
  }],
  env: env
};

env['(==#)'] = {
  expr: ['int', '(==#)', 2, function(a, b){
    return {expr: ['dat', (weak_normal_form(a).expr[1] == weak_normal_form(b).expr[1]) | 0], env: env}
  }],
  env: env
};

env['chr#'] = {
  expr: ['int', 'chr#', 1, function(n){
    return {expr: ['dat', String.fromCharCode(n.expr)], env: env}
  }],
  env: env
};

env['ord#'] = {
  expr: ['int', 'ord#', 1, function(ch){
    return {expr: ['dat', ch.expr.charCodeAt(0)], env: env}
  }],
  env: env
};

/*
'runIO': {
  expr: ['int', 'runIO', 1, function(ioAct){
    return {expr: ['app', ioAct.expr, null], env: ioAct.env};
  }],
  env: {}
},
*/

env['show-int'] = {
  expr: ['int', 'show-int', 1, function(num){
    num = weak_normal_form(num);
    return weak_normal_form(js_str_to_lc_str(''+num.expr[1]));
  }],
  env: env
};

env['read-int'] = {
  expr: ['int', 'read-int', 1, function(str){
    return {expr: ['var', lc_str_to_js_str(str) | 0], env: {}};
  }],
  env: env
};

var outBuffer = '';

env['putChar#'] = {
  expr: ['int', 'putChar', 2, function(ch, s){
    ch = weak_normal_form(ch);
    if( ch.expr[1]=='\n' ){
      console.log(outBuffer);
      outBuffer = '';
    }else{
      outBuffer += ch.expr[1];
    }
    return {expr: ['lam', 'p', ['app', ['app', ['var', 'p'], s], ['lam', 'a', ['var', 'a']]]], env: {}};
  }],
  env: env
};

env['putChar'] = {
  expr: ['lam', 'ch', ['app', ['var', 'ch'], ['lam', 'ch#', ['app', ['var', 'putChar#'], ['var', 'ch#']]]]],
  env: env
};

env['putStrLn'] = {
  expr: ['lam', 'str', ['lam', 's', ['app', ['app', ['var', 'str'], ['app', ['app', ['var', 'putChar#'], ['dat', String.fromCharCode(10)]], ['var', 's']]], ['lam', 'ch', ['lam', 'str-', ['app', ['app', ['app', ['var', 'putChar'], ['var', 'ch']], ['var', 's']], ['lam', 's1', ['lam', '_', ['app', ['app', ['var', 'putStrLn'], ['var', 'str-']], ['var', 's1']]]]]]]]]],
  env: env
};

env['getLine'] = {
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
  env: env
};
