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

//var code = 'fix (\\unfix-fibs (\\fibs cons 1 (cons 1 (zip-with + fibs (tail fibs)))) (unfix-fibs unfix-fibs))';
//console.log(code);
//console.log(JSON.stringify(parse(code)));
var code = '';
process.stdin.on('data', function(chunk){
  code += chunk;
});
process.stdin.on('end', function(){
  console.log(JSON.stringify(parse(code)));
  process.exit();
});
