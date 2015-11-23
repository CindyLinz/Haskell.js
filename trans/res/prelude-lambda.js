env['True'] = {
  expr: ['lam', 'is-false', ['lam', 'is-true', ['var', 'is-true']]],
  env: env
};
env['False'] = {
  expr: ['lam', 'is-false', ['lam', 'is-true', ['var', 'is-false']]],
  env: env
};

env['I#'] = {
  expr: ['lam', 'n', ['lam', 'f', ['app', ['var', 'f'], ['var', 'n']]]],
  env: env
};

env['(:)'] = {
  expr: ['lam', 'a', ['lam', 'as', ['lam', 'is-nil', ['lam', 'is-cons', ['app', ['app', ['var', 'is-cons'], ['var', 'a']], ['var', 'as']]]]]],
  env: env
};
env['[]'] = {
  expr: ['lam', 'is-nil', ['lam', 'is-cons', ['var', 'is-nil']]],
  env: env
};

env['(+)'] = {
  expr: ['lam', 'a', ['lam', 'b', ['app', ['var', 'a'], ['lam', 'a#', ['app', ['var', 'b'], ['lam', 'b#', ['app', ['var', 'I#'], ['app', ['app', ['var', '(+#)'], ['var', 'a#']], ['var', 'b#']]]]]]]]],
  env: env
};

env['(-)'] = {
  expr: ['lam', 'a', ['lam', 'b', ['app', ['var', 'a'], ['lam', 'a#', ['app', ['var', 'b'], ['lam', 'b#', ['app', ['var', 'I#'], ['app', ['app', ['var', '(-#)'], ['var', 'a#']], ['var', 'b#']]]]]]]]],
  env: env
};

env['(*)'] = {
  expr: ['lam', 'a', ['lam', 'b', ['app', ['var', 'a'], ['lam', 'a#', ['app', ['var', 'b'], ['lam', 'b#', ['app', ['var', 'I#'], ['app', ['app', ['var', '(*#)'], ['var', 'a#']], ['var', 'b#']]]]]]]]],
  env: env
};

env['div'] = {
  expr: ['lam', 'a', ['lam', 'b', ['app', ['var', 'a'], ['lam', 'a#', ['app', ['var', 'b'], ['lam', 'b#', ['app', ['var', 'I#'], ['app', ['app', ['var', 'quotInt#'], ['var', 'a#']], ['var', 'b#']]]]]]]]],
  env: env
};

env['mod'] = {
  expr: ['lam', 'a', ['lam', 'b', ['app', ['var', 'a'], ['lam', 'a#', ['app', ['var', 'b'], ['lam', 'b#', ['app', ['var', 'I#'], ['app', ['app', ['var', 'remInt#'], ['var', 'a#']], ['var', 'b#']]]]]]]]],
  env: env
};

//env['(<=)'] = {
//  expr: ['lam', 'a', ['lam', 'b', ['app', ['var', 'a'], ['lam', 'a#', ['app', ['var', 'b'], ['lam', 'b#', ['app', ['var', 'I#'], ['app', ['app', ['var', '(*#)'], ['var', 'a#']], ['var', 'b#']]]]]]]]],
//  env: env
//};
