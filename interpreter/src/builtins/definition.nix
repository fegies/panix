let
  throw = ___builtin_throw;
  toString = ___builtin_tostring;
  builtins = {
    inherit builtins throw toString;
    true = true;
    false = false;
    null = null;
    tryEval = ___builtin_tryeval;
    typeOf = ___builtin_typeof;
  };
in
  __body__
