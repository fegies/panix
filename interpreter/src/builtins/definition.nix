let
  throw = ___builtin_throw;
  builtins = {
    inherit builtins throw;
    true = true;
    false = false;
    null = null;
    tryEval = ___builtin_tryeval;
    typeOf = ___builtin_typeof;
  };
in
  __body__
