let
  throw = ___builtin_throw;
  builtins = {
    inherit builtins throw;
    true = true;
    false = false;
    null = null;
    tryEval = ___builtin_tryeval;
  };
in
  __body__
