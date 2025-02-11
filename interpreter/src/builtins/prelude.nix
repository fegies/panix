let
  throw = ___builtin_throw;
  toString = builtins.toString;
  import = ___builtin_import;
  abort = ___builtin_abort;
  builtins = import "<<<___builtins>>>";
  map = builtins.map;
  removeAttrs = builtins.removeAttrs;
  isNull = builtins.isNull;
  baseNameOf = builtins.baseNameOf;
  dirOf = builtins.dirOf;
in
  __body__
