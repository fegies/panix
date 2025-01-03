let
  throw = ___builtin_throw;
  map = func: list: ___builtin_map {inherit func list;};
  toString = builtins.toString;
  import = ___builtin_import;
  abort = ___builtin_abort;
  builtins = import "<<<___builtins>>>";
  removeAttrs = builtins.removeAttrs;
  isNull = builtins.isNull;
  baseNameOf = builtins.baseNameOf;
  dirOf = builtins.dirOf;
in
  __body__
