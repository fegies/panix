let
  throw = ___builtin_throw;
  map = func: list: ___builtin_map {inherit func list;};
  toString = builtins.toString;
  import = ___builtin_import;
  abort = ___builtin_abort;
  builtins = import "<<<___builtins>>>";
  removeAttrs = builtins.removeAttrs;
in
  __body__
