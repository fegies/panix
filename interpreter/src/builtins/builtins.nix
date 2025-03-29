# the purpose of this file is to define
# the builtins attribute set.
# the 4 bindings of the prelude are always available.
let
  typeOf = ___builtin_typeof;
  length = ___builtin_length;
  filter = func: list:
    ___builtin_filter_pick {
      inherit list;
      pickSet = map func list;
    };
  isString = arg: typeOf arg == "string";
  isInt = arg: typeOf arg == "int";
  split = regex: str: ___builtin_split {inherit regex str;};
  isFloat = arg: typeOf arg == "float";
  attrNames = ___builtin_attrNames;
  elemAt = list: idx: ___builtin_elemat {inherit list idx;};
  concatLists = ___builtin_concatLists;
  fromJSON = ___builtin_fromJSON;
  tryEval = ___builtin_tryeval;
  match = regex: str: ___builtin_match [regex str];
  concatMap = f: list: concatLists (map f list);
  seq = e1: e2: ___builtin_seq [e1 e2];
  deepSeq = e1: e2: ___builtin_deepSeq [e1 e2];
  stringLength = ___builtin_stringLength;
  mapAttrs = func: attrset: ___builtin_mapAttrs [func attrset];
  map = func: list: ___builtin_map {inherit func list;};
  isPath = arg: typeOf arg == "path";
  attrValues = ___builtin_attrValues;
  hasAttr = s: set: set ? "${s}";
  elem = x: xs: let
    list_len = length xs;
    pick = elemAt xs;
    iter = idx:
      if idx == list_len
      then false
      else if pick idx == x
      then true
      else iter (idx + 1);
  in
    iter 0;

  decons = len: lst:
    if len == 0
    then []
    else let
      decons_inner = len: lst:
        if len <= 4
        then
          seq lst (
            if len <= 2
            then
              (
                if len == 1
                then [lst.head]
                else [lst.head lst.tail.head]
              )
            else
              (
                if len == 3
                then let
                  t = lst.tail;
                in [lst.head t.head t.tail.head]
                else let
                  t1 = lst.tail;
                  t2 = t1.tail;
                in [lst.head t1.head t2.head t2.tail.head]
              )
          )
        else let
          step_size = len / 4;
          skip_step = cons_skip step_size;
          decons_step = decons_inner step_size;
          lst_2 = skip_step lst;
          lst_3 = skip_step lst_2;
          lst_4 = skip_step lst_3;
        in
          ___builtin_concatLists [
            (decons_step lst)
            (decons_step lst_2)
            (decons_step lst_3)
            (decons_inner (len - 3 * step_size) lst_4)
          ];
    in
      decons_inner len lst;

  # convert a cons list into a nix list
  cons_skip = len: list:
    if len == 0
    then list
    else cons_skip (len - 1) list.tail;
  to_cons = list: let
    list_len = length list;
    pick = elemAt list;
    go = idx:
      if idx == list_len
      then null
      else {
        head = pick idx;
        tail = go (idx + 1);
      };
  in
    go 0;
  cons_append = left: right: let
    go = l:
      if l == null
      then right
      else {
        head = l.head;
        tail = go l.tail;
      };
  in
    go left;
  cons_elem = x: let
    iter = xs:
      if xs == null
      then false
      else if xs.head == x
      then true
      else iter xs.tail;
  in
    iter;

  genList = generator: length: let
    # we go with radix 4 to improve perf a little and reduce recursion depth.
    # we could go wider, but the cases would need to be hardcoded, or there would be little benefit over just recursing one level deeper.
    iter = length: offset:
    # the 0 length case is handled by the outer function already.
      if length <= 4 # it is 1 or 2
      then
        (
          # base cases.
          if length <= 2
          then
            (
              if length == 1
              then [(generator offset)]
              else [(generator offset) (generator (offset + 1))] # length 2
            )
          else # it must be > 2 and <= 4
            (
              if length == 3
              then [(generator offset) (generator (offset + 1)) (generator (offset + 2))]
              else #length 4
                [(generator offset) (generator (offset + 1)) (generator (offset + 2)) (generator (offset + 3))]
            )
        )
      else let
        # recursion....
        step_size = length / 4;
        iter_step_size = iter step_size;
      in
        concatLists [
          (iter_step_size offset)
          (iter_step_size (offset + step_size))
          (iter_step_size (offset + 2 * step_size))
          # the last one may have a slightly smaller step
          (iter (length - 3 * step_size) (offset + 3 * step_size))
        ];
  in
    if typeOf length != "int"
    then throw "genList: length must be an integer"
    else if length < 0
    then throw "length must not be negative"
    else if length == 0
    then []
    else iter length 0;

  foldl' = op: nul: list: let
    max_len = length list;
    pick = elemAt list;
    iter = acc: idx:
      if idx == max_len
      then acc
      else let
        new_acc = op acc (pick idx);
      in
        ___builtin_seq [new_acc (iter new_acc)] (idx + 1);
  in
    iter nul 0;

  toString = arg:
    if ___builtin_typeof arg == "set"
    then
      toString (
        if arg ? __toString
        then arg.__toString arg
        else arg.outPath or (throw "cannot coerce a set to a string")
      )
    else ___builtin_tostring arg;
  builtins = {
    inherit
      concatMap
      throw
      builtins
      toString
      map
      mapAttrs
      deepSeq
      elem
      attrNames
      hasAttr
      attrValues
      abort
      typeOf
      match
      genList
      isInt
      isPath
      foldl'
      filter
      split
      isString
      stringLength
      isFloat
      elemAt
      fromJSON
      concatLists
      seq
      tryEval
      length
      ;
    inherit
      (import "<<<___versions>>>")
      compareVersions
      splitVersion
      parseDrvName
      ;
    true = true;
    false = false;
    null = null;
    nixVersion = "2.24.10";
    toJSON = ___builtin_toJSON;
    isAttrs = arg: typeOf arg == "set";
    isBool = arg: typeOf arg == "bool";
    isFunction = arg: typeOf arg == "lambda";
    isList = arg: typeOf arg == "list";
    isNull = arg: arg == null;
    langVersion = 6;
    getAttr = s: set: set."${s}";
    currentSystem = "x86_64-linux";
    add = a: b: a + b;
    sub = a: b: a - b;
    mul = a: b: a * b;
    div = a: b: a / b;
    bitAnd = a: b: ___builtin_bitand [a b];
    bitOr = a: b: ___builtin_bitor [a b];
    bitXor = a: b: ___builtin_bitxor [a b];
    floor = ___builtin_floor;
    ceil = ___builtin_ceil;
    functionArgs = ___builtin_functionArgs;

    concatStringsSep = separator: list: let
      pick = elemAt list;
      recurse = start: len:
        if len == 1
        then pick start
        else let
          half_len = len / 2;
          left = recurse start half_len;
          right = recurse (start + half_len) (len - half_len);
        in
          left + separator + right;
      listlen = length list;
    in
      if listlen == 0
      then ""
      else recurse 0 listlen;

    baseNameOf = let
      matcher = match "^.*?([^/]*)/?$";
    in
      name: let
        matchResult = matcher (
          if isPath name
          then (___builtin_tostring name)
          else name
        );
      in
        if matchResult == null
        then name
        else elemAt matchResult 0;

    dirOf = name: let
      matchResult = match "^(.*)/[^/]*$" name;
    in
      if matchResult == null
      then "." # it did not contain a slash
      else let
        unpacked = elemAt matchResult 0;
      in
        if unpacked == ""
        then "/" # it was just a single slash
        else unpacked;

    catAttrs = attr:
      concatMap (
        set:
          if set ? "${attr}"
          then [set."${attr}"]
          else []
      );

    replaceStrings = patterns: replacements: string:
      ___builtin_replaceStrings [
        patterns
        replacements
        string
      ];

    partition = predicate: list: let
      pickSetRight = map predicate list;
    in {
      right = ___builtin_filter_pick {
        inherit list;
        pickSet = pickSetRight;
      };
      wrong = ___builtin_filter_pick {
        inherit list;
        pickSet = map (v: !v) pickSetRight;
      };
    };
    lessThan = a: b: a < b;

    removeAttrs = set: list: ___builtin_removeAttrs [set list];

    substring = let
      least = a: b:
        if a <= b
        then a
        else b;
    in
      start: len: string: let
        strlen = stringLength string;
      in
        if start >= strlen
        then ""
        else
          ___builtin_substring [
            start
            (least (
              if len < 0
              then strlen
              else len
            ) (strlen - start))
            string
          ];

    trace = msg: value: ___builtin_trace [msg value];
    dbg = value: ___builtin_trace [value value];

    head = list: elemAt list 0;

    tail = list: let
      lstlen = length list;
      pick = elemAt list;
    in
      if lstlen == 0
      then throw "'tail called on an empty list'"
      else genList (idx: pick (idx + 1)) (lstlen - 1);

    sort = comparator: list: let
      pick = elemAt list;
      list_len = length list;
      # merge 2 cons lists, producing a cons list in order.
      merge = left: right:
        if left == null
        then right # left exhausted.
        else if right == null
        then left # left empty
        else # both not empty
          (
            let
              left_head = left.head;
              right_head = right.head;
            in
              if comparator right_head left_head
              then # right is smaller
                {
                  head = right_head;
                  tail = merge left (right.tail);
                }
              else # left is smaller or equal
                {
                  head = left_head;
                  tail = merge (left.tail) right;
                }
          );
      # generates a sorted cons list covering the requested range.
      cons_inner_sort = start_idx: len:
        if len == 1
        then {
          head = pick start_idx;
          tail = null;
        }
        else let
          left_len = len / 2;
          right_len = len - left_len;
        in
          merge (cons_inner_sort start_idx left_len) (cons_inner_sort (start_idx + left_len) right_len);
    in
      if list_len <= 1
      then list
      else decons list_len (cons_inner_sort 0 list_len);

    any = predicate: list: let
      list_len = length list;
      pick = elemAt list;
      iter = idx:
        if idx == list_len
        then false
        else if predicate (pick idx)
        then true
        else iter (idx + 1);
    in
      iter 0;

    all = predicate: list: let
      list_len = length list;
      pick = elemAt list;
      iter = idx:
        if idx == list_len
        then true
        else if predicate (pick idx)
        then iter (idx + 1)
        else false;
    in
      iter 0;

    genericClosure = {
      startSet,
      operator,
    }: let
      iter = input @ {
        done_keys,
        result,
        result_len,
        work_list,
      }:
        if work_list == null # work set is empty...
        then input # we are done, just return the result
        else if cons_elem work_list.head.key done_keys
        then # we already processed that key
          iter {
            inherit done_keys result result_len;
            work_list = work_list.tail; # skip it
          }
        else
          # the key was actually new. Add the entry to the result set
          # and expand the pending set.
          iter {
            done_keys = {
              head = work_list.head.key;
              tail = done_keys;
            };
            result = {
              head = work_list.head;
              tail = result;
            };
            result_len = result_len + 1;
            work_list = cons_append work_list.tail (to_cons (operator work_list.head));
          };
      result = iter {
        done_keys = null;
        result = null;
        result_len = 0;
        work_list = to_cons startSet;
      };
    in
      decons result.result_len result.result;

    intersectAttrs = left: right: let
      names_unique_to_right = filter (name: !(hasAttr name left)) (attrNames right);
    in
      removeAttrs right names_unique_to_right;

    listToAttrs = let
      transfromas = as: {
        "${as.name}" = as.value;
      };
      do_fold = foldl' (acc: elem: elem // acc) {};
    in
      lst: do_fold (map transfromas lst);
  };
in
  builtins
