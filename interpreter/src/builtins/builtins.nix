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
  splitOnVersionSep = split "\\.|-";
  isNotEmpty = a: isString a && a != "";
  pickNonEmpty = filter isNotEmpty;
  splitVersion = vers: pickNonEmpty (splitOnVersionSep vers);
  isFloat = arg: typeOf arg == "float";
  elemAt = list: idx: ___builtin_elemat {inherit list idx;};
  concatLists = ___builtin_concatLists;
  fromJSON = ___builtin_fromJSON;
  tryEval = ___builtin_tryeval;
  match = regex: str: ___builtin_match [regex str];
  concatMap = f: list: concatLists (map f list);
  seq = e1: e2: ___builtin_seq [e1 e2];
  deepSeq = e1: e2: ___builtin_deepSeq [e1 e2];

  genList = generator: length: let
    # we go with radix 4 to improve perf a little and reduce recursion depth.
    # we could go wider, but the cases would need to be hardcoded, or there would be little benefit over just recursing one level deeper.
    iter = length: offset:
    # the 0 length case is handled by the outer function already.
      if length <= 4 # it is 1 or 2
      then
        ___builtin_seq [
          offset
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
        ]
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
    if length == 0
    then []
    else if typeOf length == "int"
    then iter length 0
    else throw "genList: length must be an integer";

  foldl' = op: nul: list: let
    max_len = length list;
    elem_at_list = elemAt list;
    iter = acc: idx:
      if idx == max_len
      then acc
      else let
        new_acc = op acc (elem_at_list idx);
        next_idx = idx + 1;
        step = ___builtin_seq [new_acc next_idx (iter new_acc)];
      in
        step next_idx;
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
      deepSeq
      abort
      typeOf
      match
      genList
      isInt
      foldl'
      filter
      split
      isString
      isFloat
      elemAt
      splitVersion
      fromJSON
      concatLists
      seq
      tryEval
      length
      ;
    true = true;
    false = false;
    null = null;
    nixVersion = "2.24.10";
    isAttrs = arg: typeOf arg == "set";
    isBool = arg: typeOf arg == "bool";
    isFunction = arg: typeOf arg == "lambda";
    isList = arg: typeOf arg == "list";
    isNull = arg: arg == null;
    isPath = arg: typeOf arg == "path";
    langVersion = 6;
    hasAttr = s: set: set ? "${s}";
    getAttr = s: set: set."${s}";

    baseNameOf = let
      matcher = match "^.*?([^/]*)/?$";
      result = name: let
        matchResult = matcher name;
      in
        if matchResult == null
        then name
        else elemAt matchResult 0;
    in
      seq matcher result;

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

    mapAttrs = func: attrset: ___builtin_mapAttrs [func attrset];

    replaceStrings = patterns: replacements: string:
      ___builtin_replaceStrings [
        patterns
        replacements
        string
      ];

    attrValues = ___builtin_attrValues;

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
    lessThan = let
      isNum = a: let type = typeOf a; in type == "int" || type == "float";
    in
      e1:
        if isNum e1
        then
          e2:
            if isNum e2
            then e1 < e2
            else throw "lessThan: e2 must be a number"
        else throw "lessThan: e1 must be a number";

    removeAttrs = set: list: ___builtin_removeAttrs [set list];

    stringLength = ___builtin_stringLength;

    substring = start: len: string: ___builtin_substring [start len string];

    compareVersions = let
      unpackList = v:
        if typeOf v == "list"
        then elemAt v 0
        else v;
      # splitLeadingDigits: string -> [string|[string]]
      splitLeadingDigits = split "^([0-9]+)";
      # isNotEmptyString: any -> bool
      isNotEmptyString = v: v != "";
      # splitOffAlpha: string -> [string]
      # example: "0123" -> ["0123"]
      # example: "abc123" -> ["abc123"]
      # example: "123abc" -> ["123" "abc"]
      splitOffAlpha = value:
        if value == ""
        then [value]
        else map unpackList (filter isNotEmptyString (splitLeadingDigits value));
      # already partially applied concatMap
      concatMap_splitOffAlpha = concatMap splitOffAlpha;
      # a function string -> [string], extension of splitVersion.
      # first splits on the version separators, then splits each item again
      # on leading digits if any available. We do this to match what upstream nix does.
      deepSplitVersion = input: concatMap_splitOffAlpha (splitVersion input);
      # simpleCompare: any -> any -> -1|0|1
      simpleCompare = left: right:
        if left < right
        then -1
        else if right < left
        then 1
        else 0;
      # tryParseInt : string -> int|null
      tryParseInt = value: let
        parsed = (tryEval (fromJSON value)).value;
      in
        if typeOf parsed == "int"
        then parsed
        else null;
      # compareComponents: string -> string -> -1|0|1
      compareComponents = left: right:
        if left == right
        then 0
        else let
          left_int = tryParseInt left;
          right_int = tryParseInt right;
        in
          if left_int != null && right_int != null
          then simpleCompare left_int right_int
          else if left == "pre"
          then -1
          else if right == "pre"
          then 1
          else if right_int != null
          then -1
          else if left_int != null
          then 1
          else simpleCompare left right;
    in
      v1: let
        v1_split = deepSplitVersion v1;
        v1_len = length v1_split;
      in
        v2: let
          v2_split = deepSplitVersion v2;
          v2_len = length v2_split;
          max_len =
            if v1_len <= v2_len
            then v1_len
            else v2_len;
          compare_iter = idx:
            if idx == max_len
            then
              (
                if v1_len < v2_len
                then 1
                else if v2_len < v1_len
                then -1
                else 0
              )
            else let
              res = compareComponents (elemAt v1_split idx) (elemAt v2_split idx);
            in
              if res != 0
              then res
              else compare_iter (idx + 1);
        in
          compare_iter 0;

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
              if comparator left_head right_head
              then # left is smaller
                {
                  head = right_head;
                  tail = merge left (right.tail);
                }
              else # right is smaller or equal
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
      # convert a cons list into a nix list
      cons_skip = len: list:
        if len == 0
        then list
        else cons_skip (len - 1) list.tail;
      # converts a cons list of the specified length to a nix list
      decons = len: lst:
        if len == 1
        then let
          head = lst.head;
        in
          # we do not want to carry around the thunks from decons
          ___builtin_seq [head [lst.head]]
        else let
          left_len = len / 2;
          right_len = len - left_len;
        in
          (decons left_len lst) ++ (decons right_len (cons_skip left_len lst));
    in
      if list_len <= 1
      then list
      else decons list_len (cons_inner_sort 0 list_len);
  };
in
  builtins
