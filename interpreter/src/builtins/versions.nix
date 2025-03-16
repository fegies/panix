let
  inherit (builtins) length typeOf elemAt concatMap filter split tryEval fromJSON isString match;
  isNotEmpty = a: isString a && a != "";
  pickNonEmpty = filter isNotEmpty;
  splitOnVersionSep = split "\\.|-";

  splitVersion = vers: pickNonEmpty (splitOnVersionSep vers);

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

  parseDrvName = input: let
    matchResult = match "^([^-]+)-(.+)$" input;
  in
    if matchResult == null
    then {
      name = input;
      version = "";
    }
    else {
      name = elemAt matchResult 0;
      version = elemAt matchResult 1;
    };
in {
  inherit compareVersions splitVersion parseDrvName;
}
