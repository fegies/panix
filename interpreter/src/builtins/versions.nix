let
  inherit (builtins) length typeOf elemAt filter split tryEval fromJSON isString match substring stringLength;
  isNotEmpty = a: isString a && a != "";
  pickNonEmpty = filter isNotEmpty;
  splitOnVersionSep = split "[\\.-]";

  splitVersion = vers: pickNonEmpty (splitOnVersionSep vers);

  compareVersions = l: r: compareVersionsSplit (splitVersion l) (splitVersion r);

  compareVersionsSplit = l: r: let
    iter = idx: let
      el = tryPick l idx;
      er = tryPick r idx;
    in
      if el == null && er == null
      then 0
      else if el == null
      then -1
      else if er == null
      then 1
      else let
        cmp_result = compareComponents el er;
      in
        if cmp_result == 0
        then iter (idx + 1)
        else cmp_result;
  in
    iter 0;

  tryPick = xs: idx:
    if idx < (length xs)
    then elemAt xs idx
    else null;

  compareComponents = l: r: let
    left_int = tryParseInt l;
    right_int = tryParseInt r;
  in
    if left_int != null && right_int != null
    then simpleCompare left_int right_int
    else if left_int != null
    then 1
    else if right_int != null
    then -1
    else compareComponentsStrings l r;

  # compare two version components, both of which are known to be strings
  compareComponentsStrings = l: r: let
    split_l = splitOffAlpha l;
    split_r = splitOffAlpha r;
  in
    if length split_l > 1 || length split_r > 1
    then compareVersionsSplit split_l split_r
    else let
      strip_pre = stripPrefix "pre";
      l_stripped = strip_pre l;
      r_stripped = strip_pre r;
    in
      if l_stripped != l
      then
        (
          if r_stripped != r
          then compareComponents l_stripped r_stripped
          else -1
        )
      else if r_stripped != r
      then 1
      else simpleCompare l r;

  stripPrefix = prefix: let
    prefixLen = stringLength prefix;
    pickPrefix = substring 0 prefixLen;
    removePrefix = substring prefixLen (-1);
  in
    str:
      if pickPrefix str == prefix
      then removePrefix str
      else str;

  tryParseInt = value: let
    parsed = (tryEval (fromJSON value)).value;
  in
    if typeOf parsed == "int"
    then parsed
    else null;

  simpleCompare = left: right:
    if left < right
    then -1
    else if right < left
    then 1
    else 0;

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
  unpackList = v:
    if typeOf v == "list"
    then elemAt v 0
    else v;

  parseDrvName = input: let
    matchResult = match "^(.+[^-])-(-*[^-]+)$" input;
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
