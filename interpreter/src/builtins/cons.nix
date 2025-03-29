let
  inherit (builtins) length elemAt seq;
  # convert a cons list into a nix list
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
in {
  inherit decons cons_elem cons_append to_cons;
}
