type binary_tree('a) =
  | Leaf
  | Node('a, binary_tree('a), binary_tree('a));

type str_binary_tree = binary_tree(string);

let node: str_binary_tree =
  Node("12", Node("11", Leaf, Leaf), Node("14", Leaf, Leaf));

let serialize = (node: str_binary_tree): string => {
  let rec aux = (n, acc) =>
    switch (n) {
    | Leaf => acc ++ "-1"
    | Node(x, l, Leaf) => aux(l, acc ++ " " ++ x ++ " " ++ "-1 ")
    | Node(x, Leaf, r) => aux(r, acc ++ " " ++ "-1" ++ " " ++ x)
    | Node(x, l, r) => aux(l, acc ++ x) ++ aux(r, acc)
    };

  aux(node, "");
};

let splitString = str => Js.String.split(" ", str);

Js.log(splitString(serialize(node)));

let rec deserialize = lst => {
  switch (lst) {
  | [] => Leaf
  | [x, ...xs] =>
    x === "-1"
      ? deserialize(xs) : Node(x, deserialize(xs), deserialize(xs))
  };
};

Js.log(splitString(serialize(node)) |> Array.to_list |> deserialize);

let deserializewithArray = arr => {
  let rec aux = arr => {
    switch (Array.length(arr)) {
    | 0 => Leaf
    | len =>
      Node(
        arr[0],
        aux(Array.sub(arr, 1, len - 1)),
        aux(Array.sub(arr, 1, len - 1)),
      )
    };
  };
  aux(arr);
};

Js.log(splitString(serialize(node)) |> deserializewithArray);