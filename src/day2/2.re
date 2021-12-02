let parser = () => {
  Sys.argv->Belt.Array.sliceToEnd(-1)->Belt.Array.get(0)
  |> Belt.Option.getUnsafe
  |> Node.Fs.readFileAsUtf8Sync
  |> Js.String.split("\n");
};

let splitInstruction = line => {
  switch (line |> Js.String.split(" ")) {
  | [|a, b|] => Some((a, b->int_of_string))
  | _ => None
  };
};

let part1 = (input: array(Js.String.t)) => {
  let (hp, d) =
    input
    ->Belt.Array.map(splitInstruction)
    ->Belt.Array.reduce((0, 0), ((hp, d), value) => {
        switch (value) {
        | Some(("forward", number)) => (hp + number, d)
        | Some(("down", number)) => (hp, d + number)
        | Some(("up", number)) => (hp, d - number)
        | _ => (hp, d)
        }
      });

  hp * d;
};

let part2 = (input: array(Js.String.t)) => {
  let (hp, d, _) =
    input
    ->Belt.Array.map(splitInstruction)
    ->Belt.Array.reduce((0, 0, 0), ((hp, d, aim), value) => {
        switch (value) {
        | Some(("forward", number)) => (hp + number, d + aim * number, aim)
        | Some(("down", number)) => (hp, d, aim + number)
        | Some(("up", number)) => (hp, d, aim - number)
        | _ => (hp, d, aim)
        }
      });

  hp * d;
};

Js.log("2_1. " ++ string_of_int(part1(parser())));
Js.log("2_2. " ++ string_of_int(part2(parser())));
