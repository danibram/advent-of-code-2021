let parser = () => {
  Sys.argv->Belt.Array.sliceToEnd(-1)->Belt.Array.get(0)
  |> Belt.Option.getUnsafe
  |> Node.Fs.readFileAsUtf8Sync
  |> Js.String.split("\n");
};

let arrayCalc = array =>
  array->Belt.Array.reduce((0, None), ((acc, lastValue), value) => {
    switch (lastValue) {
    | Some(lastValue) =>
      lastValue < value ? (acc + 1, Some(value)) : (acc, Some(value))
    | _ => (0, Some(value))
    }
  });

let part1 = (input: array(Js.String.t)) => {
  let (inc, _) = input->Belt.Array.map(int_of_string)->arrayCalc;

  inc;
};

let part2 = (input: array(Js.String.t)) => {
  let inputArr = input->Belt.Array.map(int_of_string);

  let (inc, _) =
    inputArr
    ->Belt.Array.reduceWithIndex(
        [||],
        (acc, value, index) => {
          let one = value;
          let two = inputArr->Belt.Array.get(index + 1);
          let three = inputArr->Belt.Array.get(index + 2);

          switch (two, three) {
          | (_, None)
          | (None, _) => acc
          | (Some(two), Some(three)) =>
            acc->Belt.Array.concat([|one + two + three|])
          };
        },
      )
    ->arrayCalc;

  inc;
};

Js.log("1_1. " ++ string_of_int(part1(parser())));
Js.log("1_2. " ++ string_of_int(part2(parser())));
