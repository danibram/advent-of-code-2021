let parser = () => {
  Sys.argv->Belt.Array.sliceToEnd(-1)->Belt.Array.get(0)
  |> Belt.Option.getUnsafe
  |> Node.Fs.readFileAsUtf8Sync
  |> Js.String.split("\n");
};

let count = ((zeros, ones), value) =>
  value == "1" ? (zeros, ones + 1) : (zeros + 1, ones);
let inv = value => value == "1" ? "0" : "1";

let convertNum = (num: string, toBase: int): string => {
  let v = int_of_string(num);
  Js.Int.toStringWithRadix(v, ~radix=toBase);
};

let checkAllPositions = arrayBinaries => {
  let initialState =
    Belt.Array.make(
      arrayBinaries->Belt.Array.getUnsafe(0)->String.length,
      (0, 0),
    );

  arrayBinaries->Belt.Array.reduce(initialState, (acc, value) => {
    acc->Belt.Array.mapWithIndex((index, acc) => {
      let v = value->Js.String2.get(index);
      count(acc, v);
    })
  });
};
let part1 = (input: array(Js.String.t)) => {
  let gammaRate =
    checkAllPositions(input)
    ->Belt.Array.map(((zeros, ones)) => {zeros > ones ? "0" : "1"})
    ->Js.Array2.joinWith("");

  let epsilonRate =
    Js.Array2.fromMap(Js.String2.castToArrayLike(gammaRate), x => x)
    ->Js.Array.map(inv, _)
    ->Js.Array2.joinWith("");

  int_of_string("0B" ++ gammaRate) * int_of_string("0B" ++ epsilonRate);
};

let part2 = (input: array(Js.String.t)) => {
  let rec find = (array: array(Js.String.t), index: int, inverted) =>
    if (array->Belt.Array.length == 1) {
      array->Belt.Array.getUnsafe(0);
    } else {
      let (zeros, ones) =
        checkAllPositions(array)->Belt.Array.getUnsafe(index);
      let toFilter = zeros > ones ? "0" : "1";
      let toFilter = inverted ? inv(toFilter) : toFilter;
      let newArr =
        array->Belt.Array.keep(v => v->Js.String.get(index) == toFilter);

      find(newArr, index + 1, inverted);
    };

  let ogr = find(input, 0, false);
  let co2 = find(input, 0, true);

  int_of_string("0B" ++ ogr) * int_of_string("0B" ++ co2);
};

Js.log("3_1. " ++ string_of_int(part1(parser())));
Js.log("3_2. " ++ string_of_int(part2(parser())));
