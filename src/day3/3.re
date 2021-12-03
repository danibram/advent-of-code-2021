let parser = () => {
  Sys.argv->Belt.Array.sliceToEnd(-1)->Belt.Array.get(0)
  |> Belt.Option.getUnsafe
  |> Node.Fs.readFileAsUtf8Sync
  |> Js.String.split("\n");
};

let binToDec = bin => int_of_string("0B" ++ bin);

let toBin =
  fun
  | true => "0"
  | false => "1";

let mostCommon = ((zeros, ones)) => (zeros > ones)->toBin;
let leastCommon = ((zeros, ones)) => (!(zeros > ones))->toBin;

let calculateRepetitions = (arrayBinaries, initialState) => {
  let count = ((zeros, ones), value) =>
    value == "1" ? (zeros, ones + 1) : (zeros + 1, ones);

  arrayBinaries->Belt.Array.reduce(initialState, (acc, value) => {
    acc->Belt.Array.mapWithIndex((index, acc) => {
      let v = value->Js.String2.get(index);
      count(acc, v);
    })
  });
};

let part1 = (input: array(Js.String.t)) => {
  let initialState =
    Belt.Array.make(input->Belt.Array.getUnsafe(0)->String.length, (0, 0));

  let repetitions = input->calculateRepetitions(initialState);
  let gammaRate =
    repetitions->Belt.Array.map(mostCommon)->Js.Array2.joinWith("");

  let epsilonRate =
    repetitions->Belt.Array.map(leastCommon)->Js.Array2.joinWith("");

  gammaRate->binToDec * epsilonRate->binToDec;
};

let part2 = (input: array(Js.String.t)) => {
  let initialState =
    Belt.Array.make(input->Belt.Array.getUnsafe(0)->String.length, (0, 0));

  let rec find =
          (
            array: array(Js.String.t),
            index: int,
            decideZeroOne: ((int, int)) => string,
          ) =>
    switch (array->Belt.Array.length) {
    | 1 => array->Belt.Array.getUnsafe(0)
    | _ =>
      let repetitionSelected =
        array
        ->calculateRepetitions(initialState)
        ->Belt.Array.getUnsafe(index);

      let newArr =
        array->Belt.Array.keep(v =>
          v->Js.String.get(index) == decideZeroOne(repetitionSelected)
        );

      find(newArr, index + 1, decideZeroOne);
    };

  let oxygenGeneratorRating = find(input, 0, mostCommon);
  let co2ScrubberRating = find(input, 0, leastCommon);

  oxygenGeneratorRating->binToDec * co2ScrubberRating->binToDec;
};

Js.log("3_1. " ++ string_of_int(part1(parser())));
Js.log("3_2. " ++ string_of_int(part2(parser())));
