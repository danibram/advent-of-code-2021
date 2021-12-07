let readInput = () => {
  Sys.argv->Belt.Array.sliceToEnd(-1)->Belt.Array.get(0)
  |> Belt.Option.getUnsafe
  |> Node.Fs.readFileAsUtf8Sync;
};

let process = v =>
  v |> Js.String.split(",") |> Belt.Array.map(_, Int64.of_string);

let rec tick = (queue, fishes, index, finish) =>
  if (index == finish) {
    fishes;
  } else {
    let value = queue |> Js.Array.shift |> Belt.Option.getUnsafe;

    queue[6] = queue[6]->Int64.add(value);

    tick(
      queue->Belt.Array.concat([|value|]),
      value->Int64.add(fishes),
      index + 1,
      finish,
    );
  };

let arrayToOctalArray = fish =>
  fish->Belt.Array.reduce(
    Belt.Array.make(8 + 1, 0->Int64.of_int),
    (acc, value) => {
      acc[value->Int64.to_int] =
        acc[value->Int64.to_int]->Int64.add(1->Int64.of_int);
      acc;
    },
  );

let part1 = (octalArray, start, finish) => {
  tick(octalArray, start, 0, finish);
};

let part2 = (octalArray, start, finish) => {
  tick(octalArray, start, 0, finish);
};

let input = readInput()->process;
let startFish = input->Belt.Array.length->Int64.of_int;
let initialOctalArray = input->arrayToOctalArray;

Js.log("6_1. " ++ Int64.to_string(part1(initialOctalArray, startFish, 80)));
Js.log(
  "6_2. " ++ Int64.to_string(part2(initialOctalArray, startFish, 256)),
);
