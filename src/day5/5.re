let getArrayMax = Belt.Array.reduce(_, 0, (acc, v) => {v > acc ? v : acc});
let flatten = Belt.Array.reduce(_, [||], Belt.Array.concat);

module Point = {
  type t = (int, int);
  let transpose = ((x, y)) => (y, x);
};

module Line = {
  type t = (Point.t, Point.t);

  type direction =
    | Horizontal
    | Vertical
    | Diagonal;

  let direction =
    fun
    | ((_, y1), (_, y2)) when y1 == y2 => Horizontal
    | ((x1, _), (x2, _)) when x1 == x2 => Vertical
    | _ => Diagonal;

  let sort = (((x1, y1), (x2, y2)): t) => {
    x1 > x2 ? ((x2, y2), (x1, y1)) : ((x1, y1), (x2, y2));
  };

  let toArray = (((x1, y1), (x2, y2)): t) => [|x1, x2, y1, y2|];
  let getMax = t => t->toArray->getArrayMax;

  let make = (x1, y1, x2, y2) => ((x1, y1), (x2, y2));
  let transpose = ((p1, p2)) => (p1->Point.transpose, p2->Point.transpose);

  let getLongestDistance = (((x1, y1), (x2, y2)): t) => {
    let lengthX = x2 - x1 < 0 ? x1 - x2 : x2 - x1;
    let lengthY = y2 - y1 < 0 ? y1 - y2 : y2 - y1;

    lengthX > lengthY ? lengthX : lengthY;
  };

  let getDirection = (((x1, y1), (x2, y2)): t) => (
    x2 == x1 ? 0 : x2 - x1 > 0 ? 1 : (-1),
    y2 == y1 ? 0 : y2 - y1 > 0 ? 1 : (-1),
  );

  let toPoints = (((x1, y1), (x2, y2)): t): array(Point.t) => {
    let (opx, opy) = ((x1, y1), (x2, y2))->getDirection;
    let length = ((x1, y1), (x2, y2))->getLongestDistance;

    Belt.Array.make(length, (0, 0))
    ->Belt.Array.reduce(
        [|(x1, y1)|],
        (acc, _) => {
          let (xx, yy) = acc->Belt.Array.reverse->Belt.Array.getUnsafe(0);
          acc->Belt.Array.concat([|(xx + opx, yy + opy)|]);
        },
      );
  };
};

module Matrix = {
  type t = array(array(int));

  let make = size =>
    Belt.Array.make(size, 0)->Belt.Array.map(_ => Belt.Array.make(size, 0));
};

let readInput = () => {
  Sys.argv->Belt.Array.sliceToEnd(-1)->Belt.Array.get(0)
  |> Belt.Option.getUnsafe
  |> Node.Fs.readFileAsUtf8Sync
  |> Js.String.split("\n");
};

let processPoint = v =>
  switch (v |> Js.String.split(",")) {
  | [|x, y|] => (x->int_of_string, y->int_of_string)
  | _ => (0, 0)
  };

let processLine = v =>
  switch (v |> Js.String.split(" -> ")) {
  | [|start, end_|] =>
    let (x1, x2) = start->processPoint;
    let (y1, y2) = end_->processPoint;

    Line.make(x1, x2, y1, y2);
  | _ => Line.make(0, 0, 0, 0)
  };

let process = input => {
  input |> Js.Array.map(processLine);
};

let part1 = (lines, matrix) => {
  lines
  ->Belt.Array.map(line => {
      switch (line->Line.direction) {
      | Horizontal
      | Vertical => line->Line.sort->Line.toPoints
      | Diagonal => [||]
      }
    })
  ->flatten
  ->Belt.Array.reduce(
      matrix,
      (acc, (x, y)) => {
        let value = acc[y][x];
        acc[y][x] = value + 1;
        acc;
      },
    )
  ->Belt.Array.reduce(0, (acc, line) => {
      acc + line->Belt.Array.keep(v => v > 1)->Belt.Array.length
    });
};

let part2 = (lines, matrix) => {
  lines
  ->Belt.Array.map(line => {
      switch (line->Line.direction) {
      | Horizontal
      | Vertical
      | Diagonal => line->Line.sort->Line.toPoints
      }
    })
  ->flatten
  ->Belt.Array.reduce(
      matrix,
      (acc, (x, y)) => {
        let value = acc[y][x];
        acc[y][x] = value + 1;
        acc;
      },
    )
  ->Belt.Array.reduce(0, (acc, line) => {
      acc + line->Belt.Array.keep(v => v > 1)->Belt.Array.length
    });
};

let lines = readInput()->process;
let maxMatrix = lines |> Belt.Array.map(_, Line.getMax) |> getArrayMax;

let matrix = Matrix.make(maxMatrix + 1);
let matrix2 = Matrix.make(maxMatrix + 1);

Js.log("5_1. " ++ string_of_int(part1(lines, matrix)));
Js.log("5_2. " ++ string_of_int(part2(lines, matrix2)));
