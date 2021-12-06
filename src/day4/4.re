exception IndexOutOfBounds;

module Bingo = {
  module Value = {
    type t =
      | Touched(string)
      | UnTouched(string);
    let unbox =
      fun
      | UnTouched(str) => str
      | Touched(str) => str;

    let touch =
      fun
      | UnTouched(str) => Touched(str)
      | a => a;

    let check = (value, reference) =>
      value->unbox == reference ? touch(value) : value;

    let isTouched =
      fun
      | Touched(_) => true
      | UnTouched(_) => false;

    let isNotTouched =
      fun
      | Touched(_) => false
      | UnTouched(_) => true;

    let make = v => UnTouched(v);
  };

  module Line = {
    type t = array(Value.t);

    let touch = (value, line) =>
      line->Belt.Array.map(v => v->Value.unbox == value ? Value.touch(v) : v);

    let check = line => line->Belt.Array.every(Value.isTouched);

    let fromColum = (matrix, index) => {
      matrix->Belt.Array.map(Belt.Array.getUnsafe(_, index));
    };
  };

  module Matrix = {
    type t = array(Line.t);

    let touch = (v, matrix) => {
      matrix->Belt.Array.map(Line.touch(v));
    };
  };

  type t =
    | Win(Matrix.t, Line.t)
    | NoWin(Matrix.t);

  let isWin =
    fun
    | Win(_) => true
    | NoWin(_) => false;

  let unboxWin = win =>
    switch (win) {
    | Win(matrix, values) => (matrix, values->Belt.Array.map(Value.unbox))
    | NoWin(matrix) => (matrix, [||])
    };

  let checkMatrix = (matrix: array(Line.t)): t => {
    let thereIsLines =
      matrix
      ->Belt.Array.map(line =>
          Line.check(line) ? Win(matrix, line) : NoWin(matrix)
        )
      ->Belt.Array.keep(isWin);

    let transposedMatrix =
      matrix->Belt.Array.mapWithIndex((index, _) =>
        Line.fromColum(matrix, index)
      );

    let thereIsColumns =
      transposedMatrix
      ->Belt.Array.map(line =>
          Line.check(line) ? Win(matrix, line) : NoWin(matrix)
        )
      ->Belt.Array.keep(isWin);

    switch (
      thereIsLines->Belt.Array.get(0),
      thereIsColumns->Belt.Array.get(0),
    ) {
    | (Some(Win(matrix, value)), _) => Win(matrix, value)
    | (_, Some(Win(matrix, value))) => Win(matrix, value)
    | (_, _) => NoWin(matrix)
    };
  };
};

open Bingo;

let parser = () => {
  Sys.argv->Belt.Array.sliceToEnd(-1)->Belt.Array.get(0)
  |> Belt.Option.getUnsafe
  |> Node.Fs.readFileAsUtf8Sync
  |> Js.String.split("\n\n");
};

let processLine = line =>
  line
  |> Js.String.split(" ")
  |> Js.Array.filter(v => v != "")
  |> Js.Array.map(Value.make);
let processMatrix = matrix =>
  matrix |> Js.String.split("\n") |> Js.Array.map(processLine);

let processInput = (input: array(Js.String.t)) => {
  let winners = input |> Js.Array.unsafe_get(_, 0) |> Js.String.split(",");
  let tables = input |> Js.Array.sliceFrom(1) |> Js.Array.map(processMatrix);
  (winners, tables);
};

let calculateNumber = ((index, line), winners) => {
  switch (line) {
  | Win(matrix, _) =>
    winners->Belt.Array.getUnsafe(index)->int_of_string
    * matrix->Belt.Array.reduce(0, (acc, line) => {
        acc
        + line
          ->Belt.Array.keep(Value.isNotTouched)
          ->Belt.Array.map(Value.unbox)
          ->Belt.Array.map(int_of_string)
          ->Belt.Array.reduce(0, (+))
      })
  | _ => (-1)
  };
};

let part1 = (winners, tables) => {
  let rec iteration =
          (numbers: array(string), boards: array(Matrix.t), index) => {
    let boards =
      boards->Belt.Array.map(
        Matrix.touch(numbers->Belt.Array.getUnsafe(index), _),
      );
    let boardsWon =
      boards->Belt.Array.map(checkMatrix)->Belt.Array.keep(isWin);

    switch (boardsWon->Belt.Array.get(0)) {
    | Some(line) => (index, line)
    | _ => iteration(numbers, boards, index + 1)
    };
  };

  iteration(winners, tables, 0)->calculateNumber(winners);
};

let part2 = (winners, tables) => {
  let rec iteration =
          (numbers: array(string), boards: array(Matrix.t), index) => {
    let (boardWin, boardsLost) =
      boards
      ->Belt.Array.map(
          Matrix.touch(numbers->Belt.Array.getUnsafe(index), _),
        )
      ->Belt.Array.map(checkMatrix)
      ->Belt.Array.partition(isWin);

    switch (boardsLost->Belt.Array.length) {
    | 0 => (index, boardWin->Belt.Array.getUnsafe(0))
    | _ =>
      iteration(
        numbers,
        boardsLost->Belt.Array.map(unboxWin)->Belt.Array.map(fst),
        index + 1,
      )
    };
  };

  iteration(winners, tables, 0)->calculateNumber(winners);
};

let (winners, tables) = parser()->processInput;
Js.log("4_1. " ++ string_of_int(part1(winners, tables)));
Js.log("4_2. " ++ string_of_int(part2(winners, tables)));
