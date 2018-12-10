#[macro_use]
extern crate nom;

use nom::{digit, eol, newline, space};
use std::io::Read;
use std::i32;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Point {
  position: (i32, i32),
  velocity: (i32, i32)
}

fn parse_point_pair(input: &str) -> (i32, i32) {
  // drop the < and >
  let input = &input[1 .. input.len() - 1];
  let mut pair = input.split(',');

  (pair.next().unwrap().trim().parse().unwrap(), pair.next().unwrap().trim().parse().unwrap())
}

fn parse_point(input: &str) -> Point {
  // cut in half
  let mut split = input.split(" velocity=");

  let position = parse_point_pair(&split.next().unwrap()[10..]);
  let velocity = parse_point_pair(split.next().unwrap());

  Point { position, velocity }
}

type AABB = ((i32, i32), (i32, i32)); // minx, maxx   miny, maxy

fn fill_ascii(ascii: &mut Vec<char>, aabb: AABB, width: i32, height: i32, points: &Vec<Point>) {
  let minx = (aabb.0).0;
  let miny = (aabb.0).1;

  println!("wat");
  ascii.resize((width * height) as usize, '.');

  for p in points {
    let (col, line) = p.position;
    println!("col: {}, line: {}", col, line);
    ascii[(col + minx + width * (line + miny)) as usize] = '#';
  }
}

fn main() {
  let mut input = String::new();
  std::io::stdin().read_to_string(&mut input);

  let input_lines: Vec<_> = input.lines().collect();
  let mut points: Vec<_> = input_lines.iter().map(|&l| parse_point(l)).collect();
  let aabb =
    points.iter()
          .fold(((0, 0), (0, 0)), |((minx, maxx), (miny, maxy)), p|
            ((minx.min(p.position.0), maxx.max(p.position.0)),
             (miny.min(p.position.1), maxy.max(p.position.1)))
          );

  let width = i32::abs((aabb.0).1 - (aabb.0).0);
  let height = i32::abs((aabb.1).1 - (aabb.1).0);

  println!("width {} height {}", width, height);
  let mut ascii = vec!['.'; (width * height) as usize];

  loop {
    fill_ascii(&mut ascii, aabb, width, height, &points);

    let mut i = 0;
    for line in &ascii {
      print!("{}", line);

      i += 1;

      if i >= width {
        println!("");
        i = 0;
      }
    }

    std::io::stdin().read_line(&mut input); // lol
  }
}
