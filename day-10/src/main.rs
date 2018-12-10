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

// part 1

// First thing to do is to compute the AABB in order to get dimensions of the input space. With that
// in hands, we can shrink the vector space into an acceptable N² space (we’ll just ditch Z² because
// fuck negative integers).

const SHRUNK_WIDTH: i32 = 120;

/// Compute shrunk dimension. These will respect the input ration and have the new width set to
/// SHRUNK_WIDTH.
fn shrink_dim(width: i32, height: i32) -> (i32, i32) {
  let new_height = width as f32 * (height as f32) / (SHRUNK_WIDTH as f32);
  (SHRUNK_WIDTH, new_height as i32)
}

/// Our AABB, used to wrap all points at a given time.
struct AABB {
  lower: (i32, i32),
  upper: (i32, i32)
}

impl AABB {
  fn new(p: Point) -> Self {
    AABB {
      lower: p.position,
      upper: p.position
    }
  }

  fn from_points<P>(points: P) -> Self where P: IntoIterator<Item = Point> {
    let mut iter = points.into_iter();
    let first = iter.next().unwrap();

    iter.fold(Self::new(first), Self::accum)
  }

  /// Accumulate an AABB with a point.
  fn accum(mut self, p: Point) -> Self {
    self.lower.0 = self.lower.0.min(p.position.0);
    self.lower.1 = self.lower.0.min(p.position.1);

    self.upper.0 = self.upper.0.max(p.position.0);
    self.upper.1 = self.upper.0.max(p.position.1);

    self
  }

  /// Get current dimension.
  fn dim(&self) -> (i32, i32) {
    (self.upper.0 - self.lower.0, self.upper.1 - self.lower.1)
  }

  /// Transform a point from free coordinates to origin-based ones ([0; width), [0; height)).
  fn normalize_point(&self, p: &Point) -> Point {
    let x = p.position.0 + self.lower.0;
    let y = height - p.position.1 + self.lower.1;

    Point { position: (x, y), ..*p }
  }

  // /// Shrink the AABB into our solution vector space.
  // fn to_solution_space(self) -> Self {
  //   // compute the current dimensions
  //   let (width, height) = (self.upper.0 - self.lower.0, self.upper.1 - self.lower.1);
  //   // compute the new dimensions
  //   let (new_width, new_height) = shrink_dim(width, height);
  // }
}

//fn fill_ascii(ascii: &mut Vec<char>, aabb: AABB, width: i32, height: i32, points: &Vec<Point>) {
//  let minx = (aabb.0).0;
//  let miny = (aabb.0).1;
//
//  println!("wat");
//  ascii.resize((width * height) as usize, '.');
//
//  for p in points {
//    let (col, line) = p.position;
//    println!("col: {}, line: {}", col, line);
//    ascii[(col + minx + width * (line + miny)) as usize] = '#';
//  }
//}

fn main() {
  let mut input = String::new();
  std::io::stdin().read_to_string(&mut input);

  let input_lines: Vec<_> = input.lines().collect();
  let mut points: Vec<_> = input_lines.iter().map(|&l| parse_point(l)).collect();
}
