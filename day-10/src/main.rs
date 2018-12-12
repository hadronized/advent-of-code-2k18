use std::io::Read;
use std::i64;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Point {
  position: (i64, i64),
  velocity: (i64, i64)
}

fn parse_point_pair(input: &str) -> (i64, i64) {
  // drop the < and >
  let input = &input[1 .. input.len() - 1];
  let mut pair = input.split(',');

  (pair.next().unwrap().trim().parse().unwrap(), pair.next().unwrap().trim().parse().unwrap())
}

fn parse_point(input: &str) -> Point {
  // cut in half
  let mut split = input.split(" velocity=");

  let position = parse_point_pair(&split.next().unwrap()[9..]);
  let velocity = parse_point_pair(split.next().unwrap());

  Point { position, velocity }
}

/// Move points according to their fucking goddamm velocity.
fn fucking_move_fucking_points_fucking_faster<'a, P>(points: P, k: i64) where P: IntoIterator<Item = &'a mut Point> {
  for point in points {
    point.position = (point.position.0 + point.velocity.0 * k, point.position.1 + point.velocity.1 * k)
  }
}

// part 1

/// Our AABB, used to wrap all points at a given time.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct AABB {
  lower: (i64, i64),
  upper: (i64, i64)
}

impl AABB {
  fn new(p: Point) -> Self {
    AABB {
      lower: p.position,
      upper: p.position
    }
  }

  fn from_points<'a, P>(points: P) -> Self where P: IntoIterator<Item = &'a Point> {
    let mut iter = points.into_iter();
    let first = iter.next().unwrap();

    iter.fold(Self::new(*first), Self::accum)
  }

  /// Accumulate an AABB with a point.
  fn accum(mut self, p: &Point) -> Self {
    self.lower.0 = self.lower.0.min(p.position.0);
    self.lower.1 = self.lower.1.min(p.position.1);

    self.upper.0 = self.upper.0.max(p.position.0);
    self.upper.1 = self.upper.1.max(p.position.1);

    self
  }

  /// Dimensions.
  fn dims(&self) -> (i64, i64) {
    ((self.upper.0 - self.lower.0), (self.upper.1 - self.lower.1))
  }

  // /// Area (used to find the 10011 below)
  // fn area(&self) -> i64 {
  //   let (w, h) = self.dims();
  //   w * h
  // }
}

fn main() {
  let mut input = String::new();
  let _ = std::io::stdin().read_to_string(&mut input);

  let input_lines: Vec<_> = input.lines().collect();
  let mut points: Vec<_> = input_lines.iter().map(|&l| parse_point(l)).collect();

  fucking_move_fucking_points_fucking_faster(&mut points, 10011);

  let aabb = AABB::from_points(&points);
  let (w, h) = aabb.dims();

  let mut lol = vec!['.'; w as usize * h as usize];

  for p in points {
    let x = (p.position.0 - aabb.lower.0) * (w - 1) / w;
    let y = (p.position.1 - aabb.lower.1) * (h - 1) / h;
    let o = x + y * w;

    lol[o as usize] = '#';
  }

  for row in 0 .. h {
    for col in 0 .. w {
      print!("{}", lol[(col + row * w) as usize]);
    }

    println!("");
  }
}
