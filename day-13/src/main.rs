use std::collections::HashMap;
use std::io::{Read, stdin};

fn main() {
  let mut input = String::new();
  stdin().read_to_string(&mut input).unwrap();
  let (map, mut carts) = Map::parse(&input);

  let collision = loop {
    // sort the cart by y component
    carts.sort_by(|a, b| a.pos.cmp(&b.pos));

    for cart in &carts {
      println!("{:?}", cart);
    }
    
    println!("");

    if let Some(collision) = move_carts(&map, &mut carts) {
      break collision;
    }
  };

  println!("Collision at {:?}", collision);
}

// part 1

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Rail {
  Cross, // +
  RampRight, // /
  RampLeft // \
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Direction {
  Left,
  Up,
  Right,
  Down
}

impl Direction {
  fn turn(&self, turn: Turn) -> Self {
    match turn {
      Turn::Left => {
        match *self {
          Direction::Left => Direction::Down,
          Direction::Up => Direction::Left,
          Direction::Right => Direction::Up,
          Direction::Down => Direction::Right
        }
      }

      Turn::Straight => *self,

      Turn::Right => {
        match *self {
          Direction::Left => Direction::Up,
          Direction::Up => Direction::Right,
          Direction::Right => Direction::Down,
          Direction::Down => Direction::Left
        }
      }
    }
  }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Turn {
  Left,
  Straight,
  Right
}

impl Turn {
  fn next(&self) -> Self {
    match *self {
      Turn::Left => Turn::Straight,
      Turn::Straight => Turn::Right,
      Turn::Right => Turn::Left
    }
  }
}

struct Map(HashMap<(u32, u32), Rail>);

#[derive(Debug)]
struct Cart {
  pos: (u32, u32),
  dir: Direction,
  next_turn: Turn
}

impl Cart {
  fn new(pos: (u32, u32), dir: Direction) -> Self {
    Cart { pos, dir, next_turn: Turn::Left }
  }
}

impl Map {
  fn parse(input: &str) -> (Self, Vec<Cart>) {
    let mut x = 0;
    let mut y = 0;
    let mut map = HashMap::new();
    let mut carts = Vec::new();

    for c in input.chars() {
      match c {
        '\n' => {
          x = 0;
          y += 1;
          continue;
        }

        '/' => {
          map.insert((x, y), Rail::RampRight);
        }

        '\\' => {
          map.insert((x, y), Rail::RampLeft);
        }

        '+' => {
          map.insert((x, y), Rail::Cross);
        }

        '<' => {
          carts.push(Cart::new((x, y), Direction::Left));
        }

        '>' => {
          carts.push(Cart::new((x, y), Direction::Right));
        }

        '^' => {
          carts.push(Cart::new((x, y), Direction::Up));
        }

        'v' => {
          carts.push(Cart::new((x, y), Direction::Down));
        }

        _ => ()
      }

      x += 1;
    }

    (Map(map), carts)
  }
}

/// Move all carts one by one and return any collision.
fn move_carts(
  map: &Map,
  carts: &mut Vec<Cart>,
) -> Option<(u32, u32)> {
  for i in 0 .. carts.len() {
    move_cart(map, &mut carts[i]);
    let collision = find_collision(&carts, i);

    if collision.is_some() {
      return collision;
    }
  }

  None
}

/// Move a cart according to its current direction and position on a given map.
///
/// This function will also change the direction of the cart if needed, according to the situation
/// on the map.
fn move_cart(map: &Map, cart: &mut Cart) {
  let (x, y) = cart.pos;

  let next_pos = match cart.dir {
    Direction::Up => (x, y - 1),
    Direction::Down => (x, y + 1),
    Direction::Left => (x - 1, y),
    Direction::Right => (x + 1, y)
  };

  // change the cart position
  cart.pos = next_pos;

  // change the direction of the cart
  // TODO: try remove the | and -: it should work and take less memory!
  if let Some(rail) = map.0.get(&next_pos) {
    match rail {
      Rail::Cross => {
        // change the direction and change the memory for the turn
        cart.dir = cart.dir.turn(cart.next_turn);
        cart.next_turn = cart.next_turn.next();
      }

      Rail::RampRight => {
        cart.dir = match cart.dir {
          Direction::Left => Direction::Down,
          Direction::Up => Direction::Right,
          Direction::Right => Direction::Up,
          Direction::Down => Direction::Left,
        };
      }

      Rail::RampLeft => {
        cart.dir = match cart.dir {
          Direction::Left => Direction::Up,
          Direction::Up => Direction::Left,
          Direction::Right => Direction::Down,
          Direction::Down => Direction::Right
        }
      }

      _ => ()
    }
  }
}

/// Detect a collision between two carts.
fn find_collision(
  carts: &Vec<Cart>,
  current_index: usize
) -> Option<(u32, u32)> {
  for i in 0 .. carts.len() {
    if i != current_index {
      let cart = &carts[i];

      if cart.pos == carts[current_index].pos {
        println!("collision for i={} (current_pos={}) and pos={:?}", i, current_index, cart.pos);
        return Some(cart.pos);
      }
    }
  }

  None
}
