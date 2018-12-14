use std::io::{Read, stdin};
use std::iter::repeat;
use std::mem;

const PADDING: usize = 20;

fn main() {
  let mut input = String::new();
  stdin().read_to_string(&mut input).unwrap();

  let parsed = Game::parse(&input);
  let mut pots = parsed.initial_state;
  let mut pots2 = pots.clone();
  let rules = parsed.rules;

  for _ in 0 .. 20 {
    for i in 2 .. pots.len() - 2 {
      let l2 = pots[i-2];
      let l1 = pots[i-1];
      let c = pots[i];
      let r1 = pots[i+1];
      let r2 = pots[i+2];

      let mut found = false;

      for rule in &rules {
        if let Some(new_pot) = Pot::apply_rule(l2, l1, c, r1, r2, &rule.0, rule.1) {
          pots2[i] = new_pot;
          found = true;
          break;
        }
      }

      if !found {
        pots2[i] = Pot::Empty;
      }
    }

    mem::swap(&mut pots, &mut pots2);
  }

  let score: isize = pots.iter().enumerate().map(|(i, &x)| if x == Pot::Plant { i as isize - PADDING as isize } else { 0 }).sum();
  println!("score: {}", score);
}

// part 1

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Pot {
  Empty,
  Plant
}

impl Pot {
  fn from_char(c: char) -> Pot {
    if c == '#' {
      Pot::Plant
    } else {
      Pot::Empty
    }
  }

  fn apply_rule(
    l2: Self, l1: Self,
    c: Self,
    r1: Self, r2: Self,
    rule: &[Self; 5],
    mutation: Self
  ) -> Option<Self> {
    let x = [l2, l1, c, r1, r2];

    if x == *rule {
      Some(mutation)
    } else {
      None
    }
  }
}

#[derive(Debug)]
struct Game {
  initial_state: Vec<Pot>,
  rules: Vec<([Pot; 5], Pot)>
}

impl Game {
  fn parse(s: &str) -> Self {
    let mut lines = s.lines();
    let initial_state = lines.next().unwrap().chars().skip("initial state: ".len()).map(Pot::from_char);
    let initial_state = repeat(Pot::Empty).take(PADDING).chain(initial_state).chain(repeat(Pot::Empty).take(PADDING)).collect();

    let _ = lines.next(); // remove the empty line

    let rules = lines.map(|line| {
      let mut iter = line.split(" => ");
      let left: Vec<_> = iter.next().unwrap().chars().map(Pot::from_char).collect();
      let left = [left[0], left[1], left[2], left[3], left[4]];
      let right = Pot::from_char(iter.next().unwrap().chars().next().unwrap());

      (left, right)
    }).collect();

    Self { initial_state, rules }
  }
}
