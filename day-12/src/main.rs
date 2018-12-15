use std::collections::VecDeque;
use std::io::{Read, stdin};
use std::mem;

const GENERATIONS: usize = 20;

fn main() {
  let mut input = String::new();
  stdin().read_to_string(&mut input).unwrap();

  let parsed = Game::parse(&input);
  let mut pots = parsed.initial_state;
  let mut pots2 = pots.clone();
  let rules = parsed.rules;

  for _ in 0 .. GENERATIONS {
    // account for left and right paddings

    for pot in &pots {
      print!("{}", if pot.1 == Pot::Plant { '#' } else { '.' });
    }

    println!("");

    for i in 0 .. 5 {
      if pots[i].1 == Pot::Plant {
        for _ in 0 .. 5 - i {
          pots.push_front((pots[0].0 - 1, Pot::Empty));
          pots2.push_front((pots2[0].0 - 1, Pot::Empty));
        }

        break;
      }
    }

    for i in 0 .. 5 {
      if pots[pots.len() - 1 - i].1 == Pot::Plant {
        for _ in 0 .. 5 - i {
          pots.push_back((pots[pots.len() - 1].0 + 1, Pot::Empty));
          pots2.push_back((pots2[pots2.len() - 1].0 + 1, Pot::Empty));
        }

        break;
      }
    }

    for i in 2 .. pots.len() - 2 {
      let l2 = pots[i-2];
      let l1 = pots[i-1];
      let c = pots[i];
      let r1 = pots[i+1];
      let r2 = pots[i+2];

      let mut found = false;

      for rule in &rules {
        if let Some(new_pot) = Pot::apply_rule(l2.1, l1.1, c.1, r1.1, r2.1, &rule.0, rule.1) {
          pots2[i].1 = new_pot;
          found = true;
          break;
        }
      }

      if !found {
        pots2[i].1 = Pot::Empty;
      }
    }

    mem::swap(&mut pots, &mut pots2);
  }

  let score: i64 = pots.iter().map(|x| if x.1 == Pot::Plant { x.0 } else { 0 }).sum();
  println!("score: {}", score);
}

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
  initial_state: VecDeque<(i64, Pot)>,
  rules: VecDeque<([Pot; 5], Pot)>,
}

impl Game {
  fn parse(s: &str) -> Self {
    let mut lines = s.lines();
    let initial_state = lines
      .next().unwrap()
      .chars().skip("initial state: ".len())
      .enumerate()
      .map(|(i, c)| (i as i64, Pot::from_char(c))).collect();

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
