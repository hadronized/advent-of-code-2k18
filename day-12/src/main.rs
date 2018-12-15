use std::collections::{BTreeMap, VecDeque};
use std::io::{Read, stdin};
use std::mem;

const GENERATIONS: usize = 50000000000;
const THREADS_NB: usize = 8;

fn main() {
  let mut input = String::new();
  stdin().read_to_string(&mut input).unwrap();

  let parsed = Game::parse(&input);
  let mut pots = parsed.initial_state;
  let mut pots2 = pots.clone();
  let rules = parsed.rules;

  for gen in 0 .. GENERATIONS {
    println!("generation {}", gen);

    // account for left and right paddings

    //for pot in &pots {
    //  print!("{}", if pot.1 == Pot::Plant { '#' } else { '.' });
    //}

    //println!("");

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

      if let Some(new_pot) = rules.get(&[l2.1, l1.1, c.1, r1.1, r2.1]) {
        pots2[i].1 = *new_pot;
      } else {
        pots2[i].1 = Pot::Empty;
      }
    }

    mem::swap(&mut pots, &mut pots2);
  }

  let score: i64 = pots.iter().map(|x| if x.1 == Pot::Plant { x.0 } else { 0 }).sum();
  println!("score: {}", score);
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
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
}

#[derive(Debug)]
struct Game {
  initial_state: VecDeque<(i64, Pot)>,
  //rules: Vec<([Pot; 5], Pot)>,
  rules: BTreeMap<[Pot; 5], Pot>
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
