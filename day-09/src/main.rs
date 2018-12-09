use std::collections::LinkedList;

const PLAYERS_NB: usize = 426;
const MAX_MARBLES: isize = 72058 * 100;

struct Zipper {
  left: LinkedList<isize>,
  middle: isize,
  right: LinkedList<isize>,
}

impl Zipper {
  fn new() -> Self {
    Zipper {
      left: LinkedList::new(),
      middle: 0,
      right: LinkedList::new()
    }
  }

  fn len(&self) -> usize {
    self.left.len() + self.right.len() + 1
  }

  fn focus(&self) -> isize {
    self.middle
  }

  fn insert(&mut self, x: isize) {
    self.right.push_front(self.middle);
    self.middle = x;
  }

  fn remove(&mut self) {
    if self.right.is_empty() {
      self.middle = self.left.pop_back().unwrap();
    } else {
      self.middle = self.right.pop_front().unwrap();
    }
  }

  fn move_by(mut self, offset: isize) -> Self {
    if offset < 0 {
      if self.left.is_empty() {
        let left = self.left;
        self.right.push_front(self.middle);

        self.left = self.right;
        self.middle = self.left.pop_back().unwrap();
        self.right = left;
      } else {
        self.right.push_front(self.middle);
        self.middle = self.left.pop_back().unwrap();
      }

      self.move_by(offset + 1)
    } else if offset > 0 {
      if self.right.is_empty() {
        let right = self.right;
        self.left.push_back(self.middle);

        self.right = self.left;
        self.middle = self.right.pop_front().unwrap();
        self.left = right;
      } else {
        self.left.push_back(self.middle);
        self.middle = self.right.pop_front().unwrap();
      }

      self.move_by(offset - 1)
    } else {
      self
    }
  }
}

fn main() {
  let mut players = vec![0; PLAYERS_NB];
  let mut game = Zipper::new();
  let mut marble = 1;
  let mut index = 1;
  let mut player = 0;

  loop {
    if marble > MAX_MARBLES {
      break;
    }

    if marble % 23 == 0 {
      game = game.move_by(-7);
      let marble_m7 = game.focus();
      game.remove();

      players[player] += marble + marble_m7;
    } else {
      game = game.move_by(2);
      game.insert(marble);
    }

    marble += 1;

    player = (player + 1) % PLAYERS_NB;
  }

  println!("winning score: {:?}", players.iter().max());
}
