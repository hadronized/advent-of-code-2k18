use std::collections::{HashSet, HashMap, VecDeque};

const MAX_HP: u8 = 200;
const ATTACK_DMG: u8 = 3;

fn main() {
  println!("Hello, world!");
}

/// A block of a map.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Block {
  Wall,
  Empty,
  Elf(UnitId),
  Goblin(UnitId)
}

#[derive(Debug)]
struct Unit {
  hp: u8,
  species: Species
}

impl Unit {
  fn is_elf(&self) -> bool {
    self.species == Species::Elf
  }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Species {
  Elf,
  Goblin
}

type UnitId = usize;

type Pos = (usize, usize);

/// The map, containing walls, open caverns, globins and elves.
#[derive(Debug)]
struct Map {
  width: usize,
  height: usize,
  blocks: Vec<Block>,
  units: Vec<(Unit, Pos)>,
}

impl Map {
  /// Parse the map out of a string.
  fn parse(input: &str) -> Self {
    let mut width = 0;
    let mut height = 0;
    let mut blocks = Vec::new();
    let mut units = Vec::new();

    for line in input.lines() {
      if width == 0 {
        width = line.len();
      }

      for (col, c) in line.chars().enumerate() {
        match c {
          '#' => blocks.push(Block::Wall),

          '.' => blocks.push(Block::Empty),

          'E' => {
            let unit_id = units.len();
            let pos = (col, height);
            units.push((Unit { hp: MAX_HP, species: Species::Elf }, pos));
            blocks.push(Block::Elf(unit_id));
          }

          'G' => {
            let unit_id = units.len();
            let pos = (col, height);
            units.push((Unit { hp: MAX_HP, species: Species::Goblin }, pos));
            blocks.push(Block::Goblin(unit_id));
          }

          _ => panic!("impossible map: {}", c)
        }
      }

      height += 1;
    }

    Map {
      width,
      height,
      blocks,
      units
    }
  }

  /// Tell whether the game is over. If so, return the species that won along with the sum of the
  /// remaining units’ HP.
  fn is_game_over(&self) -> Option<(Species, u32)> {
    let mut elves_hp = 0;
    let mut goblins_hp = 0;

    for &(ref unit, _) in &self.units {
      match unit.species {
        Species::Elf => elves_hp += unit.hp as u32,
        Species::Goblin => goblins_hp += unit.hp as u32
      }
    }

    if elves_hp == 0 {
      Some((Species::Goblin, goblins_hp))
    } else if goblins_hp == 0 {
      Some((Species::Elf, elves_hp))
    } else {
      None
    }
  }

  /// Get the block at a given position.
  fn block_at(&self, p: &Pos) -> Option<&Block> {
    let i = p.0 + p.1 * self.width;
    self.blocks.get(i)
  }

  /// Get all the possible positions a unit can move to.
  fn targets(&self, pos: &Pos) -> Vec<Pos> {
    [
      self.north_of(pos).and_then(|p| if self.block_at(&p) == Some(&Block::Empty) { Some(p) } else { None }),
      self.east_of(pos).and_then(|p| if self.block_at(&p) == Some(&Block::Empty) { Some(p) } else { None }),
      self.south_of(pos).and_then(|p| if self.block_at(&p) == Some(&Block::Empty) { Some(p) } else { None }),
      self.west_of(pos).and_then(|p| if self.block_at(&p) == Some(&Block::Empty) { Some(p) } else { None }),
    ].iter().cloned().flatten().collect()
  }

  /// Get all the adjacent foes that we can hit around us.
  fn get_adjacent_foes(&self, unit_id: UnitId) -> [Option<UnitId>; 4] {
    assert!(unit_id < self.units.len());

    let &(ref unit, unit_pos) = &self.units[unit_id];
    let foe_id = |p| {
      if unit.species == Species::Elf {
        match self.block_at(&p) {
          Some(Block::Goblin(id)) => Some(*id),
          _ => None
        }
      } else {
        match self.block_at(&p) {
          Some(Block::Elf(id)) => Some(*id),
          _ => None
        }
      }
    };

    [
      self.north_of(&unit_pos).and_then(foe_id),
      self.east_of(&unit_pos).and_then(foe_id),
      self.south_of(&unit_pos).and_then(foe_id),
      self.west_of(&unit_pos).and_then(foe_id),
    ]
  }

  /// Get all the in-range target blocks of a given unit. These are all available destinations of
  /// all foes (even unreachable).
  fn in_range_targets(&self, unit_id: UnitId) -> HashSet<Pos> {
    assert!(unit_id < self.units.len());

    let mut tgt_list = HashSet::new();
    let &(ref current_unit, _) = &self.units[unit_id];

    eprintln!("unit_id={:?}, species={:?}", unit_id, current_unit.species);

    for (i, (unit, pos)) in self.units.iter().enumerate() {
      eprintln!("i={:?}, unit={:?}, pos={:?}", i, unit, pos);
      if i != unit_id {
        eprintln!("oui id");
        if unit.species != current_unit.species {
          eprintln!("oui species");
          eprintln!("should add: {:?}", self.targets(pos));
          tgt_list.extend(self.targets(pos));
        }
      }
    }

    tgt_list
  }

  /// Prune a list of in-range target blocks by removing those that are not reachable (i.e. they
  /// would require to go through walls or other units).
  fn prune_unreachable_targets(&self, unit_id: UnitId, targets: &HashSet<Pos>) -> HashSet<Pos> {
    // algorithm: we explore around (without keeping trace of paths) until we cannot discover
    // anything; if we find a target block, we add it to the list of reachable
    assert!(unit_id < self.units.len());

    let mut reachables = HashSet::new();
    let mut visited = HashSet::new();
    let mut visiting = VecDeque::new();

    visiting.push_back(self.units[unit_id].1);

    while !visiting.is_empty() {
      let current = visiting.pop_front().unwrap();
      visited.insert(current);

      // check if this position is reachable; if so, add it to the reachables
      if targets.contains(&current) {
        reachables.insert(current);
      }

      // get all the adjacent position we can go to 
      let around = self.targets(&current);

      // add them to the visiting only if we haven’t visited them already and that they’re don’t
      // contain a unit
      for p in &around {
        if !visited.contains(p) && self.block_at(p) == Some(&Block::Empty) {
          visiting.push_back(*p);
        }
      }
    }

    reachables
  }

  /// Get the nearest blocks out of a set.
  ///
  /// This function will find the path to the nearest block that can be found in the input set. If
  /// two paths are at the same distance, a rule of thumb is applied to return the one with the
  /// hightest probability.
  fn nearest_targets(&self, start: &Pos, targets: &HashSet<Pos>) {
    // distance table, mapping traversed position to the position that minimizes the distance to go
    // there
    let mut min_table: HashMap<Pos, (usize, Pos)> = HashMap::new();
    let mut visiting = VecDeque::new();
    let mut visited = HashSet::new();

    min_table.insert(*start, (0, *start));
    visiting.push_back(*start);

    while let Some(current) = visiting.pop_front() {
      visited.insert(current);

      let tgts = self.targets(&current).into_iter().filter(|p| !visited.contains(&p));

      for tgt in tgts {
        visiting.push_back(tgt);

        // if that target already has a distance, update only if ours is less
        match (min_table.get(&tgt).cloned(), min_table.get(&current).cloned()) {
          (Some(tgt_min), Some(mut current_min)) if current_min.0 + 1 < tgt_min.0 => {
            current_min.0 += 1;
            min_table.insert(tgt, current_min);
          }

          _ => ()
        }
      }
    }
  }

  fn north_of(&self, p: &Pos) -> Option<Pos> {
    if p.1 == 0 {
      None
    } else {
      Some((p.0, p.1 - 1))
    }
  }

  fn east_of(&self, p: &Pos) -> Option<Pos> {
    if p.0 == self.width - 1 {
      None
    } else {
      Some((p.0 + 1, p.1))
    }
  }

  fn south_of(&self, p: &Pos) -> Option<Pos> {
    if p.1 == self.height - 1 {
      None
    } else {
      Some((p.0, p.1 + 1))
    }
  }

  fn west_of(&self, p: &Pos) -> Option<Pos> {
    if p.0 == 0 {
      None
    } else {
      Some((p.0 - 1, p.1))
    }
  }
}

/// Check whether a unit can move.
fn can_move(destinations: &[Option<Pos>; 4]) -> bool {
  match destinations {
    &[None, None, None, None] => false,
    _ => true
  }
}

/// Check whether a unit can attack someone.
fn can_attack(foes: &[Option<UnitId>; 4]) -> bool {
  match foes {
    &[None, None, None, None] => false,
    _ => true
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn block_at() {
    let input = r#"
#######
#E..G.#
#...#.#
#.G.#G#
#######"#;
    let map = Map::parse(input);

    assert_eq!(map.block_at(&(0, 0)), Some(&Block::Wall));
    assert_eq!(map.block_at(&(1, 1)), Some(&Block::Elf(0)));
    assert_eq!(map.block_at(&(4, 1)), Some(&Block::Goblin(1)));
    assert_eq!(map.block_at(&(5, 3)), Some(&Block::Goblin(3)));
  }

  #[test]
  fn targets() {
    let input = r#"
#######
#E..G.#
#...#.#
#.G.#G#
#######"#;
    let map = Map::parse(input);

    assert_eq!(map.targets(&(1, 1)).len(), 2);
    assert_eq!(map.targets(&(4, 1)).len(), 2);
    assert_eq!(map.targets(&(2, 3)).len(), 3);
    assert_eq!(map.targets(&(5, 3)).len(), 1);
  }

  #[test]
  fn in_range_targets() {
    let input = r#"
#######
#E..G.#
#...#.#
#.G.#G#
#######"#;
    let map = Map::parse(&input[1..]);
    let irt = map.in_range_targets(0);

    assert_eq!(irt.len(), 6);
  }

  #[test]
  fn prune_unreachable_targets() {
    let input = r#"
#######
#E..G.#
#...#.#
#.G.#G#
#######"#;
    let map = Map::parse(&input[1..]);
    let irt = map.in_range_targets(0);
    let pruned = map.prune_unreachable_targets(0, &irt);

    assert_eq!(pruned.len(), 4);
  }
}
