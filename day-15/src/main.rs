const ATTACK_DMG: u8 = 3;

/// A block of a map.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Block {
  Wall,
  Free,
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
  fn get_available_destinations(&self, unit_id: UnitId) -> [Option<Pos>; 4] {
    assert!(unit_id < self.units.len());

    let unit_pos = self.units[unit_id].1;

    [
      self.north_of(&unit_pos).and_then(|p| if self.block_at(&p) == Some(&Block::Free) { Some(p) } else { None }),
      self.east_of(&unit_pos).and_then(|p| if self.block_at(&p) == Some(&Block::Free) { Some(p) } else { None }),
      self.south_of(&unit_pos).and_then(|p| if self.block_at(&p) == Some(&Block::Free) { Some(p) } else { None }),
      self.west_of(&unit_pos).and_then(|p| if self.block_at(&p) == Some(&Block::Free) { Some(p) } else { None }),
    ]
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
  fn in_range_targets(&self, unit_id: UnitId) -> Vec<Pos> {
    assert!(unit_id < self.units.len());

    let mut tgt_list = Vec::new();
    let &(ref current_unit, _) = &self.units[unit_id];

    for (i, (unit, _)) in self.units.iter().enumerate() {
      if i != unit_id {
        if unit.species != current_unit.species {
          tgt_list.extend(self.get_available_destinations(i).iter().flatten());
        }
      }
    }

    tgt_list
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

fn main() {
  println!("Hello, world!");
}
