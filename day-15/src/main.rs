const ATTACK_DMG: u8 = 3;

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

  // /// Get all the possible positions a unit can move to.
  // fn get_available_destinations(&self, unit_id: UnitId) -> [Option<Pos>; 4] {
  //   assert!(unit_id < self.units.len());

  //   let unit_pos = self.units[unit_id].1;
  //   let mut destinations = [None; 4];

  //   if let Some(Block::Free) = self.map.get(unit_pos.north())
  // }

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

/// A block of a map.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Block {
  Wall,
  Free,
  Elf(UnitId),
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

fn main() {
  println!("Hello, world!");
}
