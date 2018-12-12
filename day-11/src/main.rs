const GRID_SERIAL_NUMBER: i64 = 7347;

fn main() {
  let grid: Vec<_> = (0 .. 300 * 300).map(|i| power_level(1 + i % 300, 1 + i / 300, GRID_SERIAL_NUMBER)).collect();
  let mut largest = (0, i8::min_value()); // (index, power)

  for row in 0 .. 298 {
    for col in 0 .. 298 {
      let mut power = 0;

      for i in 0 .. 3 {
        for k in 0 .. 3 {
          power += grid[index(col + i, row + k)];
        }
      }

      let i = index(col, row);

      if (power == largest.1 && i < largest.0) || power > largest.1 {
        largest = (i, power);
      }
    }
  }

  println!("Largest fuel cell: ({}, {})", 1 + largest.0 % 300, 1 + largest.0 / 300);
}

fn index(x: usize, y: usize) -> usize {
  x + y * 300
}

fn power_level(x: u64, y: u64, serial: i64) -> i8 {
  let rack_id = x as i64 + 10;
  let power_level = rack_id * y as i64;
  let power_level = power_level + serial;
  let power_level = power_level * rack_id;
  
  hundred_digit(power_level) as i8 - 5
}

fn hundred_digit(x: i64) -> usize {
  ((x / 100) % 10) as usize
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_hundred_digit() {
    assert_eq!(hundred_digit(0), 0);
    assert_eq!(hundred_digit(99), 0);
    assert_eq!(hundred_digit(100), 1);
    assert_eq!(hundred_digit(200), 2);
    assert_eq!(hundred_digit(1000), 0);
    assert_eq!(hundred_digit(28356), 3);
  }

  #[test]
  fn test_power_levels() {
    assert_eq!(power_level(3, 5, 8), 4);
    assert_eq!(power_level(122, 79, 57), -5);
    assert_eq!(power_level(217, 196, 39), 0);
    assert_eq!(power_level(101, 153, 71), 4);
  }
}
