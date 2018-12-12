const GRID_SERIAL_NUMBER: i64 = 7347;

fn main() {
  let grid: Vec<_> = (0 .. 300 * 300).map(|i| power_level(1 + i % 300, 1 + i / 300, GRID_SERIAL_NUMBER)).collect();
  let mut largest = (0, i8::min_value()); // (index, power)

  // part 1
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

  // part 2
  let mut largest2 = (0, i64::min_value(), 0); // (index, power, dimension)

  for row in 0 .. 300 {
    let max_iter_row = 300 - row; // 300 -> 1

    for col in 0 .. 300 {
      let max_iter_col = 300 - col; // 300 -> 1
      let max_dim_squared = max_iter_row.min(max_iter_col); // 300x300 -> 1x1

      // power used for nested dimensions
      let mut nested_power = grid[index(col, row)] as i64;

      for d in 1 .. max_dim_squared {
        let mut power = nested_power;

        for k in 0 .. d {
          power += grid[index(col + d, row + k)] as i64;
          power += grid[index(col + k, row + d)] as i64;
        }

        power += grid[index(col + d, row + d)] as i64;

        let i = index(col, row);

        if (power == largest2.1 && i < largest2.0) || power > largest2.1 {
          largest2 = (index(col, row), power, d);
        }

        nested_power = power;
      }
    }
  }

  println!("Largest fuel cell of all: ({}, {}, {}, of power {})", 1 + largest2.0 % 300, 1 + largest2.0 / 300, largest2.2, largest2.1);
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
