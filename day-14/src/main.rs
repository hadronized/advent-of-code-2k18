const RECIPES_NB: usize = 9;
const SLICE_TO_SEARCH: &[usize] = &[1, 6, 5, 0, 6, 1];
//const SLICE_TO_SEARCH: &[usize] = &[5, 9, 4, 1, 4];

fn main() {
  let mut scoreboard = vec![3, 7];
  let mut henry = (0, 3);
  let mut robert = (1, 7);

  // part 1
  create_recipes(RECIPES_NB, &mut scoreboard, &mut henry, &mut robert);
  create_recipes(RECIPES_NB + 10, &mut scoreboard, &mut henry, &mut robert);
  println!("{:?} ", &scoreboard[RECIPES_NB .. RECIPES_NB + 10]);

  // part 2
  scoreboard = vec![3, 7];
  henry = (0, 3);
  robert = (1, 7);

  find_lefts(&mut scoreboard, &mut henry, &mut robert);
  println!("left: {:?} recipes", scoreboard.len() - SLICE_TO_SEARCH.len());
}

fn create_recipes(recipes_nb: usize, scoreboard: &mut Vec<usize>, henry: &mut (usize, usize), robert: &mut (usize, usize)) {
  let mut created = scoreboard.len();

  while created < recipes_nb {
    let (x, y) = create_new_recipe(henry.1, robert.1);

    created += 1;
    if let Some(x) = x {
      scoreboard.push(x);
      created += 1;
    }

    scoreboard.push(y);

    // update positions and current recipes
    henry.0 = (henry.0 + henry.1 + 1) % scoreboard.len();
    henry.1 = scoreboard[henry.0];

    robert.0 = (robert.0 + robert.1 + 1) % scoreboard.len();
    robert.1 = scoreboard[robert.0];
  }
}

fn find_lefts(scoreboard: &mut Vec<usize>, henry: &mut (usize, usize), robert: &mut (usize, usize)) {
  let mut c = 0;

  loop {
    let (x, y) = create_new_recipe(henry.1, robert.1);

    if let Some(x) = x {
      scoreboard.push(x);
      c += 1;
    }

    if scoreboard.ends_with(SLICE_TO_SEARCH) {
      break;
    }

    scoreboard.push(y);
    c += 1;

    // update positions and current recipes
    henry.0 = (henry.0 + henry.1 + 1) % scoreboard.len();
    henry.1 = scoreboard[henry.0];

    robert.0 = (robert.0 + robert.1 + 1) % scoreboard.len();
    robert.1 = scoreboard[robert.0];

    if scoreboard.ends_with(SLICE_TO_SEARCH) {
      break;
    }
  }

  println!("finished with {} recipes", c);
}

fn create_new_recipe(a: usize, b: usize) -> (Option<usize>, usize) {
  let s = a + b;
  let x = s / 10;
  let y = s % 10;

  (if x == 1 { Some(x) } else { None }, y)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn new_recipes() {
    assert_eq!(create_new_recipe(0, 0), (None, 0));
    assert_eq!(create_new_recipe(1, 0), (None, 1));
    assert_eq!(create_new_recipe(0, 1), (None, 1));
    assert_eq!(create_new_recipe(4, 5), (None, 9));
    assert_eq!(create_new_recipe(5, 4), (None, 9));
    assert_eq!(create_new_recipe(5, 5), (Some(1), 0));
    assert_eq!(create_new_recipe(9, 9), (Some(1), 8));
  }
}
