const PLAYERS_NB: usize = 426;
const MAX_MARBLES: isize = 72058 * 100;

fn main() {
  let mut players = vec![0; PLAYERS_NB];
  let mut game = vec![0];
  let mut marble = 1;
  let mut index = 1;
  let mut player = 0;

  loop {
    if marble > MAX_MARBLES {
      break;
    }

    if marble % 23 == 0 {
      index = game_index(index - 7, &game);
      let marble_m7 = game[index as usize];
      game.remove(index as usize);

      players[player] += marble + marble_m7;
    } else {
      index = game_index(index + 2, &game);
      game.insert(index as usize, marble);
    }

    marble += 1;

    player = (player + 1) % PLAYERS_NB;
  }

  println!("{}", -5 % 10);
  println!("winning score: {:?}", players.iter().max());
}

fn game_index(i: isize, game: &Vec<isize>) -> isize {
  if i < 0 {
    game.len() as isize + i
  } else {
    (i as usize % game.len()) as isize
  }
}
