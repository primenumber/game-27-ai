use crate::game27::*;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Input {
    Init(usize),
    PlayedMove(usize, usize),
    PlayedPass,
    Res(isize),
    Wait,
}
impl Input {
    fn parse(input: &str) -> Result<Input, String> {
        let v: Vec<&str> = input.split(" ").collect();
        match v[0] {
            "init" => Ok(Input::Init(v[1].parse().unwrap())),
            "played" => match v[1] {
                "move" => Ok(Input::PlayedMove(
                    v[2].parse().unwrap(),
                    v[3].parse().unwrap(),
                )),
                "pass" => Ok(Input::PlayedPass),
                _ => Err(format!("Unknown Input: {:?}", v)),
            },
            "result" => Ok(Input::Res(v[1].parse().unwrap())),
            "wait" => Ok(Input::Wait),
            _ => Err(format!("Unknown Input: {:?}", v)),
        }
    }
    
}

#[derive(Debug, Clone)]
struct RandomPlayer {
    board: Option<Game27>,
    first: bool,
}
impl RandomPlayer {
    fn new() -> RandomPlayer {
        RandomPlayer {
            board: None,
            first: false,
        }
    }
    fn play(&mut self, input: &str) -> Option<String> {
        let i = Input::parse(input).unwrap();
        match i {
            Input::Init(p) => {
                if p == 0 {
                    self.first = true
                } else {
                    self.first = false
                }
                self.board = Some(Game27::new());
                None
            }
            Input::PlayedMove(c, i) => {
                let b = self.board.as_mut().unwrap();
                b.act(Action::Move(c, i)).unwrap();
                None
            }
            Input::PlayedPass => {
                let b = self.board.as_mut().unwrap();
                b.act(Action::Pass).unwrap();
                None
            }
            Input::Res(_) => None,
            Input::Wait => {
                use rand::thread_rng;
                use rand::seq::SliceRandom;
                let b = self.board.as_mut().unwrap();
                let p = b.playable();
                let mut rng = thread_rng();
                let a = p.choose(&mut rng).unwrap();
                b.act(*a).unwrap();
                match a {
                    Action::Move(y, x) => Some(format!("move {} {}", y, x)),
                    Action::Pass => Some(format!("pass"))
                }
            }
        }
    }
}

use std::cmp::{max, min};

#[derive(Debug, Clone)]
struct AlphaBetaPlayer<T: TGame27> {
    board: Option<T>,
    first: bool,
    memo: HashMap<T, (isize, isize)>,
}
impl<T: TGame27> AlphaBetaPlayer<T> {
    fn new() -> AlphaBetaPlayer<T> {
        AlphaBetaPlayer {
            board: None,
            first: false,
            memo: HashMap::<T, (isize, isize)>::new(),
        }
    }
    fn eval_impl(&self, b: &T) -> isize {
        if b.is_end() {
            return b.result() * 100;
        }
        return b.result() * 100;
    }
    fn eval(&self, b: &T) -> isize {
        if b.active() == Piece::First {
            self.eval_impl(b)
        } else {
            -self.eval_impl(b)
        }
    }
    fn alpha_beta_impl(&mut self, b: &T, mut alpha: isize, beta: isize, depth: isize) -> isize {
        let mut result = -10000;
        let vp = b.playable();
        for p in vp {
            let mut next = b.clone();
            next.act(p).unwrap();
            let next_depth = match p {
                Action::Pass => depth,
                _ => depth-1,
            };
            let child_result = -self.alpha_beta(&next, -beta, -alpha, next_depth);
            result = max(result, child_result);
            if result >= beta {
                return result;
            }
            alpha = max(alpha, result);
        }
        result
    }
    fn alpha_beta(&mut self, b: &T, alpha: isize, beta: isize, depth: isize) -> isize {
        if depth == 0 || b.is_end() {
            return self.eval(b);
        }
        let (lower, upper) = match self.memo.get(b) {
            Some(&(lower, upper)) => {
                if alpha >= upper {
                    return upper;
                } else if beta <= lower {
                    return lower;
                }
                (lower, upper)
            }
            None => (-1800, 1800)
        };
        let new_alpha = max(alpha, lower);
        let new_beta = min(beta, upper);
        let result = self.alpha_beta_impl(b, new_alpha, new_beta, depth);
        let new_range = if result <= new_alpha {
            (-1800, result)
        } else if result >= new_beta {
            (result, 1800)
        } else {
            (result, result)
        };
        let new_lower = max(lower, new_range.0);
        let new_upper = min(upper, new_range.1);
        self.memo.insert(b.clone(), (new_lower, new_upper));
        result
    }
    fn alpha_beta_root(&mut self, b: &T) -> Action {
        use rand::prelude::*;
        use std::time::Instant;
        let start = Instant::now();
        let mut action = None;
        let time_limit = 800; // as milliseconds
        for depth in 8..40 { // iterative deepening
            self.memo.clear();
            let mut result = -10000;
            let mut alpha = -1800;
            let mut rng = rand::thread_rng();
            let mut same_count = 0;
            for p in b.playable() {
                let mut next = b.clone();
                next.act(p).unwrap();
                let next_depth = match p {
                    Action::Pass => depth,
                    _ => depth-1,
                };
                let child_result = -self.alpha_beta(&next, -1800, -alpha, next_depth);
                if child_result >= result {
                    if child_result == result {
                        same_count += 1;
                        if rng.gen::<f64>() <= 1.0 / same_count as f64 {
                            action = Some(p);
                        }
                    } else {
                        result = child_result;
                        same_count = 0;
                        action = Some(p);
                    }
                    alpha = max(alpha, child_result);
                }
            }
            let end = start.elapsed();
            if end.as_millis() > time_limit {
                break;
            }
        }
        action.unwrap()
    }
    fn play(&mut self, input: &str) -> Option<String> {
        let i = Input::parse(input).unwrap();
        match i {
            Input::Init(p) => {
                if p == 0 {
                    self.first = true
                } else {
                    self.first = false
                }
                self.board = Some(T::new());
                None
            }
            Input::PlayedMove(c, i) => {
                let b = self.board.as_mut().unwrap();
                b.act(Action::Move(c, i)).unwrap();
                None
            }
            Input::PlayedPass => {
                let b = self.board.as_mut().unwrap();
                b.act(Action::Pass).unwrap();
                None
            }
            Input::Res(_) => None,
            Input::Wait => {
                let tmp = self.board.as_ref().unwrap().clone();
                let a = self.alpha_beta_root(&tmp);
                let b = self.board.as_mut().unwrap();
                b.act(a).unwrap();
                match a {
                    Action::Move(y, x) => Some(format!("move {} {}", y, x)),
                    Action::Pass => Some(format!("pass"))
                }
            }
        }
    }
}
pub fn start() -> Result<(), String> {
    use std::io::{self, BufRead};
    let mut player = AlphaBetaPlayer::<Game27Opt>::new();
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let resp = player.play(&line.unwrap());
        if let Some(resp) = resp {
            println!("{}", resp);
        }
    }
    Ok(())
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_player() {
        let mut p0 = RandomPlayer::new();
        let mut p1 = RandomPlayer::new();
        p0.play("init 0");
        p1.play("init 1");
        for _ in 0..100 {
            println!("{:?}", p0.board.as_mut().unwrap().playable());
            let r = p0.play("wait").unwrap();
            println!("{:?}", r);
            let p = Action::parse(&r).unwrap();
            p1.board.as_mut().unwrap().act(p).unwrap();
            println!("X\n{:?}", p0.board.as_mut().unwrap());
            if p1.board.as_mut().unwrap().is_end() {
                break;
            }

            println!("{:?}", p1.board.as_mut().unwrap().playable());
            let r = p1.play("wait").unwrap();
            println!("{:?}", r);
            let p = Action::parse(&r).unwrap();
            p0.board.as_mut().unwrap().act(p).unwrap();
            println!("O\n{:?}", p1.board.as_mut().unwrap());
            if p0.board.as_mut().unwrap().is_end() {
                break;
            }
        }
        let result = p1.board.as_mut().unwrap().result();
        println!("Result: {}", result)
    }
}
