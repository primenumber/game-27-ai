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

trait TEvaluator {
    type Game: TGame27;
    fn new() -> Self;
    fn eval(&self, b: &Self::Game) -> isize;
    fn max_score(&self) -> isize;
}

use std::marker::PhantomData;

#[derive(Debug, Clone)]
struct NaiveEvaluator<T: TGame27> {
    _marker: PhantomData<fn() -> T>,
}
impl<T: TGame27> TEvaluator for NaiveEvaluator<T> {
    type Game = T;
    fn new() -> Self {
        NaiveEvaluator {
            _marker: PhantomData {}
        }
    }
    fn eval(&self, b: &T) -> isize {
        if b.active() == Piece::First {
            b.result()
        } else {
            -b.result()
        }
    }
    fn max_score(&self) -> isize {
        18
    }
}

#[derive(Debug, Clone)]
struct TowerCountEvaluator<T: TGame27> {
    _marker: PhantomData<fn() -> T>,
}
impl<T: TGame27> TowerCountEvaluator<T> {
    fn eval_impl(&self, b: &T) -> isize {
        if b.is_end() {
            return b.result() * 100;
        } else {
            let summary = b.tower_summaries();
            let mut pt: isize = 0;
            for (_len, owner) in &summary {
                match owner {
                    Some(Piece::First) => pt += 1,
                    Some(Piece::Second) => pt -= 1,
                    None => ()
                }
            }
            pt * 20
        }
    }
}
impl<T: TGame27> TEvaluator for TowerCountEvaluator<T> {
    type Game = T;
    fn new() -> Self {
        TowerCountEvaluator {
            _marker: PhantomData {}
        }
    }
    fn eval(&self, b: &T) -> isize {
        if b.active() == Piece::First {
            self.eval_impl(b)
        } else {
            -self.eval_impl(b)
        }
    }
    fn max_score(&self) -> isize {
        18 * 100
    }
}

use std::cmp::{max, min};

#[derive(Debug, Clone)]
struct AlphaBetaPlayer<T: TGame27, E: TEvaluator<Game = T>> {
    board: Option<T>,
    first: bool,
    memo: HashMap<T, (isize, isize)>,
    evaluator: E,
}
impl<T: TGame27, E: TEvaluator<Game = T>> AlphaBetaPlayer<T, E> {
    fn new() -> AlphaBetaPlayer<T, E> {
        AlphaBetaPlayer {
            board: None,
            first: false,
            memo: HashMap::<T, (isize, isize)>::new(),
            evaluator: E::new(),
        }
    }
    fn alpha_beta_impl(&mut self, b: &T, mut alpha: isize, beta: isize, depth: isize) -> isize {
        let mut result = -10000;
        for p in b.playable_generator() {
            let mut next = b.clone();
            next.act(p).unwrap();
            let next_depth = depth - 1;
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
            return self.evaluator.eval(b);
        }
        if depth < 2 {
            return self.alpha_beta_impl(b, alpha, beta, depth);
        }
        let max_score = self.evaluator.max_score();
        let (lower, upper) = match self.memo.get(b) {
            Some(&(lower, upper)) => {
                if alpha >= upper {
                    return upper;
                } else if beta <= lower {
                    return lower;
                }
                (lower, upper)
            }
            None => {
                (-max_score, max_score)
            }
        };
        let new_alpha = max(alpha, lower);
        let new_beta = min(beta, upper);
        let result = self.alpha_beta_impl(b, new_alpha, new_beta, depth);
        let new_range = if result <= new_alpha {
            (-max_score, result)
        } else if result >= new_beta {
            (result, max_score)
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
        let max_score = self.evaluator.max_score();
        let start = Instant::now();
        let mut action = None;
        let time_limit = 800; // as milliseconds
        for depth in 8..40 { // iterative deepening
            self.memo.clear();
            let mut result = -max_score - 1;
            let mut alpha = -max_score;
            let mut rng = rand::thread_rng();
            let mut same_count = 0;
            for p in b.playable() {
                let mut next = b.clone();
                next.act(p).unwrap();
                let next_depth = match p {
                    Action::Pass => depth,
                    _ => depth-1,
                };
                let child_result = -self.alpha_beta(&next, -max_score, -alpha, next_depth);
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
    let mut player = AlphaBetaPlayer::<Game27Opt, TowerCountEvaluator<Game27Opt>>::new();
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
