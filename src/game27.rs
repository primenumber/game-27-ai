extern crate rand;
use std::collections::HashMap;
use std::hash::Hash;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Piece {
    First,
    Second,
}
const SIZE: usize = 9;

trait TGame27: Eq + Hash + Clone {
    fn new() -> Self;
    fn playable(&self) -> Vec<Action>;
    fn active(&self) -> Piece;
    fn is_end(&self) -> bool;
    fn act(&mut self, a: Action) -> Result<(), String>;
    fn result(&self) -> isize;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Game27 {
    board: [Vec<Piece>; SIZE],
    first_turn: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Action {
    Move(usize, usize),
    Pass,
}
impl Action {
    #[allow(dead_code)]
    fn parse(s: &str) -> Result<Action, String> {
        let v: Vec<&str> = s.split(" ").collect();
        match v[0] {
            "move" => Ok(Action::Move(v[1].parse().unwrap(), v[2].parse().unwrap())),
            "pass" => Ok(Action::Pass),
            _ => Err(format!("Unknown played: {:?}", v)),
        }
    }
}

impl Game27 {
    fn count_tower(&self) -> usize {
        let mut res = 0;
        for c in 0..SIZE {
            if self.board[c].len() > 0 && self.board[c][0] == self.active() {
                res += 1;
            }
        }
        return res;
    }
    fn move_to(&self, c: usize) -> isize {
        c as isize + self.count_tower() as isize * (if self.first_turn { 1 } else { -1 })
    }
}

impl TGame27 for Game27 {
    fn new() -> Game27 {
        use Piece::*;
        let mut board : [Vec<Piece>; SIZE]= Default::default();
        board[0] = vec![First; SIZE];
        board[SIZE-1] = vec![Second; SIZE];
        Game27 { board, first_turn: true }
    }
    fn playable(&self) -> Vec<Action> {
        let mut res = vec![];
        for c in 0..SIZE {
            if self.board[c].len() > 0 && self.board[c][0] == self.active() {
                for i in 1..self.board[c].len()+1 {
                    let d = self.move_to(c);
                    if 0 <= d && d < SIZE as isize{
                        res.push(Action::Move(c, i))
                    }
                }
            }
        }
        if res.is_empty() {
            res.push(Action::Pass)
        }
        return res;
    }
    fn active(&self) -> Piece {
        if self.first_turn {
            Piece::First
        } else {
            Piece::Second
        }
    }
    fn is_end(&self) -> bool {
        if self.playable()[0] == Action::Pass {
            let mut s = self.clone();
            s.first_turn = !s.first_turn;
            return s.playable()[0] == Action::Pass
        }
        return false;
    }
    fn act(&mut self, a: Action) -> Result<(), String> {
        if self.is_end() {
            return Err(format!("game is over"));
        }
        match a {
            Action::Move(c, i) => {
                if !(c < SIZE) {
                    return Err(format!("The column is over SIZE: c = {}", c))
                }
                if !(self.board[c].len() > 0) {
                    return Err(format!("The column has no tower: c = {}", c))

                }
                if !(self.board[c][0] == self.active()) {
                    return Err(format!("The tower is not yours"))
                }
                if !(0 < i && i <= self.board[c].len()) {
                    return Err(format!("The move is over the tower: i = {}", i))
                }
                let d = self.move_to(c);
                if !(0 <= d && d < SIZE as isize) {
                    return Err(format!("The column it moved to is over SIZE: move_to = {}", d))
                }
                let from_tower = self.board[c].clone();
                self.board[c] = from_tower[i..from_tower.len()].to_vec();
                self.board[d as usize].splice(0..0, from_tower[0..i].to_vec());
            }
            Action::Pass => {
                let playable = self.playable();
                if  !(playable.len() == 1 && playable[0] == Action::Pass) {
                    return Err(format!("There are playable moves"))
                }
            }
        }
        self.first_turn = !self.first_turn;
        Ok(())
    }
    fn result(&self) -> isize {
        let f = self.board[SIZE-1].len();
        let s = self.board[0].len();
        f as isize - s as isize
    }
}
use std::fmt;
impl fmt::Display for Game27 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ch = |p| match p {
            Piece::First => "O",
            Piece::Second => "X",
        };
        let mut s = String::new();
        for c in 0..SIZE {
            for i in 0..self.board[c].len() {
                let c = ch(self.board[c][i]);
                s = format!("{}{}", s, c);
            }
            s = format!("{}\n", s);
        }
        if self.is_end() {
            s = format!("{}Over! Result: {}\n", s, self.result());
        } else {
            s = format!("{}{}'s turn\n", s, if self.first_turn { "X" } else { "O" });
        }
        write!(f, "{}", s)
    }
}

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

fn bit_rotate_in_range(bits: u64, first: usize, middle: usize, last: usize) -> u64 {
    let to_first  = (1 <<  first) - 1;
    let to_middle = (1 << middle) - 1;
    let to_last   = (1 <<   last) - 1;
    let mask1 = to_last ^ to_middle;
    let mask2 = to_middle ^ to_first;
    let mask3 = !(to_last ^ to_first);
    (bits & mask3) | ((bits & mask1) >> (middle - first)) | ((bits & mask2) << (last - middle))
}
fn partail_bitwise_or(mut bits: u64) -> u64 {
    bits |= bits >> 1;
    bits |= bits >> 2;
    bits |= bits >> 4;
    bits |= bits >> 8;
    bits |= bits >> 16;
    bits |= bits >> 32;
    bits
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Game27Opt {
    board: u64,
    first_turn: bool,
}
impl Game27Opt {
    fn to_str(&self) -> String {
        let mut result = String::with_capacity(26);
        for i in 0..26 {
            let bits = (self.board >> (2 * i)) & 0b11;
            result.push_str(match bits {
                0 => "|",
                2 => "O",
                3 => "X",
                _ => "!",
            });
        }
        result
    }
    fn tower_tops(&self) -> u64 {
        const MASK: u64 = 0x002A_AAAA_AAAA_AAAA;
        MASK & !self.board
    }
    fn count_tower(&self) -> usize {
        let flag = if self.first_turn {
            self.tower_tops() & (self.board << 2) & (!self.board << 3)
        } else {
            self.tower_tops() & (self.board << 2) & (self.board << 3)
        };
        flag.count_ones() as usize
    }
    fn move_to(&self, c: usize) -> isize {
        c as isize + self.count_tower() as isize * (if self.first_turn { 1 } else { -1 })
    }
}
impl TGame27 for Game27Opt {
    fn new() -> Game27Opt {
        // B(Base): 00
        // F(First): 10
        // S(Second): 11
        // MSB <- BBBB_BBSS_SSSS_SSSB_BBBB_BBBF_FFFF_FFFF -> LSB
        Game27Opt { board: 0x000F_FFFC_0002_AAAA, first_turn: true }
    }
    fn playable(&self) -> Vec<Action> {
        const MASK: u64 = 0x002A_AAAA_AAAA_AAAA;
        let mut res = vec![];
        let mut top_bits = self.tower_tops();
        let mut bits = self.board;
        for c in 0..SIZE {
            let top_bit = top_bits & top_bits.wrapping_neg();
            let tower = bits & (top_bit - 1);
            top_bits ^= top_bit;
            bits ^= tower;
            if tower == 0 {
                continue;
            }
            let tower_size = (tower & MASK).count_ones() as usize;
            let is_tower_top_first = (top_bit & (tower << 3)) == 0;
            if self.first_turn == is_tower_top_first {
                for i in 1..=tower_size {
                    let d = self.move_to(c);
                    if 0 <= d && d < SIZE as isize {
                        res.push(Action::Move(c, i))
                    }
                }
            }
        }
        if res.is_empty() {
            res.push(Action::Pass)
        }
        return res;
    }
    fn active(&self) -> Piece {
        if self.first_turn {
            Piece::First
        } else {
            Piece::Second
        }
    }
    fn is_end(&self) -> bool {
        if self.playable()[0] == Action::Pass {
            let mut s = self.clone();
            s.first_turn = !s.first_turn;
            return s.playable()[0] == Action::Pass
        }
        return false;
    }
    fn act(&mut self, a: Action) -> Result<(), String> {
        if self.is_end() {
            return Err(format!("game is over"));
        }
        let mut tower_top_bits = [1; SIZE+1];
        let mut bits = self.tower_tops();
        for i in 1..=SIZE {
            let bit = bits & bits.wrapping_neg();
            bits ^= bit;
            tower_top_bits[i] = bit;
        }
        match a {
            Action::Move(c, i) => {
                if !(c < SIZE) {
                    return Err(format!("The column is over SIZE: c = {}", c))
                }
                const MASK: u64 = 0x002A_AAAA_AAAA_AAAA;
                let tower = self.board & ((tower_top_bits[c+1] - 1) ^ (tower_top_bits[c] - 1));
                let tower_size = (tower & MASK).count_ones() as usize;
                if !(tower_size > 0) {
                    return Err(format!("The column has no tower: c = {}, {}", c, self))
                }
                let is_tower_top_first = (tower_top_bits[c+1] & (tower << 3)) == 0;
                if is_tower_top_first != self.first_turn {
                    return Err(format!("The tower is not yours"))
                }
                if !(0 < i && i <= tower_size) {
                    return Err(format!("The move is over the tower: i = {}", i))
                }
                let d = self.move_to(c);
                if !(0 <= d && d < SIZE as isize) {
                    return Err(format!("The column it moved to is over SIZE: move_to = {}", d))
                }
                let from_top = 62 - tower_top_bits[c + 1].leading_zeros() as usize;
                let from_bottom = from_top - 2 * i;
                let to = 62 - tower_top_bits[d as usize + 1].leading_zeros() as usize;
                if d < c as isize {
                    self.board = bit_rotate_in_range(self.board, to, from_bottom, from_top);
                } else {
                    self.board = bit_rotate_in_range(self.board, from_bottom, from_top, to);
                }
            }
            Action::Pass => {
                let playable = self.playable();
                if  !(playable.len() == 1 && playable[0] == Action::Pass) {
                    return Err(format!("There are playable moves"))
                }
            }
        }
        self.first_turn = !self.first_turn;
        Ok(())
    }
    fn result(&self) -> isize {
        const MASK: u64 = 0x002A_AAAA_AAAA_AAAA;
        let top_bits = self.tower_tops();
        let except_last = top_bits ^ (1 << 53);
        let last_tower = self.board & !partail_bitwise_or(except_last);
        let f = (last_tower & MASK).count_ones();
        let first_tower = self.board & (top_bits ^ (top_bits - 1));
        let s = (first_tower & MASK).count_ones();
        f as isize - s as isize
    }
}
impl fmt::Display for Game27Opt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ch = |p| match p {
            Piece::First => "O",
            Piece::Second => "X",
        };
        let mut s = String::new();
        s = format!("{}\n", self.to_str());
        if self.is_end() {
            s = format!("{}Over! Result: {}\n", s, self.result());
        } else {
            s = format!("{}{}'s turn\n", s, if self.first_turn { "X" } else { "O" });
        }
        write!(f, "{}", s)
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
        self.memo.clear();
        let mut result = -10000;
        let mut alpha = -1800;
        let mut rng = rand::thread_rng();
        let mut same_count = 0;
        let mut action = None;
        let depth = 18;
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
    fn test_board() {
        let mut board = Game27::new();
        let playable = board.playable();

        let mut expected_playable = vec![];
        for i in 1..SIZE+1 {
           expected_playable.push((0, i))
        }

        fn same_action(e: Vec<Action>, g: Vec<(usize, usize)>) {
            assert_eq!(e.len(), g.len());
            for (y, x) in g {
                assert!(e.iter().any(|a| a == &Action::Move(y, x)))
            }
        }
        println!("{:?}", board);
        println!("{:?}", playable);
        same_action(playable, expected_playable);

        board.act(Action::Move(0, 4)).unwrap();
        println!("{}", board);
        println!("{:?}", board.playable());
        assert_eq!(board.board[0].len(), SIZE - 4);
        assert_eq!(board.board[1].len(), 4);

        let moves = [(8, 8), (1, 4), (7, 4), (3, 4), (7, 3), (0, 4)];
        for (c, i) in &moves {
            println!("{} {}", c, i);
            board.act(Action::Move(*c, *i)).unwrap();
            println!("{}", board);
            println!("{:?}", board.playable());
        }
    }
    #[test]
    fn test_board_opt() {
        let mut board = Game27Opt::new();
        let playable = board.playable();

        let mut expected_playable = vec![];
        for i in 1..SIZE+1 {
           expected_playable.push((0, i))
        }

        fn same_action(e: Vec<Action>, g: Vec<(usize, usize)>) {
            assert_eq!(e.len(), g.len());
            for (y, x) in g {
                assert!(e.iter().any(|a| a == &Action::Move(y, x)))
            }
        }
        println!("{:?}", board);
        println!("{:?}", playable);
        same_action(playable, expected_playable);

        board.act(Action::Move(0, 4)).unwrap();
        println!("{}", board);
        println!("{:?}", board.playable());
        //assert_eq!(board.board[0].len(), SIZE - 4);
        //assert_eq!(board.board[1].len(), 4);

        let moves = [(8, 8), (1, 4), (7, 4), (3, 4), (7, 3), (0, 4)];
        for (c, i) in &moves {
            println!("{} {}", c, i);
            board.act(Action::Move(*c, *i)).unwrap();
            println!("{}", board);
            println!("{:?}", board.playable());
        }
    }
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
