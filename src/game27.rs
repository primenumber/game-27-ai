use std::hash::Hash;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Piece {
    First,
    Second,
}
const SIZE: usize = 9;

pub trait TGame27: Eq + Hash + Clone {
    fn new() -> Self;
    fn playable(&self) -> Vec<Action>;
    fn active(&self) -> Piece;
    fn is_end(&self) -> bool;
    fn act(&mut self, a: Action) -> Result<(), String>;
    fn result(&self) -> isize;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Game27 {
    board: [Vec<Piece>; SIZE],
    first_turn: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Action {
    Move(usize, usize),
    Pass,
}
impl Action {
    #[allow(dead_code)]
    pub fn parse(s: &str) -> Result<Action, String> {
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

fn bit_rotate_in_range(bits: u64, first: usize, middle: usize, last: usize) -> u64 {
    let to_first  = (1 <<  first) - 1;
    let to_middle = (1 << middle) - 1;
    let to_last   = (1 <<   last) - 1;
    let mask1 = to_last ^ to_middle;
    let mask2 = to_middle ^ to_first;
    let mask3 = !(to_last ^ to_first);
    (bits & mask3) | ((bits & mask1) >> (middle - first)) | ((bits & mask2) << (last - middle))
}
fn partial_bitwise_or(mut bits: u64) -> u64 {
    bits |= bits >> 1;
    bits |= bits >> 2;
    bits |= bits >> 4;
    bits |= bits >> 8;
    bits |= bits >> 16;
    bits |= bits >> 32;
    bits
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Game27Opt {
    board: u64,
    first_turn: bool,
}
const BOARD_MASK: u64 = 0x002A_AAAA_AAAA_AAAA;
impl Game27Opt {
    #[allow(dead_code)]
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
        BOARD_MASK & !self.board
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
    fn is_movable(&self) -> bool {
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
            let is_tower_top_first = (top_bit & (tower << 3)) == 0;
            if self.first_turn == is_tower_top_first {
                let d = self.move_to(c);
                if 0 <= d && d < SIZE as isize {
                    return true;
                }
            }
        }
        false
    }
    #[allow(dead_code)]
    fn tower_sizes(&self) -> [usize; SIZE] {
        let mut result = [0; SIZE];
        let mut top_bits = self.tower_tops();
        let mut bits = self.board;
        for count in &mut result {
            let bit = top_bits & top_bits.wrapping_neg();
            top_bits ^= bit;
            let tower = bits & (bit - 1);
            bits ^= tower;
            *count = (tower & BOARD_MASK).count_ones() as usize;
        }
        result
    }
    fn to_game27_tower(&self, mut bits: u64) -> Vec<Piece> {
        let mut tower = Vec::new();
        while bits > 0 {
            let lowest = bits & 0b11;
            let piece = match lowest {
                0b10 => Some(Piece::First),
                0b11 => Some(Piece::Second),
                _ => None
            };
            tower.push(piece.unwrap());
            bits >>= 2;
        }
        tower.reverse();
        tower
    }
    #[allow(dead_code)]
    fn to_game27(&self) -> Game27 {
        let mut towers: [Vec<Piece>; SIZE] = Default::default();
        let mut top_bits = self.tower_tops();
        let mut bits = self.board;
        let mut old_offset = 0;
        for c in 0..SIZE {
            let top_bit = top_bits & top_bits.wrapping_neg();
            let tower_bits = bits & (top_bit - 1);
            top_bits ^= top_bit;
            bits ^= tower_bits;
            towers[c] = self.to_game27_tower(tower_bits >> old_offset);
            old_offset = top_bit.trailing_zeros() + 1;
        }
        Game27 { board: towers, first_turn: self.first_turn }
    }
    fn reverse_towers_board(mut bits: u64) -> u64 {
        let mut top_bits = BOARD_MASK & !bits;
        let mut old_offset = 0;
        let mut result = 0;
        for _c in 0..SIZE {
            let top_bit = top_bits & top_bits.wrapping_neg();
            let tower_bits = bits & (top_bit - 1);
            top_bits ^= top_bit;
            bits ^= tower_bits;
            let new_offset = top_bit.trailing_zeros() + 1;
            result <<= new_offset - old_offset;
            result |= tower_bits >> old_offset;
            old_offset = new_offset;
        }
        result
    }
    fn normalize(&self) -> u64 {
        if self.first_turn {
            self.board
        } else {
            Self::reverse_towers_board(self.board)
        }
    }
    #[allow(dead_code)]
    fn compress_bits(&self) -> u64 {
        // 5*5 + 1 pieces -> 5*8 + 2 bits = 42-bit
        let mut norm_bits = self.normalize();
        let mut result = 0;
        for i in 0..5 {
            let mut five_pieces = norm_bits & 0x3FF;
            let mut index = 0;
            for _j in 0..5 {
                let lowest = five_pieces & 0b11;
                index *= 3;
                match lowest {
                    0b00 => index += 0, // Base
                    0b10 => index += 1, // First
                    0b11 => index += 2, // Second
                    _ => panic!()
                };
                five_pieces >>= 2;
            }
            result |= index << (i * 8);
            norm_bits >>= 10;
        }
        result |= norm_bits << 40;
        result
    }
    #[allow(dead_code)]
    fn uncomress_bits(mut bits: u64) -> u64 {
        // 5*8 + 2 bits -> 5*5 + 1 pieces = 26-pcs
        let mut result = 0;
        for i in 0..5 {
            let mut index = bits & 0xFF;
            let mut five_pieces = 0;
            for _j in 0..5 {
                let lowest = index % 3;
                five_pieces <<= 2;
                match lowest {
                    0 => five_pieces |= 0b00,
                    1 => five_pieces |= 0b10,
                    2 => five_pieces |= 0b11,
                    _ => panic!()
                };
                index /= 3;
            }
            result |= five_pieces << (i * 10);
            bits >>= 8;
        }
        result |= bits << 50;
        result
    }
    #[allow(dead_code)]
    fn game_from_bits(bits: u64, first_turn: bool) -> Game27Opt {
        let uc_bits = Self::uncomress_bits(bits);
        if first_turn {
            Game27Opt { board: uc_bits, first_turn: first_turn }
        } else {
            Game27Opt { board: Self::reverse_towers_board(uc_bits), first_turn: first_turn }
        }
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
        let mut res = Vec::with_capacity(SIZE);
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
            let d = self.move_to(c);
            if d < 0 || SIZE as isize <= d {
                continue;
            }
            let tower_size = (tower & BOARD_MASK).count_ones() as usize;
            let is_tower_top_first = (top_bit & (tower << 3)) == 0;
            if self.first_turn == is_tower_top_first {
                for i in 1..=tower_size {
                    res.push(Action::Move(c, i))
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
        if !self.is_movable() {
            let mut s = self.clone();
            s.first_turn = !s.first_turn;
            return !s.is_movable();
        }
        false
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
                let tower = self.board & ((tower_top_bits[c+1] - 1) ^ (tower_top_bits[c] - 1));
                let tower_size = (tower & BOARD_MASK).count_ones() as usize;
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
                if self.is_movable() {
                    return Err(format!("There are playable moves"))
                }
            }
        }
        self.first_turn = !self.first_turn;
        Ok(())
    }
    fn result(&self) -> isize {
        let top_bits = self.tower_tops();
        let except_last = top_bits ^ (1 << 53);
        let last_tower = self.board & !partial_bitwise_or(except_last);
        let f = (last_tower & BOARD_MASK).count_ones();
        let first_tower = self.board & (top_bits ^ (top_bits - 1));
        let s = (first_tower & BOARD_MASK).count_ones();
        f as isize - s as isize
    }
}
impl fmt::Display for Game27Opt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = format!("{}\n", self.to_str());
        if self.is_end() {
            s = format!("{}Over! Result: {}\n", s, self.result());
        } else {
            s = format!("{}{}'s turn\n", s, if self.first_turn { "X" } else { "O" });
        }
        write!(f, "{}", s)
    }
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
        let mut board_naive = Game27::new();
        assert_eq!(board.to_game27(), board_naive);
        let playable = board.playable();
        let playable_naive = board.playable();
        assert_eq!(playable, playable_naive);

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
        board_naive.act(Action::Move(0, 4)).unwrap();
        println!("{}", board);
        println!("{:?}", board.playable());
        assert_eq!(board.to_game27(), board_naive);
        let sizes = board.tower_sizes();
        assert_eq!(sizes[0], SIZE - 4);
        assert_eq!(sizes[1], 4);

        let moves = [(8, 8), (1, 4), (7, 4), (3, 4), (7, 3), (0, 4)];
        for (c, i) in &moves {
            println!("{} {}", c, i);
            board.act(Action::Move(*c, *i)).unwrap();
            board_naive.act(Action::Move(*c, *i)).unwrap();
            println!("{}", board);
            println!("{:?}", board.playable());
            assert_eq!(board.to_game27(), board_naive);
        }
    }
    #[test]
    fn test_board_opt_encode_decode() {
        let mut board = Game27Opt::new();
        println!("{}", board);

        assert_eq!(board, Game27Opt::game_from_bits(board.compress_bits(), board.first_turn));

        let moves = [(0, 4), (8, 8), (1, 4), (7, 4), (3, 4), (7, 3), (0, 4)];
        for (c, i) in &moves {
            println!("{} {}", c, i);
            board.act(Action::Move(*c, *i)).unwrap();
            println!("{}", board);
            assert_eq!(board, Game27Opt::game_from_bits(board.compress_bits(), board.first_turn));
        }
    }
}
