use std::convert::From;
use std::fs::File;
use std::io::prelude::*;

enum Jump {
    Old,
    New,
}

#[derive(Clone)]
struct JumpVec {
    position: usize,
    vector: Vec<isize>,
}

impl JumpVec {
    #[allow(dead_code)]
    fn new() -> JumpVec {
        JumpVec { position: 0, vector: Vec::new() }
    }

    fn jump(&mut self) -> Result<isize, &str> {
        let old_position = self.position;
        let new_position = (self.position as isize)+self.vector[self.position];
        if new_position < self.vector.len() as isize && new_position >= 0 {
            self.position = 
                (self.position as isize + self.vector[self.position]) as usize;    
            self.vector[old_position] += 1;
            return Ok(new_position);
        } else {
            return Err("position out of bounds");
        }
    }

    fn new_jump(&mut self) -> Result<isize, &str> {
        let old_position = self.position;
        let new_position = (self.position as isize)+self.vector[self.position];
        if new_position < self.vector.len() as isize && new_position >= 0 {
            self.position = 
                (self.position as isize + self.vector[self.position]) as usize;    
            if self.vector[old_position] < 3 {
                self.vector[old_position] += 1;
            } else {
                self.vector[old_position] -= 1;
            }
            return Ok(new_position);
        } else {
            return Err("position out of bounds");
        }
    }

    
    fn steps_to_escape(&mut self, jump_type: Jump) -> u32 {
        let mut steps = 0;
        loop {
            let jump_result = match jump_type {
                Jump::Old => self.jump(),
                Jump::New => self.new_jump(),
            };
            steps += 1;
            match jump_result {
                Ok(_) => continue,
                Err(_) => break,
            }
        }
        steps
    }
}

impl From<Vec<isize>> for JumpVec {
    fn from(v: Vec<isize>) -> JumpVec {
        JumpVec { position: 0, vector: v }
    }
}

fn main() {
    let mut f = File::open("5.txt").expect("file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("error reading file");
    let instructions: Vec<isize> = contents
                                   .trim()
                                   .split('\n')
                                   .map(|x| x.parse::<isize>().unwrap())
                                   .collect();
    let mut instructions = JumpVec::from(instructions);
    let mut new_instructions = instructions.clone();
    println!("{}", instructions.steps_to_escape(Jump::Old));
    println!("{}", new_instructions.steps_to_escape(Jump::New));
}
