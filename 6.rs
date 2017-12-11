use std::cmp::Ord;
use std::convert::From;
use std::fs::File;
use std::io::prelude::*;

trait HasDuplicates {
    fn has_duplicates(self) -> bool;
}

impl<'a, T: Clone + Ord> HasDuplicates for &'a [T] {
    fn has_duplicates(self) -> bool {
        let mut v = self.to_vec();
        &mut v.sort();
        let mut duplicates = false;
        for i in 1..v.len() {
            if v[i] != v[i - 1] {
                continue;
            } else {
                duplicates = true;
                break;
            }
        }
        duplicates
    }
}

struct MemoryBankCollection {
    banks: Vec<u32>,
    history: Vec<Vec<u32>>,
}

impl MemoryBankCollection {
    fn get_max_bank(&self) -> usize {
        let max_value: u32 = *self.banks.iter().max()
            .expect("vector is empty");
        self.banks.iter().position(|&x| x == max_value)
            .expect("value not found")
    }

    fn empty_bank(&mut self, index: usize) -> u32 {
        let ret: u32 = self.banks[index];
        self.banks[index] = 0;
        ret
    }

    fn realloc_cycle(&mut self) {
        let max: usize = self.get_max_bank();
        // empties bank with the most blocks and designates its blocks
        // for reallocation
        let mut blocks_to_reallocate: u32 = self.empty_bank(max);
        let mut realloc_iter = (0..self.banks.len()).cycle().skip(max + 1);
        while blocks_to_reallocate > 0 {
            let current_bank = realloc_iter.next()
                .expect("reached end of iterator");
            blocks_to_reallocate -= 1;
            self.banks[current_bank] += 1;
        }
    }

    // runs reallocation cycles until we can tell we're stuck in a loop
    fn reallocate(&mut self) -> u32 {
        let mut realloc_cycles = 0;
        loop {
            self.realloc_cycle();
            realloc_cycles += 1;
            self.history.push(self.banks.clone());
            if self.history.has_duplicates() { break; }
        }
        realloc_cycles
    }

    // to be used at the beginning of an infinite loop (the ending state
    // of MemoryBankCollection.reallocate()); measures the length of the
    // repeating sequence
    fn cycles_until_same_state(&mut self) -> u32 {
        let target_state: Vec<u32> = self.history.last()
                                     .expect("vector is empty")
                                     .to_vec();
        let mut realloc_cycles = 0;
        loop {
            self.realloc_cycle();
            realloc_cycles += 1;
            if self.banks.clone() == target_state {
                break;
            }
        }
        realloc_cycles
    }
}

impl From<Vec<u32>> for MemoryBankCollection {
    fn from(v: Vec<u32>) -> MemoryBankCollection {
        MemoryBankCollection { banks: v, history: Vec::new() }
    }
}

fn main() {
   let mut f = File::open("6.txt").expect("file not found");
   let mut contents = String::new();
   f.read_to_string(&mut contents).expect("error reading file");
   let memory_banks: Vec<u32> = contents
                                .split_whitespace()
                                .map(|x| x.parse::<u32>()
                                          .expect("parsing error"))
                                .collect();
   let mut memory_banks = MemoryBankCollection::from(memory_banks);
   println!("{}", memory_banks.reallocate());
   println!("{}", memory_banks.cycles_until_same_state());
}
