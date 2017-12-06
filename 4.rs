use std::cmp::Ord;
use std::fs::File;
use std::io::prelude::*;

trait Alphabetize {
    fn alphabetize(&mut self);
}

impl Alphabetize for String {
    fn alphabetize(&mut self) {
        let mut chars: Vec<char> = self.chars().collect();
        chars.sort();
        *self = chars.into_iter().collect();
    }
}

trait ToStringVec {
    fn to_string_vec(self) -> Vec<String>;
}

impl <'a> ToStringVec for Vec<&'a str> {
    fn to_string_vec(self) -> Vec<String> {
        self.iter().map(|s| String::from(*s)).collect()
    }
}

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

trait HasDuplicatesOrAnagrams {
    fn has_duplicates_or_anagrams(&self) -> bool;
}

impl HasDuplicatesOrAnagrams for Vec<String> {
    fn has_duplicates_or_anagrams(&self) -> bool {
        let mut v = self.to_vec();
        // rearrange the letters in words into alphabetical order so
        // that anagrams will be identical
        for word in v.iter_mut() {
            word.alphabetize();
        }
        v.has_duplicates()
    }
}

fn parse_line(line: &str) -> Vec<&str> {
    line.split_whitespace().collect()
}

fn main() {
    let mut f = File::open("4.txt").expect("file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("error reading file");
    let passphrases: Vec<Vec<String>> = contents
                                        .trim()
                                        .split('\n')
                                        .map(|x| parse_line(x))
                                        .map(|x| x.to_string_vec())
                                        .collect();
    let valid_passphrases =
        passphrases.len()
        - (passphrases.iter()
           .filter(|x| x.has_duplicates())
           .collect::<Vec<_>>()
           .len());
    println!("{}", valid_passphrases);
    let valid_passphrases_a =
        passphrases.len()
        - (passphrases.iter()
           .filter(|x| x.has_duplicates_or_anagrams())
           .collect::<Vec<_>>()
           .len());
    println!("{}", valid_passphrases_a);
}
