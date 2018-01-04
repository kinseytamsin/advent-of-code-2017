use std::fs::File;
use std::io::prelude::*;

fn matching_digits_sum(puzzle_input: &str, compare_digit_distance: usize) -> u32 {
    let input_digits_iter = puzzle_input.chars().cycle();
    let mut sum = 0;

    for (i, j) in input_digits_iter.clone()
                  .zip(input_digits_iter.skip(compare_digit_distance))
                  .take(puzzle_input.len()) {
        let i = i.to_digit(10).unwrap();
        let j = j.to_digit(10).unwrap();
        if i == j {
            sum += i;
        }
    }
    sum
}

fn main() {
    let mut f = File::open("1.txt").expect("file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("error reading file");
    let contents = contents.trim();
    println!("{}", matching_digits_sum(contents, 1));
    println!("{}", matching_digits_sum(contents, contents.len() / 2));
}
