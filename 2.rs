use std::fs::File;
use std::io::prelude::*;

fn parse_line(line: &str) -> Vec<i32> {
    line.split_whitespace().map(|x| x.parse().unwrap()).collect()
}

fn get_row_checksum(row: &Vec<i32>) -> i32 {
    row.iter().max().unwrap() - row.iter().min().unwrap()
}

fn get_sheet_checksum(sheet: &Vec<Vec<i32>>) -> i32 {
    let mut row_checksum;
    let mut sheet_checksum = 0;
    for row in sheet {
        row_checksum = get_row_checksum(&row);
        sheet_checksum += row_checksum;
    }
    sheet_checksum
}

fn divide_evenly_divisible(row: &Vec<i32>) -> Option<i32> {
    for &elem in row {
        if row.iter().any(|&x| elem % x == 0 && elem != x) {
            let smaller_factor = row.iter()
                                 .find(|&&x| elem % x == 0 && elem != x)
                                 .unwrap();
            return Some(elem / smaller_factor);
        }
    }
    None
}

fn sum_divided_values(sheet: &Vec<Vec<i32>>) -> i32 {
    let mut sum = 0;
    for row in sheet {
        sum += divide_evenly_divisible(&row).unwrap();
    }
    sum
}

fn main() {
    let mut f = File::open("2.txt").expect("file not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("error reading file");

    let sheet: Vec<Vec<i32>> = contents.trim().split('\n')
                              .map(|x| parse_line(x)).collect();
    let checksum = get_sheet_checksum(&sheet);
    println!("{}", checksum);
    let sum = sum_divided_values(&sheet);
    println!("{}", sum);
}
