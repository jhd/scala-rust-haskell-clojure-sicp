fn same_parity(xs: Vec<i32>) -> Option<Vec<i32>> {
    if xs.len() < 1 {
        return None
    }
    let res = xs.clone().into_iter().filter(|x| x % 2 == xs[0] % 2).collect();
    return Some(res)
}
fn main() {
    if let Some(odds) = same_parity(vec![1, 2, 3, 4, 5, 6, 7]) {
        println!("{:?}", odds);
    }
    if let Some(even) = same_parity(vec![2, 3, 4, 5, 6, 7]) {
        println!("{:?}", even);
    }
}
