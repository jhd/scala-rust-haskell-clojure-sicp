fn queens(x: u32) -> Vec<Vec<u32>> {
    if x == 0 {
        return vec![]
    }
    return generate_board(x, x)
}

fn generate_board(k: u32, board_size: u32) -> Vec<Vec<u32>>{
    if k == 1{
        let mut start = vec![];
        for i in 1..board_size+1{
            start.push(vec![i]);
        }
        return start
    }
    let existing:Vec<Vec<u32>> = generate_board(k-1, board_size).into_iter().filter(|pos| is_safe(pos.to_vec())).collect();
    let mut candidates = vec![];
    for i in 1..board_size+1{
        for pos in existing.iter(){

            let mut candidate = pos.clone();
            candidate.push(i);
            candidates.push(candidate);
        }
    }
    return candidates.into_iter().filter(|pos| is_safe(pos.to_vec())).collect();
}

fn is_safe(pos: Vec<u32>) -> bool {
    if let Some((&new_queen, existing)) = pos.split_last(){
        if existing.is_empty() {
            return true
        }
        for (i, &queen) in existing.iter().rev().enumerate() {
            //println!("{} {}", i, queen);
            if queen              == new_queen ||
               queen + (i as u32)+1 == new_queen ||
               queen > (i as u32)+1 && queen - (i as u32)-1 == new_queen {
                return false
            }
        }
        return true

    }
    else{
        return true
    }
}

fn main(){
    println!("{:?}", queens(4));
    println!("{:?}", queens(8));
}
