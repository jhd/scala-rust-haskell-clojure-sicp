struct Tree {
    left: Option<Box<Tree>>,
    right: Option<Box<Tree>>,
    symbols: Vec<String>,
    weight: i32,
}

impl Tree {
    fn new_leaf(symbol: String, weight: i32) -> Tree {
        Tree {
            left: None,
            right: None,
            symbols: vec![symbol],
            weight,
        }
    }

    fn new_node(left: Tree, right: Tree) -> Tree {
        let symbols = [left.symbols.clone(), right.symbols.clone()].concat();
        let weight = left.weight + right.weight;
        Tree {
            left: Some(Box::new(left)),
            right: Some(Box::new(right)),
            symbols,
            weight,
        }
    }
}

fn is_leaf(tree: &Tree) -> bool {
    tree.left.is_none() && tree.right.is_none()
}

fn encode_symbol(symbol: &str, tree: &Tree) -> Vec<i32> {
    if is_leaf(tree) {
        return vec![];
    }

    if tree.left.as_ref().unwrap().symbols.contains(&symbol.to_string()) {
        let mut encoded = vec![0];
        encoded.extend(encode_symbol(symbol, tree.left.as_ref().unwrap()));
        encoded
    } else {
        let mut encoded = vec![1];
        encoded.extend(encode_symbol(symbol, tree.right.as_ref().unwrap()));
        encoded
    }
}

fn encode(message: Vec<&str>, tree: &Tree) -> Vec<i32> {
    let mut encoded_message = vec![];
    for symbol in message {
        encoded_message.extend(encode_symbol(symbol, tree));
    }
    encoded_message
}

fn decode(bits: Vec<i32>, tree: &Tree) -> Vec<String> {
    let mut decoded_message = vec![];
    let mut current_tree = tree;

    for bit in bits {
        if bit == 0 {
            current_tree = current_tree.left.as_ref().unwrap();
        } else {
            current_tree = current_tree.right.as_ref().unwrap();
        }

        if is_leaf(current_tree) {
            decoded_message.push(current_tree.symbols[0].clone());
            current_tree = tree;
        }
    }

    decoded_message
}

fn generate_huffman_tree(freq_list: Vec<(&str, i32)>) -> Tree {
    let mut trees: Vec<Tree> = freq_list
        .into_iter()
        .map(|(symbol, weight)| Tree::new_leaf(symbol.to_string(), weight))
        .collect();

    while trees.len() > 1 {
        trees.sort_by_key(|tree| tree.weight);
        let left = trees.remove(0);
        let right = trees.remove(0);
        let new_tree = Tree::new_node(left, right);
        trees.push(new_tree);
    }

    trees.remove(0)
}

fn main() {
    let freq_list = vec![
        ("NA", 16),
        ("YIP", 9),
        ("SHA", 3),
        ("A", 2),
        ("GET", 2),
        ("JOB", 2),
        ("BOOM", 1),
        ("WAH", 1),
    ];
    let tree = generate_huffman_tree(freq_list);

    let message = vec!["NA", "YIP", "SHA", "JOB"];
    let encoded_message = encode(message.clone(), &tree);
    let decoded_message = decode(encoded_message.clone(), &tree);

    println!("Original message: {:?}", message);
    println!("Encoded message: {:?}", encoded_message);
    println!("Decoded message: {:?}", decoded_message);
}
