const things = 5;
const MY_CONST = 2;

fn main(not_a_keyword: i32) -> i32 {
    var local = 5;

    switch(not_a_keyword) {
        case 1: {
            return 1;
        }
        default: {
            return 3;
        }
    }
}

fn helper_add(a:i32, b:i32) -> i32 {
    return a + b;
}

fn other_function() {
    var my_other_var = helper_add(1, 2);
    
    var my_var = 5;
    my_var = 5;
}
