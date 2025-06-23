type points = {contained_in:int; contains:int; intersects: int; is_right_regular: int}

let points = {contained_in=3; contains=3; intersects=1; is_right_regular=3}


let problem_name = "abdashStr"

let regexp_solution = "((-)(aVb)+(-))((;((-)(aVb)+(-)))*)"


let rrg_solution = "Start Symbol = L L::=-A A::=aA|bA|aR|bR R::=-S|- S::=;L"
