--| Error: x and y are not disjoint

f = /\A. \(x:A) -> x;
g = /\B. \(y:B) -> y;
(f , g) @() ()
