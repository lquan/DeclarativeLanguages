node(a).
node(b).
node(c).
node(d).
node(e).
edge(node(a),node(b)).
edge(node(b),node(d)).
edge(node(b),node(c)).
edge(node(d),node(c)).
edge(node(b),node(a)).
edge(node(d),node(b)).
edge(node(c),node(b)).
edge(node(c),node(d)).

buur(A,B):-edge(A,B).

pad(A,A).
pad(A,B):-buur(A,C),pad(C,B).

