# threepasskata

## Limitations

### pass2

Add (Add (Imm 1) (Imm 2)) (Add (Imm 3) (Imm 4)) should reduce to Imm 10

### pass3

Add (Arg 0) (Imm 1) should compile to ["AR 0", "SW", "IM 1", "AD"] or ["IM 1", "SW", "AR 0", "AD"].
