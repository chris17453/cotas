# TAN()

## Summary
PURPOSE
This function returns the tangent of the N type field specified.

## Signature
```
PARTS
1
```

## Details
f/c/e
The N value.
RETURN TYPE
N The return value is in radians.
EXAMPLE
? tan(1.539)
31.43957419
TEST(1,2)
PURPOSE
This function will determine whether a bit or bits are set in a byte.
PARTS
1
fn/v
Field to check. The function will only test the first byte/character.
2
f/c/e
This value determines which bits to check. To set the proper value use the following
guide:
Bit#
Value
7
128
6
64
5
32
4
16
3
8
2
4
1
2
0
1
To test for bits 7 and 4 (to see if the bits are set to 1 instead of 0) the test value
would be 144 (128+16). The function will ignore the other 6 bits.
RETURN TYPE
L If the bit(s) is (are) set (1 not 0), the function will return .T. Otherwise it will return .F.
Function Reference
