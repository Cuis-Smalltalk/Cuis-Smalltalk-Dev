# Unicode support in Cuis Smalltalk

[Paper](2022-11-UnicodeSupportInCuisSmalltalk.pdf) written by Juan and presented at the ["FAST Workshop 2022 on Smalltalk Related Technologies"](https://openreview.net/group?id=FAST.org.ar/2022/Workshop&referrer=[Homepage]). It describes the approach used to support Unicode in Smalltalk source code and generally in Text objects. It also includes a review of pitfalls in the implementation of Unicode in other programming languages and the lessons learnt from them.

This was written in 2022 and reflects the state of the system at that time. Since then, a few things have changed. Please keep them in mind as you read the paper:
- Utf8String is now called UnicodeString.
- Utf8Symbol is now called UnicodeSymbol.
- UnicodeCodePoint was integrated into Character.  Character includes the whole Unicode space.
- UnicodeString instances are mutable. #at:put: is supported. The #hash value can change if a UnicodeString is modified, just as in String.
- "Future Work" Combining characters are correctly handled.
- "Future Work" Character equivalence, composition, decomposition and normalization are supported.

This is the [Video recording of the presentation](https://youtu.be/P6e2_NvLpxE?t=0) given by Juan.
