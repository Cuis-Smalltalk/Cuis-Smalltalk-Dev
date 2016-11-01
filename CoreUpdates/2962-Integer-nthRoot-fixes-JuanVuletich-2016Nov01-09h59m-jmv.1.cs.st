'From Cuis 4.2 of 25 July 2013 [latest update: #2960] on 1 November 2016 at 10:06:47 am'!

!Integer methodsFor: 'mathematical functions' stamp: 'jmv 10/15/2016 10:57:07'!
nthRootRounded: aPositiveInteger
    "Answer the integer nearest the nth root of the receiver.
http://stackoverflow.com/questions/39794338/precise-nth-root

http://smallissimo.blogspot.com.ar/2011/09/clarifying-and-optimizing.html
Ojo 32/64!!

Tambien
http://smallissimo.blogspot.com.ar/2011/09/reviewing-fraction-asfloat.html
"
    | guess |
    self = 0 ifTrue: [^0].
    self negative
        ifTrue:
            [aPositiveInteger even ifTrue: [ ArithmeticError signal: 'Negative numbers don''t have even roots.' ].
            ^(self negated nthRootRounded: aPositiveInteger) negated].
    guess := self nthRootTruncated: aPositiveInteger.
    ^self * 2 > ((guess + 1 raisedTo: aPositiveInteger) + (guess raisedTo: aPositiveInteger))
        ifTrue: [guess + 1]
        ifFalse: [guess]! !


!Integer methodsFor: 'mathematical functions' stamp: 'jmv 10/15/2016 10:38:37'!
nthRoot: aPositiveInteger
          "Answer the nth root of the receiver.
          Answer an Integer if root is exactly this Integer, else answer the Float nearest the exact root."
  
         | guess excess scaled nBits |
          guess := self nthRootRounded: aPositiveInteger.
         excess := (guess raisedTo: aPositiveInteger) - self.
         excess = 0 ifTrue: [ ^ guess ].
  
         nBits := Float precision - guess highBitOfMagnitude.
         nBits <= 0 ifTrue: [ ^(Fraction numerator: guess * 4 - excess sign denominator: 4) asFloat].
  
         scaled := self << (nBits * aPositiveInteger).
         guess := scaled nthRootRounded: aPositiveInteger.
         excess := (guess raisedTo: aPositiveInteger) - scaled.
         ^(Fraction numerator: guess * 4 - excess sign denominator: 1 << (nBits + 2)) asFloat! !

!Integer methodsFor: 'mathematical functions' stamp: 'jmv 10/15/2016 10:39:29'!
nthRootTruncated: aPositiveInteger
          "Answer the integer part of the nth root of the receiver."
          | guess guessToTheNthMinusOne nextGuess |
          self = 0 ifTrue: [^0].
          self negative
                  ifTrue:
                          [aPositiveInteger even ifTrue: [ ArithmeticError signal: 'Negative numbers don''t have even roots.' ].
                          ^(self negated nthRootTruncated: aPositiveInteger) negated].
          guess := 1 bitShift: self highBitOfMagnitude + aPositiveInteger - 1 // aPositiveInteger.
          [
                  guessToTheNthMinusOne := guess raisedTo: aPositiveInteger - 1.
                  nextGuess := (aPositiveInteger - 1 * guess * guessToTheNthMinusOne + self) // (guessToTheNthMinusOne * aPositiveInteger).
                 nextGuess >= guess ] whileFalse:
                          [ guess := nextGuess ].
          ( guess raisedTo: aPositiveInteger) > self  ifTrue:
                          [ guess := guess - 1 ].
          ^guess! !

