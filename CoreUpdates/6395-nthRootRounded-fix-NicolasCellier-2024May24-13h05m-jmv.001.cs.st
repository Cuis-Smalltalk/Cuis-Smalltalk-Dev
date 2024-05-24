'From Cuis7.1 [latest update: #6392] on 24 May 2024 at 1:05:54 pm'!

!Integer methodsFor: 'mathematical functions' stamp: 'nice 5/4/2019 00:26'!
nthRootRounded: aPositiveInteger
    "Answer the integer nearest the nth root of the receiver.
http://stackoverflow.com/questions/39794338/precise-nth-root
http://smallissimo.blogspot.com.ar/2011/09/clarifying-and-optimizing.html
http://smallissimo.blogspot.com.ar/2011/09/reviewing-fraction-asfloat.html
"
	| guess |
	self = 0 ifTrue: [^0].
	self negative
		ifTrue: [
			aPositiveInteger even ifTrue: [
				^DomainError signal: 'Negative numbers don''t have even roots.' ].
			^(self negated nthRootRounded: aPositiveInteger) negated ].
	guess := self nthRootTruncated: aPositiveInteger.
	^(2 * guess + 1 raisedToInteger: aPositiveInteger) < (self bitShift: aPositiveInteger)
		ifTrue: [guess + 1]
		ifFalse: [guess].
! !

