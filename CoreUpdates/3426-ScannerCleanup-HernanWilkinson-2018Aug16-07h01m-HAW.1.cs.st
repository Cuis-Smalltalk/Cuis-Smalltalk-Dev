'From Cuis 5.0 of 7 November 2016 [latest update: #3418] on 16 August 2018 at 7:05:45 am'!

!Scanner methodsFor: 'multi-character scans' stamp: 'HAW 8/16/2018 07:04:23'!
xBacktick
	
	"Smalltalk code evaluated at compile time as a literal."

	self readUpToNext: $` ifNotFound: [^self offEnd: 'Unmatched back quote'].
	self compileBacktickCodeHandlingErrors.! !

!Scanner methodsFor: 'multi-character scans' stamp: 'HAW 8/16/2018 07:03:12'!
xSingleQuote
	
	"String."

	self readUpToNext: $'  ifNotFound: [^self offEnd: 'Unmatched string quote'].
	tokenType := #string! !

!methodRemoval: Scanner #isAtBacktick!
Scanner removeSelector: #isAtBacktick!
!methodRemoval: Scanner #readUpToNextBacktick!
Scanner removeSelector: #readUpToNextBacktick!
