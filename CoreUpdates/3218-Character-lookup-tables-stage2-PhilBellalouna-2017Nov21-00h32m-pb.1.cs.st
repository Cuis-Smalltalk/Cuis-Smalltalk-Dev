'From Cuis 5.0 of 7 November 2016 [latest update: #3212] on 21 November 2017 at 3:19:55 am'!
!Character methodsFor: 'testing' stamp: 'pb 11/21/2017 02:29:43'!
isLetter
	"Answer whether the receiver is a letter."
	^ LetterTruthTable at: self numericValue + 1! !

!Character methodsFor: 'testing' stamp: 'pb 11/21/2017 02:24:10'!
isLowercase
	"Answer whether the receiver is a lowercase letter."
	^ LowercaseTruthTable at: self numericValue + 1.! !

!Character methodsFor: 'testing' stamp: 'pb 11/21/2017 02:24:25'!
isUppercase
	"Answer whether the receiver is an uppercase letter."
	^ UppercaseTruthTable at: self numericValue + 1.! !

!Character methodsFor: 'converting' stamp: 'pb 11/21/2017 02:22:41'!
asLowercase
	^ LowercaseMappingTable at: self numericValue + 1.! !

!Character methodsFor: 'converting' stamp: 'pb 11/21/2017 02:23:38'!
asUppercase
	"If the receiver is lowercase, answer its matching uppercase Character."
	^ UppercaseMappingTable at: self numericValue + 1.! !
