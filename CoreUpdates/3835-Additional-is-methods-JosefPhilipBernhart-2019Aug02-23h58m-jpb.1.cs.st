'From Cuis 5.0 [latest update: #3826] on 3 August 2019 at 12:15:41 am'!

!Boolean methodsFor: 'testing' stamp: 'jpb 8/3/2019 00:11:00'!
is: aSymbol
	^#Boolean = aSymbol or: [ super is: aSymbol ]! !


!Number methodsFor: 'testing' stamp: 'jpb 8/2/2019 23:58:01'!
is: aSymbol
	"Note: Senders might prefer #isNumber for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum."
	^#Number = aSymbol or: [ super is: aSymbol]! !


!Fraction methodsFor: 'testing' stamp: 'jpb 8/3/2019 00:08:08'!
is: aSymbol
	"Note: Senders might prefer #isFraction for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum."
	^#Fraction = aSymbol or: [ super is: aSymbol ]! !


!Integer methodsFor: 'testing' stamp: 'jpb 8/2/2019 23:58:43'!
is: aSymbol
	"Note: Senders might prefer #isInteger for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum."
	^#Integer = aSymbol or: [ super is: aSymbol ]! !


!Collection methodsFor: 'testing' stamp: 'jpb 8/2/2019 23:59:42'!
is: aSymbol
	"Note: Senders might prefer #isCollection for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum."
	^#Collection = aSymbol or: [ super is: aSymbol ]! !


!String methodsFor: 'testing' stamp: 'jpb 8/3/2019 00:01:22'!
is: aSymbol
	"Note: Senders might prefer #isString for perfomance reasons. Still, Cuis tries to keep isXXX testing selectors to a minimum."
	^#String = aSymbol or: [ super is: aSymbol]! !


!Boolean reorganize!
('logical operations' & eqv: not | xor:)
('controlling' and: and:and: and:and:and: and:and:and:and: ifFalse: ifFalse:ifTrue: ifTrue: ifTrue:ifFalse: or: or:or: or:or:or: or:or:or:or:)
('copying' shallowCopy)
('testing' is:)
('printing' isLiteral storeOn:)
!

