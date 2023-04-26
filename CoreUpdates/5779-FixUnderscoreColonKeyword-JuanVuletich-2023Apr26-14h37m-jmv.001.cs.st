'From Cuis 6.0 [latest update: #5778] on 26 April 2023 at 2:38:55 pm'!

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 4/26/2023 14:16:52'!
xUnderscore
	"Figure out if x _foo (no space between _ and foo) 
	should be a selector or assignment"

	(#(xLetter xDigit xUnderscore xColon ) statePointsTo: (self typeTableAt: aheadChar))
		ifFalse: [
			self step.
			tokenType := #leftArrow.
			^token := #'_' ].
	^self xLetter.! !

