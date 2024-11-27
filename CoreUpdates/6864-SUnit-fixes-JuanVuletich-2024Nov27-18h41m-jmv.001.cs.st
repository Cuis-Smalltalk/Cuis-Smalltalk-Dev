'From Cuis7.1 [latest update: #6863] on 27 November 2024 at 6:49:37 pm'!

!TestRunner methodsFor: 'logging' stamp: 'jmv 11/27/2024 18:44:41'!
showResult

	self errorLog newLine; newLine; show: '==== SUnit ======== Start ===='.
	self errorLog print: self result.
	self errorLog newLine; show: '==== SUnit ========== End ===='; newLine.! !


!TestResult methodsFor: 'testing' stamp: 'jmv 11/27/2024 18:48:54'!
hasErrors

	^self unexpectedErrors notEmpty
			! !

!TestResult methodsFor: 'testing' stamp: 'jmv 11/27/2024 18:49:06'!
hasFailures

	^self unexpectedFailures notEmpty
			! !

!methodRemoval: TestRunner #showResultSummary stamp: 'jmv 11/27/2024 18:44:47'!
TestRunner removeSelector: #showResultSummary!
!methodRemoval: TestRunner #showResultDefects stamp: 'jmv 11/27/2024 18:44:44'!
TestRunner removeSelector: #showResultDefects!
