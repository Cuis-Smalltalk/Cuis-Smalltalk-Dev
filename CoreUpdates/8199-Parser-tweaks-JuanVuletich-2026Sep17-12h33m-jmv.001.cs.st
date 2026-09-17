'From Cuis7.9 [latest update: #8197] on 17 September 2026 at 12:34:49 pm'!

!Parser methodsFor: 'private' stamp: 'jmv 17/Sep/2026, 1:11:22 pm (UTC)'!
init: sourceStream notifying: req failBlock: aBlock

	requestor := req.
	failBlock := aBlock.
	requestorOffset := 0.
	self scan: sourceStream.
	prevMark := hereMark := mark.
	self advance.! !

!Parser methodsFor: 'private' stamp: 'jmv 17/Sep/2026, 1:29:30 pm (UTC)'!
privateReadSelectorFrom: aMethodSource
	"Answer the message selector for the argument, aMethodSource, which should 
	 parse successfully up to the temporary declaration or the end of the 
	 method header."
	"Note: only intended to read the selector. Parser instance might be inconsistent afterwards.
	Optimized these kind of methods (compare the following):
	[ 100000 timesRepeat: [Parser new parseSelector: 'a ^#[123 123 123 123 123 123 123 123 87 87 87 87 87 87 87 87]'] ] timeToRun 863 .
	[ 100000 timesRepeat: [Parser new privateReadSelectorFrom: 'a ^#[123 123 123 123 123 123 123 123 87 87 87 87 87 87 87 87]'] ] timeToRun 118 .
	"

	| result |
	self initScannerForTokenization.
	self
		init: aMethodSource asString asPlainString readStream
		notifying: nil
		failBlock: [ ^nil ].
	encoder := self.
	result := self privateReadSelector.
	encoder := failBlock := nil.  "break cycles"
	^result! !

!Parser methodsFor: 'public access' stamp: 'jmv 17/Sep/2026, 1:25:38 pm (UTC)'!
parseSelector: aString 
	"Answer the message selector for the argument, aString, which should 
	 parse successfully up to the temporary declaration or the end of the 
	 method header.
	Generally prefer #privateReadSelectorFrom: that is much faster for long methods."

	self initScannerForTokenization.
	^self
		initPattern: aString
		notifying: nil
		return: [:pattern | pattern at: 1]! !

