'From Cuis7.5 [latest update: #7272] on 18 June 2025 at 5:05:03 pm'!

!Object methodsFor: 'user interface' stamp: 'jmv 6/17/2025 17:57:51'!
request: queryString do: acceptBlock
	"Non-modal. Method returns immediately.
	Uses acceptBlock to process user's answer later, when provided by them."
	^ self request: queryString initialAnswer: '' verifying: [:aString| true] do: acceptBlock orCancel: nil! !

!Object methodsFor: 'user interface' stamp: 'jmv 6/17/2025 17:56:49'!
request: queryString initialAnswer: defaultAnswer do: acceptBlock
	"Non-modal. Method returns immediately.
	Uses acceptBlock to process user's answer later, when provided by them."
	^ self request: queryString initialAnswer: defaultAnswer verifying: [:aString| true] do: acceptBlock orCancel: nil! !

!Object methodsFor: 'user interface' stamp: 'jmv 6/17/2025 17:58:27'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock
	"Non-modal. Method returns immediately.
	Uses acceptBlock to process user's answer later, when provided by them."
	^ self request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: nil! !

