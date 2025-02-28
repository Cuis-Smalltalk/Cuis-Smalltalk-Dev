'From Cuis7.3 [latest update: #7055] on 28 February 2025 at 10:57:26 am'!

!MessageNode methodsFor: 'testing' stamp: 'jmv 2/28/2025 10:55:24'!
isMessageNamed: aSelector
	"aSelector may be a Symbol or a String."

	^aSelector = self selectorSymbol ! !

