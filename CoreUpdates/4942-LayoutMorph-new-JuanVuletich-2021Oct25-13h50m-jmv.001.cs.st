'From Cuis 5.0 [latest update: #4940] on 25 October 2021 at 1:52:30 pm'!

!LayoutMorph class methodsFor: 'instance creation' stamp: 'jmv 10/25/2021 13:51:56'!
new
	"Answer a row by default so the 'new morph' menu doesn't fail..."
	^self newRow color: `(Color red alpha: 0.2)`! !

!methodRemoval: LayoutMorph class #initializedInstance stamp: 'jmv 10/25/2021 13:51:59'!
LayoutMorph class removeSelector: #initializedInstance!
