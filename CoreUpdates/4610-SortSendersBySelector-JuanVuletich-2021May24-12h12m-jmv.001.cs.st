'From Cuis 5.0 [latest update: #4610] on 24 May 2021 at 12:12:48 pm'!

!MethodReference methodsFor: 'comparisons' stamp: 'jmv 5/24/2021 10:02:56'!
<= anotherMethodReference

	methodSymbol < anotherMethodReference methodSymbol ifTrue: [^true].
	methodSymbol > anotherMethodReference methodSymbol ifTrue: [^false].
	classSymbol < anotherMethodReference classSymbol ifTrue: [^true].
	classSymbol > anotherMethodReference classSymbol ifTrue: [^false].
	classIsMeta == anotherMethodReference classIsMeta ifFalse: [^classIsMeta not].
	^true! !

