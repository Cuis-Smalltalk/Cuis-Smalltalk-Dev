'From Cuis7.7 [latest update: #7783] on 6 January 2026 at 11:29:22 am'!

!Object methodsFor: 'class membership' stamp: 'NPM 6/Jan/2026 11:27:26'!
isKindOf: aClass
	"Answer whether the class, aClass, is a superclass or class of the receiver."

	^ self class includesBehavior: aClass! !

