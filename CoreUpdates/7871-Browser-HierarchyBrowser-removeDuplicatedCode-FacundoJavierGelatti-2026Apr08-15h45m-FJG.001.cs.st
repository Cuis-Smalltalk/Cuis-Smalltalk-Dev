'From Cuis7.7 [latest update: #7870] on 8 April 2026 at 3:46:10 pm'!

!Browser methodsFor: 'class functions' stamp: 'FJG 6/Apr/2026 08:23:35'!
classWasDefined: aClass
	self changed: #systemCategoryRoots.
	self changed: #classList.
	self changed: #clearUserEdits.
	self setClass: aClass selector: nil.! !


!HierarchyBrowser methodsFor: 'class functions' stamp: 'FJG 6/Apr/2026 08:23:35'!
classWasDefined: aClass
	self updateAfterClassChange.
	super classWasDefined: aClass.! !


!Browser methodsFor: 'class functions' stamp: 'FJG 6/Apr/2026 08:23:35'!
defineClass: defString notifying: aRequestor
	"The receiver's textual content is a request to define a new class. The
	source code is defString. If any errors occur in compilation, notify
	aRequestor."
	| oldClass class newClassName defTokens keywdIx |
	oldClass := self selectedClassOrMetaClass.
	defTokens := defString findTokens: Character separators.
	keywdIx := defTokens findFirst: [ :x | x beginsWith: 'category' ].
	keywdIx := defTokens findFirst: [ :x | '*subclass*' match: x ].
	newClassName := (defTokens at: keywdIx+1) copyWithoutAll: '#()'.
	((oldClass isNil or: [ oldClass theNonMetaClass name asPlainString ~= newClassName ])
		and: [ Smalltalk includesKey: newClassName asSymbol ]) ifTrue: [
			"Attempting to define new class over existing one when
				not looking at the original one in this browser..."
			(self confirm: (newClassName bold , ' is an existing class in this system.
Redefining it might cause serious problems.
Is this really what you want to do?'))
				ifFalse: [ ^ false ]].
	"ar 8/29/1999: Use oldClass superclass for defining oldClass
	since oldClass superclass knows the definerClass of oldClass."
	oldClass ifNotNil: [ oldClass := oldClass superclass ].
	class := Compiler
				evaluate: defString
				notifying: aRequestor
				logged: true.
	(class isKindOf: Behavior)
		ifTrue: [
				self classWasDefined: class.
				"self clearUserEditFlag; editClass."
				^ true ]
		ifFalse: [ ^ false ]! !

!methodRemoval: HierarchyBrowser #defineClass:notifying: stamp: 'FJG 8/Apr/2026 15:45:43'!
HierarchyBrowser removeSelector: #defineClass:notifying:!
