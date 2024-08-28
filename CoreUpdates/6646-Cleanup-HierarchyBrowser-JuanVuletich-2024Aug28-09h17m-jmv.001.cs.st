'From Cuis7.1 [latest update: #6645] on 28 August 2024 at 9:18:35 am'!

!HierarchyBrowser methodsFor: 'class functions' stamp: 'jmv 8/28/2024 09:15:42'!
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
				self changed: #systemCategoryRoots.
				self updateAfterClassChange.
				self changed: #classList.
				self changed: #clearUserEdits.
				self setClass: class selector: nil.
				"self clearUserEditFlag; editClass."
				^ true ]
		ifFalse: [ ^ false ]! !

!methodRemoval: HierarchyBrowser #changed: stamp: 'jmv 8/28/2024 09:17:59'!
HierarchyBrowser removeSelector: #changed:!
