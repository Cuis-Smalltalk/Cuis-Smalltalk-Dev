'From Cuis 5.0 of 7 November 2016 [latest update: #3082] on 24 May 2017 at 12:34:49 am'!

!Collection methodsFor: 'sorting' stamp: 'jmv 5/24/2017 00:28:06'!
sorted
	"Return a new sequenceable collection which contains the same elements as self but its elements are sorted "

	^self sorted: nil! !


!ClassDescription methodsFor: 'instance variables' stamp: 'jmv 5/24/2017 00:29:04'!
chooseInstVarAlphabeticallyThenDo: aBlock
	| allVars index |
	"Put up a menu of all the instance variables in the receiver, presented in alphabetical order, and when the user chooses one, evaluate aBlock with the chosen variable as its parameter."

	allVars _ self allInstVarNames sorted.
	allVars isEmpty ifTrue: [^ self inform: 'There are no
instance variables'].

	index _ (PopUpMenu labelArray: allVars lines: #()) startUpWithCaption: 'Instance variables in
', self name.
	index = 0 ifTrue: [^ self].
	aBlock value: (allVars at: index)! !

!ClassDescription methodsFor: 'method dictionary' stamp: 'jmv 5/24/2017 00:28:55'!
allMethodsInCategory: aSymbol
	"Answer a list of all the method categories of the receiver and all its superclasses"

	| aColl |
	aColl _ OrderedCollection new.
	self withAllSuperclasses do:
		[:aClass | aColl addAll:
			(aSymbol == ClassOrganizer allCategory
				ifTrue:
					[aClass organization allMethodSelectors]
				ifFalse:
					[aClass organization listAtCategoryNamed: aSymbol])].
	^ aColl asSet sorted

"TileMorph allMethodsInCategory: #initialization"! !


!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 5/24/2017 00:29:09'!
browseClassesWithNamesContaining: aString caseSensitive: caseSensitive 
	"Smalltalk browseClassesWithNamesContaining: 'eMorph' caseSensitive: true "
	"Launch a class-list list browser on all classes whose names containg aString as a substring."

	| suffix aList |
	suffix _ caseSensitive
				ifTrue: [' (case-sensitive)']
				ifFalse: [' (use shift for case-sensitive)'].
	aList _ OrderedCollection new.
	Smalltalk allClassesDo: [ :class |
		(class name includesSubstring: aString caseSensitive: caseSensitive)
			ifTrue: [aList add: class name]].
	aList size > 0
		ifTrue: [HierarchyBrowserWindow forClassesNamed: aList asSet sorted title: 'Classes whose names contain ' , aString , suffix]! !


!ChangeSet methodsFor: 'method changes' stamp: 'jmv 5/24/2017 00:28:50'!
changedMessageList
	"Used by a message set browser to access the list view information."

	| messageList |
	messageList _ OrderedCollection new.
	changeRecords associationsDo: [ :clAssoc | | classNameInFull classNameInParts |
		classNameInFull _ clAssoc key asString.
		classNameInParts _ classNameInFull findTokens: ' '.

		(clAssoc value allChangeTypes includes: #comment) ifTrue: [
			messageList add:
				(MethodReference new
					setClassSymbol: classNameInParts first asSymbol
					classIsMeta: false 
					methodSymbol: #Comment 
					stringVersion: classNameInFull, ' Comment')].

		clAssoc value methodChangeTypes associationsDo: [ :mAssoc |
			(#(remove addedThenRemoved movedToOtherPackage) includes: mAssoc value) ifFalse: [
				messageList add:
					(MethodReference new
						setClassSymbol: classNameInParts first asSymbol
						classIsMeta: classNameInParts size > 1 
						methodSymbol: mAssoc key 
						stringVersion: classNameInFull, ' ' , mAssoc key)]]].
	^ messageList sorted! !

!methodRemoval: ArrayedCollection #asSortedArray!
ArrayedCollection removeSelector: #asSortedArray!
!methodRemoval: Collection #asSortedArray!
Collection removeSelector: #asSortedArray!
