'From Cuis 5.0 [latest update: #4246] on 27 June 2020 at 7:52:55 pm'!

!BrowserWindow class methodsFor: 'GUI building' stamp: 'HAW 6/27/2020 19:48:57'!
classNamesFrom: potentialClassNames with: pattern matching: toMatch 

	^ (pattern last = $. or: [pattern last = $ ])
		ifTrue: [potentialClassNames select: [:className | className asLowercase = toMatch]]
		ifFalse: [potentialClassNames select: [:className | className includesSubstring: toMatch caseSensitive: false]]! !

!BrowserWindow class methodsFor: 'GUI building' stamp: 'HAW 6/27/2020 19:48:57'!
findClassFrom: potentialClassNames ifFound: aBlock with: pattern

	| exactMatch foundClass index  classNames toMatch |

	pattern isEmpty ifTrue: [^self flash].
	
	toMatch _ (pattern copyWithout: $.) asLowercase withBlanksTrimmed.
	classNames _ self classNamesFrom: potentialClassNames with: pattern matching: toMatch.
	classNames isEmpty ifTrue: [^self flash].

	exactMatch _ classNames detect: [ :each | each asLowercase = toMatch] ifNone: [ nil ].
	index _ self indexOfClassFrom: classNames exactMatch: exactMatch.
	index = 0 ifTrue: [^self flash].
	
	foundClass _ Smalltalk at: (classNames at: index) asSymbol.
	aBlock value: foundClass! !

!BrowserWindow class methodsFor: 'GUI building' stamp: 'HAW 6/27/2020 19:51:15'!
indexOfClassFrom: classNames exactMatch: exactMatch

	| options |
	
	classNames size = 1 ifTrue: [ ^1 ].
	
	options := exactMatch
		ifNil: [ PopUpMenu labelArray: classNames lines: #()]
		ifNotNil: [
			classNames remove: exactMatch ifAbsent: [].
			classNames addFirst: exactMatch.
			PopUpMenu labelArray: classNames lines: #(1)].
		
	^options startUpMenu! !


!BrowserWindow methodsFor: 'commands' stamp: 'HAW 6/27/2020 19:43:29'!
findClass

	| scopedClassNames |
	
	scopedClassNames _ model potentialClassNames asOrderedCollection.
	
	self class 
		findClassFrom: scopedClassNames 
		ifFound: [:foundClass |
			model selectCategoryForClass: foundClass.
			model selectClass: foundClass ]! !


!BrowserWindow class methodsFor: 'GUI building' stamp: 'HAW 6/27/2020 19:49:27'!
findClassFrom: potentialClassNames ifFound: aBlock

	self classNameRequester 
		request: self findClassDialogTitle 
		initialAnswer: '' 
		do: [ :pattern | self findClassFrom: potentialClassNames ifFound: aBlock with: pattern ]! !


!BrowserWindow class reorganize!
('GUI building' classNameRequester classNamesFrom:with:matching: findClass findClassDialogTitle findClassFrom:ifFound: findClassFrom:ifFound:with: fullOnClass: fullOnClass:selector: indexOfClassFrom:exactMatch: openBrowser)
('browser menues' classListMenu2Options classListMenuOptions messageCategoryMenuOptions messageListMenu2Options messageListMenuOptions systemCatSingletonMenuOptions systemCategoryMenuOptions)
('menu-world' worldMenuForOpenGroup)
('instance creation' openNoSysCat:label:)
!

