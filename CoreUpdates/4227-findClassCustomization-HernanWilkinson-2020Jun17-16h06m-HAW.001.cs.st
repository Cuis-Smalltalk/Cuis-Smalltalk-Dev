'From Cuis 5.0 [latest update: #4226] on 18 June 2020 at 9:33:45 am'!

!BrowserWindow class methodsFor: 'GUI building' stamp: 'HAW 6/18/2020 09:31:57'!
classNameRequester 

	^ClassNameRequestMorph! !

!BrowserWindow class methodsFor: 'GUI building' stamp: 'HAW 6/18/2020 09:32:01'!
findClassDialogTitle

	^ 'Class name or fragment?'! !

!BrowserWindow class methodsFor: 'GUI building' stamp: 'HAW 6/18/2020 09:27:38'!
findClassFrom: potentialClassNames ifFound: aBlock
	| classNames exactMatch foundClass index toMatch |
	self  classNameRequester request: self findClassDialogTitle initialAnswer: '' do: [:pattern|
		pattern isEmpty
			ifTrue: [self flash]
			ifFalse: 
				[toMatch _ (pattern copyWithout: $.) asLowercase withBlanksTrimmed.
				classNames _ (pattern last = $. or: [pattern last = $ ])
					ifTrue: [potentialClassNames select: [:nm |  nm asLowercase = toMatch]]
					ifFalse: [potentialClassNames select: [:n | n includesSubstring: toMatch caseSensitive: false]].
				classNames isEmpty
					ifTrue: [self flash]
					ifFalse:
						[exactMatch _ classNames detect: [ :each | each asLowercase = toMatch] ifNone: nil.

						index _ classNames size = 1
							ifTrue: [ 1 ]
							ifFalse: [ exactMatch
								ifNil: [(PopUpMenu labelArray: classNames lines: #()) startUpMenu]
								ifNotNil: [classNames addFirst: exactMatch.
									(PopUpMenu labelArray: classNames lines: #(1)) startUpMenu]].

						index = 0
							ifTrue: [self flash]
							ifFalse:
								[foundClass _ Smalltalk at: (classNames at: index) asSymbol.
 	
								aBlock value: foundClass]]]]! !


!BrowserWindow class reorganize!
('GUI building' classNameRequester findClass findClassDialogTitle findClassFrom:ifFound: fullOnClass: fullOnClass:selector: openBrowser)
('browser menues' classListMenu2Options classListMenuOptions messageCategoryMenuOptions messageListMenu2Options messageListMenuOptions systemCatSingletonMenuOptions systemCategoryMenuOptions)
('menu-world' worldMenuForOpenGroup)
('instance creation' openNoSysCat:label:)
!

