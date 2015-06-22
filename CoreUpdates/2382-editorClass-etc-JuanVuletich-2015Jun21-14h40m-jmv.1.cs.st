'From Cuis 4.2 of 25 July 2013 [latest update: #2381] on 21 June 2015 at 2:42:43.075275 pm'!
!classDefinition: #ClassNameRequestMorph category: #'Morphic-Widgets'!
FillInTheBlankMorph subclass: #ClassNameRequestMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!ClassNameRequestMorph commentStamp: '<historical>' prior: 0!
Allows use of the SmalltalkCompleter to find class names!


!ObjectExplorer methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 14:26'!
autoCompleterClassFor: textGetter
	^SmalltalkCompleter! !

!ObjectExplorer methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 14:25'!
editorClassFor: textGetter
	^SmalltalkEditor! !


!ClassNameRequestMorph methodsFor: 'user interface support' stamp: 'jmv 6/21/2015 14:31'!
autoCompleterClassFor: textGetter
	^SmalltalkCompleter! !


!BrowserWindow methodsFor: 'commands' stamp: 'jmv 6/21/2015 14:33'!
findClass
	"Search for a class by name."
	| pattern foundClass classNames index toMatch exactMatch potentialClassNames |

	self okToChange ifFalse: [ ^self flash ].
	pattern _ ClassNameRequestMorph request: 'Class name or fragment?'.
	pattern isEmpty ifTrue: [^ self flash].
	toMatch _ (pattern copyWithout: $.) asLowercase withBlanksTrimmed.
	potentialClassNames _ model potentialClassNames asOrderedCollection.
	classNames _ (pattern last = $. or: [pattern last = $ ])
		ifTrue: [potentialClassNames select:
					[:nm |  nm asLowercase = toMatch]]
		ifFalse: [potentialClassNames select: 
					[:n | n includesSubstring: toMatch caseSensitive: false]].
	classNames isEmpty ifTrue: [^ self flash].
	exactMatch _ classNames detect: [ :each | each asLowercase = toMatch] ifNone: nil.

	index _ classNames size = 1
		ifTrue:	[1]
		ifFalse:	[exactMatch
			ifNil: [(PopUpMenu labelArray: classNames lines: #()) startUpMenu]
			ifNotNil: [classNames addFirst: exactMatch.
				(PopUpMenu labelArray: classNames lines: #(1)) startUpMenu]].
	index = 0 ifTrue: [^ self flash].
	foundClass _ Smalltalk at: (classNames at: index) asSymbol.
 	model selectCategoryForClass: foundClass.
	model selectClass: foundClass
! !

!methodRemoval: FillInTheBlankMorph #autoCompleterClass!
FillInTheBlankMorph removeSelector: #autoCompleterClass!
!methodRemoval: ProcessBrowser #autoCompleterClass!
ProcessBrowser removeSelector: #autoCompleterClass!

!ProcessBrowser reorganize!
('accessing' context: processList processListIndex processListIndex: searchString selectedMethod selectedProcess selectedProcess: selectedSelector stackList stackListIndex stackListIndex: text)
('menus' selectedClass)
('process actions' resumeProcess signalSemaphore suspendProcess terminateProcess wasProcessSuspendedByProcessBrowser:)
('process list' findContext: nextContext notify:at:in: processNameList updateProcessList)
('stack list' changeStackListTo: moreStack pcRange selectedContext stackNameList updateStackList updateStackList:)
('stepping' stepAt:in:)
('initialization' initialize startCPUWatcher stopCPUWatcher)
!

!methodRemoval: CodePackageList #editorClass!
CodePackageList removeSelector: #editorClass!
!methodRemoval: ObjectExplorer #autoCompleterClass!
ObjectExplorer removeSelector: #autoCompleterClass!
!methodRemoval: ObjectExplorer #editorClass!
ObjectExplorer removeSelector: #editorClass!
