'From Cuis6.3 [latest update: #6254] on 8 March 2024 at 3:06:13 pm'!
!classDefinition: #MessageNames category: #'Tools-Browser'!
MethodSet subclass: #MessageNames
	instanceVariableNames: 'searchString selectorList selectedSelector '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Browser'!
!classDefinition: #ProtocolBrowser category: #'Tools-Browser'!
MethodSet subclass: #ProtocolBrowser
	instanceVariableNames: 'baseClass selectiveClassList selectiveClassListIndex selectedName exclude '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Browser'!
!classDefinition: #TimeProfileBrowser category: #'Tools-Profiling'!
MethodSet subclass: #TimeProfileBrowser
	instanceVariableNames: 'tally talliesList '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!
!classDefinition: #ExtractMethodMessageSet category: #'Tools-Refactoring'!
MethodSet subclass: #ExtractMethodMessageSet
	instanceVariableNames: 'finder selectedIndex '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #InlineMethodMessageSendsSet category: #'Tools-Refactoring'!
MethodSet subclass: #InlineMethodMessageSendsSet
	instanceVariableNames: 'applier selectedIndex '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!Browser methodsFor: 'testing' stamp: 'jmv 3/8/2024 14:15:03'!
isEditingNone

	^self editSelection = #none! !


!CodeProvider methodsFor: 'commands' stamp: 'jmv 3/8/2024 14:58:41'!
hierarchyBrowser
	"Create and schedule a new hierarchy browser on the currently selected class or meta."

	| newBrowser aSymbol aBehavior messageCatIndex selectedClassOrMetaClass |
	(selectedClassOrMetaClass := self selectedClassOrMetaClass)
		ifNil: [^ nil].
	newBrowser := HierarchyBrowser new initHierarchyForClass: selectedClassOrMetaClass.
	((aSymbol := self selectedMessageName) notNil and: [(MethodSet isPseudoSelector: aSymbol) not])
		ifTrue: [
			aBehavior := selectedClassOrMetaClass.
			messageCatIndex := aBehavior organization numberOfCategoryOfElement: aSymbol.
			messageCatIndex = 0 ifFalse: [
				newBrowser messageCategoryListIndex: messageCatIndex + 1.
				newBrowser messageListIndex:
					((aBehavior organization listAtCategoryNumber: messageCatIndex) indexOf: aSymbol) ]].
	^newBrowser! !


!TimeProfileBrowser methodsFor: 'message list' stamp: 'jmv 3/8/2024 14:25:47'!
messageListIndex: anInteger

	"Set the index of the selected item to be anInteger."
	
	selectedMessage := (talliesList isInBounds: anInteger) ifTrue: [ talliesList at: anInteger ].
	self changed: #messageListIndex.	 "update my selection"
	self changed: #editSelection.
	self acceptedContentsChanged! !


!ChangeList methodsFor: 'accessing' stamp: 'jmv 3/8/2024 14:58:45'!
currentCompiledMethod
	"Actually, the CompiledMethod installed in the system that correspond to current selection"
	| selector |
	selector := self selectedMessageName.
	(selector isNil or: [ MethodSet isPseudoSelector: selector ]) ifTrue: [
		^nil ].
	^self selectedClassOrMetaClass compiledMethodAt: selector! !


!Behavior methodsFor: 'accessing method dictionary' stamp: 'jmv 3/8/2024 14:58:52'!
firstPrecodeCommentFor:  selector
	"If there is a comment in the source code at the given selector that preceeds the body of the method, return it here, else return nil"

	| parser source tree |
	"Behavior firstPrecodeCommentFor: #firstPrecodeCommentFor:"
	(MethodSet isPseudoSelector: selector)
		ifTrue: [
			"Not really a selector"
			^ nil].
	source := self sourceCodeAt: selector asSymbol ifAbsent: [^ nil].
	parser := self parserClass new.
	tree := 
		parser
			parse: source readStream
			class: self
			noPattern: false
			context: nil
			notifying: nil
			ifFail: [^ nil].
	^ (tree comment ifNil: [^ nil]) first! !


!SmartRefStream methodsFor: 'class changed shape' stamp: 'jmv 3/8/2024 14:59:00'!
conversionMethodsFor: classList
	| oldStruct newStruct list |
	"Each of these needs a conversion method.  Hard part is the comment in it.  Return a MethodSet."

	list := OrderedCollection new.
	classList do: [:cls |
		oldStruct := structures at: cls name ifAbsent: [#()].
		newStruct := (Array with: cls classVersion), (cls allInstVarNames).
		self writeConversionMethodIn: cls fromInstVars: oldStruct to: newStruct 
				renamedFrom: nil.
		list add: cls name, ' convertToCurrentVersion:refStream:'.
		].

	^ MethodSet new initializeMessageList: list.! !


!BrowserWindow methodsFor: 'updating' stamp: 'jmv 3/8/2024 12:48:07'!
editSelectionChanged
	
	model isEditingNone
		ifTrue: [ self disableCodePaneEditing ]
		ifFalse: [ self enableCodePaneEditing]! !


!MessageSetWindow class methodsFor: 'opening' stamp: 'jmv 3/8/2024 14:59:15'!
openMessageList: methodReferences label: aString
	"Create a standard system view for the message set on the list, anArray.
	The label of the view is aString."

	| methodSet |
	methodSet := MethodSet messageList: methodReferences asArray.
	methodSet sortByClassHierarchy.
	^self open: methodSet label: aString.! !

!MessageSetWindow class methodsFor: 'opening' stamp: 'jmv 3/8/2024 14:59:51'!
openMessageList: methodReferences label: labelString autoHighlight: autoHighlightString allOccurrences: aBoolean
	"Open a system view for a MethodSet on messageList."

	| methodSet |

	methodSet := MethodSet messageList: methodReferences asArray.
	methodSet sortByClassHierarchy.
	methodSet autoHighlightString: autoHighlightString allOccurrences: aBoolean.

	^self open: methodSet label: labelString ! !

!MessageSetWindow class methodsFor: 'opening' stamp: 'jmv 3/8/2024 15:00:02'!
openMessageListUnsorted: methodReferences label: labelString
	"Open a system view for a MethodSet on messageList.
	Don't sort entries by default."

	| methodSet |

	methodSet := MethodSet messageList: methodReferences asArray.

	^self open: methodSet label: labelString ! !

!MessageSetWindow class methodsFor: 'opening' stamp: 'jmv 3/8/2024 15:00:15'!
openMessageListUnsorted: methodReferences label: labelString autoHighlight: autoHighlightString allOccurrences: aBoolean
	"Open a system view for a MethodSet on messageList.
	Don't sort entries by default."

	| methodSet |

	methodSet := MethodSet messageList: methodReferences asArray.
	methodSet autoHighlightString: autoHighlightString allOccurrences: aBoolean.

	^self open: methodSet label: labelString ! !


!ChangeSorterWindow methodsFor: 'menu commands' stamp: 'jmv 3/8/2024 14:59:33'!
browseMethodConflicts
	"Check to see if any other change set also holds changes to any methods in the selected change set; if so, open a browser on all such."

	| aList |

	aList := model methodConflicts.
	aList isEmpty ifTrue: [
		^ self inform: 'No other change set has changes for any method in this change set.' ].

	MessageSetWindow
		open: (MethodSet messageList: aList)
		label: 'Methods in "', model changeSet name, '" that are also in other change sets (', aList size printString, ')'! !

!methodRemoval: BrowserWindow #isEditSelectionNone stamp: 'jmv 3/8/2024 15:03:54'!
BrowserWindow removeSelector: #isEditSelectionNone!
!classDefinition: #MessageNames category: #'Tools-Browser'!
MethodSet subclass: #MessageNames
	instanceVariableNames: 'searchString selectorList selectedSelector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Browser'!
!classDefinition: #ProtocolBrowser category: #'Tools-Browser'!
MethodSet subclass: #ProtocolBrowser
	instanceVariableNames: 'baseClass selectiveClassList selectiveClassListIndex selectedName exclude'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Browser'!
!classDefinition: #TimeProfileBrowser category: #'Tools-Profiling'!
MethodSet subclass: #TimeProfileBrowser
	instanceVariableNames: 'tally talliesList'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!
!classDefinition: #ExtractMethodMessageSet category: #'Tools-Refactoring'!
MethodSet subclass: #ExtractMethodMessageSet
	instanceVariableNames: 'finder selectedIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #InlineMethodMessageSendsSet category: #'Tools-Refactoring'!
MethodSet subclass: #InlineMethodMessageSendsSet
	instanceVariableNames: 'applier selectedIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
