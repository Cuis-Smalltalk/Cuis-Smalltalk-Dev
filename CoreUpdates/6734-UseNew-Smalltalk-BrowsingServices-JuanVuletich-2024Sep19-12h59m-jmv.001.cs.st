'From Cuis7.1 [latest update: #6731] on 19 September 2024 at 1:02:53 pm'!

!SystemDictionary methodsFor: 'browsing - main' stamp: 'jmv 9/19/2024 12:35:24'!
findAndBrowseClass
	"
	Smalltalk findAndBrowseClass.
	"
	^self browserWindowClass findAndBrowseClass.! !

!SystemDictionary methodsFor: 'browsing - main' stamp: 'jmv 9/19/2024 12:35:57'!
findInSourceCode
	self request: 'Text to search source code for?' do: [:aString|
		Smalltalk browseMethodsWithSourceString: aString]! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 9/19/2024 12:25:55'!
browseAllClasses
	"Create and schedule a new browser on all classes alphabetically."
	| newBrowser |
	newBrowser := HierarchyBrowser new initAlphabeticListing.
	HierarchyBrowserWindow openNoSysCat: newBrowser label: 'All Classes Alphabetically'! !


!BrowserWindow class methodsFor: 'GUI building' stamp: 'jmv 9/19/2024 12:39:55'!
findAndBrowseClass

	| allClassNames |
	allClassNames := Smalltalk classNames asOrderedCollection.
	self findClassFrom: allClassNames ifFound: [ :foundClass | self fullOnClass: foundClass selector: nil ].
 	
	! !


!Object methodsFor: 'user interface' stamp: 'jmv 9/19/2024 12:24:18'!
browseClassHierarchy
	| targetClass |
	targetClass := self class isMeta
				ifTrue: [self class theNonMetaClass]
				ifFalse: [self class ].
	Smalltalk browseHierarchy: targetClass.! !


!CompiledMethod methodsFor: 'user interface support' stamp: 'jmv 9/19/2024 12:39:02'!
browse

	Smalltalk browseFull: self methodClass selector: self selector.! !


!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 9/19/2024 12:20:13'!
browseHierarchy
	"Create and schedule a new hierarchy browser
	on the currently selected class or meta."

	| answer |
	answer := nil.
	model selectedClassOrMetaClass ifNotNil: [ :myClass |
		answer := Smalltalk browseHierarchy: myClass
			selector: model selectedMessageName ].
	^answer! !

!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 9/19/2024 12:21:32'!
browseMethodFull
	"Create and schedule a full Browser and then select
	the current class and message."

	| answer |
	answer := nil.
	model selectedClassOrMetaClass ifNotNil: [ :myClass |
		answer := Smalltalk browseFull: myClass
			selector: model selectedMessageName ].
	^answer! !


!BrowserWindow methodsFor: 'menu commands' stamp: 'jmv 9/19/2024 12:48:56'!
browseAllClasses
	Smalltalk browseAllClasses! !


!CodeFileBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 9/19/2024 12:21:26'!
browseMethodFull
	"Create and schedule a full Browser and then select
	the current class and message."

	|  answer myClass |
	answer := nil.
	(myClass := model selectedClassOrMetaClass) ifNotNil: [
		answer := Smalltalk browseFull: myClass realClass
			selector: model selectedMessageName ].
	^answer! !


!DebuggerWindow methodsFor: 'menu commands' stamp: 'jmv 9/19/2024 12:40:12'!
contextClassBrowse

	Smalltalk browseFull: model contextVariablesInspector selectedClassOrMetaClass.! !

!DebuggerWindow methodsFor: 'menu commands' stamp: 'jmv 9/19/2024 12:27:58'!
contextClassHierarchy

	Smalltalk
		browseHierarchy: model contextVariablesInspector selectedClassOrMetaClass
		selector: nil.! !

!DebuggerWindow methodsFor: 'menu commands' stamp: 'jmv 9/19/2024 12:40:18'!
receiverClassBrowse

	Smalltalk browseFull: model receiverInspector selectedClassOrMetaClass.! !

!DebuggerWindow methodsFor: 'menu commands' stamp: 'jmv 9/19/2024 12:28:20'!
receiverClassHierarchy

	Smalltalk
		browseHierarchy: model receiverInspector selectedClassOrMetaClass
		selector: nil.! !


!InspectorWindow methodsFor: 'menu commands' stamp: 'jmv 9/19/2024 12:28:45'!
browseHierarchy
	"Create and schedule a class list browser on the receiver's hierarchy."

	Smalltalk
		browseHierarchy: model selectedClassOrMetaClass
		selector: nil.! !

!InspectorWindow methodsFor: 'menu commands' stamp: 'jmv 9/19/2024 12:41:56'!
browseMethodFull
	"Create and schedule a full Browser and then select the current class and message."

	|  myClass |
	(myClass := model selectedClassOrMetaClass) ifNotNil: [
		Smalltalk browseFull: myClass selector: nil ].! !


!ObjectExplorerWindow methodsFor: 'menu commands' stamp: 'jmv 9/19/2024 12:28:57'!
browseHierarchy
	"Create and schedule a class list browser on the receiver's hierarchy."

	Smalltalk
		browseHierarchy: model selectedClass
		selector: nil.! !

!ObjectExplorerWindow methodsFor: 'menu commands' stamp: 'jmv 9/19/2024 12:41:51'!
browseMethodFull
	"Create and schedule a full Browser and then select the current class and message."

	|  myClass |
	(myClass := model selectedClass) ifNotNil: [
		Smalltalk browseFull: myClass selector: nil ].! !


!ProcessBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 9/19/2024 12:41:46'!
browseContext
	model selectedContext
		ifNil: [^ self].
	Smalltalk browseFull: model selectedClass selector: model selectedSelector.! !


!TestRunnerWindow methodsFor: 'menu commands' stamp: 'jmv 9/19/2024 12:42:11'!
browseSelected
	| cls |
	cls := (model tests at: model selectedSuite ifAbsent: ['']) copyUpTo: Character space.
	cls := cls asSymbol.
	cls := (Smalltalk at: cls ifAbsent: nil).
	cls ifNotNil: [
		Smalltalk browseFull: cls selector: nil ]! !


!WorldMorph methodsFor: 'events' stamp: 'jmv 9/19/2024 12:36:07'!
keyStroke: aKeyboardEvent

	self flag: #definesKeyboardShortcuts.
	super keyStroke: aKeyboardEvent.
	aKeyboardEvent wasHandled ifTrue: [ ^self ].

	"Handle a keystroke event."
	(aKeyboardEvent commandAltKeyPressed or: [ aKeyboardEvent controlKeyPressed ])
		ifTrue: [
			aKeyboardEvent keyCharacter = $b ifTrue: [
				Smalltalk browseFull: nil.
				aKeyboardEvent wasHandled: true. ].
			aKeyboardEvent keyCharacter = $f ifTrue: [
				Smalltalk findAndBrowseClass.
				aKeyboardEvent wasHandled: true. ].
			aKeyboardEvent keyCharacter = $F ifTrue: [
				Smalltalk findInSourceCode.
				aKeyboardEvent wasHandled: true. ].
		].
	"aKeyboardEvent keyCharacter print."! !


!PreferenceSet class methodsFor: 'sys preference' stamp: 'jmv 9/19/2024 12:44:50'!
installMiscPreferences
" Preference value defined with closure. We could insert in a dynamic array...
  Preference with event handler "
	self sysPreferences

		name: #haloHandleSize 
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize * 5 // 3 max: 16];
		
		name: #halosMorphBoundsFrameColor
		description: 'When set, use this color for a rectangle on halos morph bounds. Useful for debugging morph sizes.' 
		category: #gui 
		type: Object "Using Object as type as the property value can be either nil or a Color."
		value: nil;
		
		name: #halosMorphBoundsFillColor
		description: 'When set, use this color for filling a rectangle on halos morph bounds. Useful for debugging morph sizes.'
		category: #gui
		type: Object "Using Object as type as the property value can be either nil or a Color."
		value: nil;
	
		name: #roundedButtonRadius 	
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize * 8 // 14];
		
		name: #roundedWindowRadius 	
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize] ;
		
		name: #scrollbarThickness 
		category: #gui 		
		value: [(Preferences at: #windowTitleFont) pointSize + 2];
		
		name: #classFinder 
		category: #programming 
		value: [ Smalltalk findAndBrowseClass ];
		
		name: #defaultAuthorName 
		category: #programming 
		value: [Utilities authorName];
		
		name: #worldMenu
		category: #system
		value: TheWorldMenu ! !


!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 9/19/2024 12:46:10'!
browseClassFromIt
	"Launch a hierarchy browser for the class indicated by the current selection.  If multiple classes matching the selection exist, let the user choose among them."

	| aClass |
	self wordSelectAndEmptyCheck: [^ self].

	aClass := Utilities
		classFromPattern: self selectedString withBlanksCondensed
		withCaption: 'choose a class to browse...'.
	aClass ifNil: [^ morph flash ].

	Smalltalk
		browseHierarchy: aClass
		selector: nil.! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 9/19/2024 12:42:25'!
browseImplementorsAt: aPosition

	self 
		withSelectorAt: aPosition 
		do: [ :selector | Smalltalk browseAllImplementorsOf: selector ]
		ifBehavior: [ :behavior | Smalltalk browseFull: behavior selector: nil ]
		otherwise: [ self implementorsOfItWhenErrorsParsing ].! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 9/19/2024 12:43:13'!
browseIt
	"Launch a browser for the current selection, if appropriate"

	| aSymbol anEntry |
	(Preferences at: #alternativeBrowseIt) ifTrue: [^ self browseClassFromIt].

	self wordSelectAndEmptyCheck: [^ self].
	aSymbol := self selectedSymbol ifNil: [
		self
			evaluate: self selection string
			andDo: [ :result | result class name ]
			ifFail: [ ^morph flash ]
			profiled: false ].

	aSymbol first isUppercase
		ifTrue: [
			anEntry := (Smalltalk
				at: aSymbol
				ifAbsent: [
					Smalltalk browseAllImplementorsOf: aSymbol.
					^ nil]).
			anEntry ifNil: [^ morph flash].
			(anEntry isKindOf: Class)
				ifFalse: [anEntry := anEntry class].
			Smalltalk browseFull: anEntry selector: nil ]
		ifFalse: [
			Smalltalk browseAllImplementorsOf: aSymbol ].! !

!methodRemoval: MethodSetWindow class #findInSourceCode stamp: 'jmv 9/19/2024 13:02:09'!
MethodSetWindow class removeSelector: #findInSourceCode!
!methodRemoval: BrowserWindow class #fullOnClass: stamp: 'jmv 9/19/2024 13:01:56'!
BrowserWindow class removeSelector: #fullOnClass:!
!methodRemoval: BrowserWindow class #findClass stamp: 'jmv 9/19/2024 13:01:39'!
BrowserWindow class removeSelector: #findClass!
