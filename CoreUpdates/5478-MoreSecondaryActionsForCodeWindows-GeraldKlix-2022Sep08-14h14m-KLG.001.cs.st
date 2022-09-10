'From Haver 6.0 [latest update: #5477] on 8 September 2022 at 2:52:20 pm'!

!CodeWindow methodsFor: 'menu commands' stamp: 'KLG 9/8/2022 14:35:49'!
browseHierarchyAndReplaceMe
	"Create and schedule a new hierarchy browser on the currently selected class or meta.
	Resize and position it over me and dismiss me."

	model hierarchyBrowser ifNotNil: [ :newBrowser | | newBrowserWindow |
		newBrowserWindow _ HierarchyBrowserWindow
			openNoSysCat: newBrowser
			label: newBrowser labelString.
		newBrowser assureSelectionsShow.
		newBrowserWindow
			morphPositionInWorld: self morphPositionInWorld;
			morphExtentInWorld: self morphExtentInWorld.
		self dismissMorph ]! !

!CodeWindow methodsFor: 'menu commands' stamp: 'KLG 9/8/2022 14:27:42'!
browseMethodFullAndReplaceMe
	"Create and schedule a full Browser and then select the current class and message.
	Resize and position it over me and dismiss me."

	model selectedClassOrMetaClass ifNotNil: [ :myClass | | newBrowserWindow |
		newBrowserWindow _ BrowserWindow
			fullOnClass: myClass 
			selector: model selectedMessageName.
		newBrowserWindow
			morphPositionInWorld: self morphPositionInWorld;
			morphExtentInWorld: self morphExtentInWorld.
		self dismissMorph ]! !

!CodeWindow methodsFor: 'menu commands' stamp: 'KLG 9/8/2022 14:49:49'!
methodInheritanceAndReplaceMe
	"Create and schedule a method browser on the inheritance of implementors.
	Resize and position it over me and dismiss me."

	| newBrowser |
	(newBrowser _ self methodInheritance) == self ifFalse: [
		newBrowser
			morphPositionInWorld: self morphPositionInWorld;
			morphExtentInWorld: self morphExtentInWorld.
		self dismissMorph ]
	! !


!SystemDictionary methodsFor: 'browsing' stamp: 'KLG 9/8/2022 14:48:26'!
browseMessageList: methodReferences name: labelString autoSelect: autoSelectString
	"Create and schedule a MessageSet browser on the message list."

	| messageListSize title |

	messageListSize _ methodReferences size.
	messageListSize = 0 ifTrue: [^ PopUpMenu inform: ('There are no\' , labelString) withNewLines ].

	title _ messageListSize > 1
		ifFalse: [ labelString ]
		ifTrue: [ '[', messageListSize printString, '] ', labelString].

	^ MessageSetWindow
		openMessageList: methodReferences
		label: title
		autoSelect: autoSelectString! !


!CodeWindow methodsFor: 'GUI building' stamp: 'KLG 9/8/2022 14:45:06'!
optionalButtonTuples
	"Answer a tuple buttons, in the format:
			button label
			selector to send
			help message"

	| aList textConstructor |
	textConstructor _ [ :string :backgroundColor | 
		string asText addAttribute: (TextBackgroundColor color: backgroundColor) ].

	aList _ #(
		(10	'browse'			browseMethodFull							'view this method in a browser'							browseMethodFullAndReplaceMe)
		(11	'senders' 			browseSendersOfMessages							'browse senders of...'							browseSendersOfMethod)
		(16	'implementors'			browseMessages							'browse implementors of...' 							browseImplementors)
		(12	'versions'			browseVersions							'browse versions')), 

		((Preferences at: #decorateBrowserButtons)
			ifTrue: [
				{{13	. 'inheritance'.	 #methodInheritance. 	'Browse Method Inheritance

', (textConstructor value: 'green' value: `Color green muchLighter`),': sends to super
', (textConstructor value: 'tan' value: `Color tan`), ': has override(s)
', (textConstructor value: 'mauve' value: `Color blue muchLighter`), ': both of the above
', (textConstructor value: 'pink' value: `Color red muchLighter`), ': is an override but doesn''t call super
', (textConstructor value: 'pinkish tan' value: `Color r: 0.94 g: 0.823 b: 0.673`), ': has override(s), also is an override but doesn''t call super
'. #methodInheritanceAndReplaceMe }}]
			ifFalse: [
				{#(13	'inheritance'		 methodInheritance	 'browse method inheritance'. #methodInheritanceAndReplaceMe)}]),

		#(
		(12	'hierarchy'			browseHierarchy							'browse class hierarchy'							browseHierarchyAndReplaceMe)
		(10	'inst vars'			browseInstVarRefs							'inst var refs...')
		(11	'class vars'			browseClassVarRefs							'class var refs...')
		(10	'show...'			offerWhatToShowMenu							'menu of what to show in lower pane')).

	^ aList! !

!CodeWindow methodsFor: 'menu commands' stamp: 'KLG 9/8/2022 14:41:23'!
methodInheritance
	"Create and schedule a method browser on the inheritance of implementors."

	| list aClassNonMeta isMeta theClassOrMeta aClass sel |
	aClass _ model selectedClassOrMetaClass.
	sel _ model selectedMessageName.
	aClass ifNil: [ ^ self ].
	sel ifNil: [ ^ self ].
	aClassNonMeta _ aClass theNonMetaClass.
	isMeta _ aClassNonMeta ~~ aClass.
	list _ OrderedCollection new.
	aClass allSuperclasses reverseDo: [ :cl |
		(cl includesSelector: sel) ifTrue: [
			list addLast: (MethodReference class: cl selector: sel) ]].
	aClassNonMeta
		allSubclassesWithLevelDo: [ :cl :level |
			theClassOrMeta _ isMeta
				ifTrue: [ cl class ]
				ifFalse: [ cl ].
			(theClassOrMeta includesSelector: sel) ifTrue: [
				list addLast: (MethodReference class: theClassOrMeta selector: sel) ]]
		startingLevel: 0.
	^ Smalltalk
		browseMessageList: list
		name: 'Inheritance of ' , sel.! !


!BrowserWindow class methodsFor: 'GUI building' stamp: 'KLG 9/8/2022 14:30:16'!
fullOnClass: aClass selector: aSelector
	"Open a new full browser set to class."

	| browser |
	browser _ Browser new.
	browser setClass: aClass selector: aSelector.
	^ self open: browser label: browser labelString! !

!BrowserWindow class methodsFor: 'instance creation' stamp: 'KLG 9/8/2022 14:37:14'!
openNoSysCat: model label: aString

	^ self new
		model: model;
		buildNoSysCatMorphicWindow;
		setLabel: aString;
		openInWorld! !

