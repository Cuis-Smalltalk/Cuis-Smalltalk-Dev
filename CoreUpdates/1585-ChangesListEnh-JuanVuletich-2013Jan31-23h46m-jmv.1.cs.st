'From Cuis 4.1 of 12 December 2012 [latest update: #1584] on 31 January 2013 at 11:47:48 pm'!

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 1/31/2013 23:06'!
removeUpToDate
	"Remove all up to date version of entries from the receiver"

	Cursor wait showWhile: [
		| newChangeList newList |
		newChangeList _ OrderedCollection new.
		newList _ OrderedCollection new.

		changeList with: list do: [ :chRec :strNstamp | | keep cls name |
			keep _ chRec isClassDeletion not or: [ chRec changeClass notNil ]. "If a class deletion, and class already gone, don't keep it"
			keep ifTrue: [
				(cls _ chRec changeClass) ifNotNil: [ | sel str |
					str _ chRec string.
					sel _ chRec methodSelector.
					keep _ (cls sourceCodeAt: sel ifAbsent:['']) asString ~= str.
				].
			].
			(chRec changeType == #classComment and: [
					name _ chRec changeClassName.
					Smalltalk includesKey: name]) ifTrue: [
				cls _ Smalltalk at: name.
				keep _ cls organization classComment ~= chRec text.
			].
			(chRec changeType == #classDefinition and: [
					name _ chRec changeClassName.
					Smalltalk includesKey: name]) ifTrue: [
				cls _ Smalltalk at: name.
				chRec isMetaClassChange ifTrue: [ cls _ cls class ].
				keep _ cls definition ~= chRec text.
			].
			keep ifTrue:[
				newChangeList add: chRec.
				newList add: strNstamp]].
		newChangeList size < changeList size ifTrue: [
			changeList _ newChangeList.
			list _ newList.
			listIndex _ 0.
			self clearSelections ].
	].
	self changed: #list! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 1/31/2013 23:19'!
selectAllForAbsentClasses
	"Selects all method definitions for which there is no counterpart method in the current image"

	Cursor read showWhile: [
		| change |
		1 to: changeList size do: [ :i | 
			change _ changeList at: i.
			listSelections at: i put:
				((#(method classComment classDefinition) includes: change changeType)
					and: [ change changeClass isNil ])]].
	self changed: #allSelections.
	self changed: #annotation! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 1/31/2013 23:07'!
selectAllForThisClass
	| name |
	self currentChange ifNil: [ ^self ].
	name _ self currentChange changeClassName.
	name ifNil: [ ^self ].
	^self selectSuchThat: [ :change |
		change changeClassName = name ].! !


!ChangeRecord methodsFor: 'access' stamp: 'jmv 1/31/2013 23:15'!
changeClass
	| methodClassName methodClass |
	(#(method classComment classDefinition) includes: type) ifFalse: [ ^ nil ].
	methodClassName _ class substrings
		ifEmpty: [ ^ nil ]
		ifNotEmpty:
			[ : parts | parts first asSymbol ].
	(Smalltalk includesKey: methodClassName) ifFalse: [ ^ nil ].
	methodClass _ Smalltalk at: methodClassName.
	^ meta
		ifTrue: [ methodClass class ]
		ifFalse: [ methodClass ]! !

!ChangeRecord methodsFor: 'access' stamp: 'jmv 1/31/2013 22:55'!
changeClassName
	^class! !

!ChangeRecord methodsFor: 'access' stamp: 'jmv 1/31/2013 22:41'!
changeType
	^ type! !


!ClassDeletionChangeRecord methodsFor: 'accessing' stamp: 'jmv 1/31/2013 22:59'!
changeClass
	^Smalltalk at: clsName ifAbsent: nil! !

!ClassDeletionChangeRecord methodsFor: 'accessing' stamp: 'jmv 1/31/2013 22:55'!
changeClassName
	^clsName! !

!ClassDeletionChangeRecord methodsFor: 'accessing' stamp: 'jmv 1/31/2013 22:41'!
changeType
	^ #classDefinition! !


!MethodDeletionChangeRecord methodsFor: 'accessing' stamp: 'jmv 1/31/2013 22:59'!
changeClass
	^methodReference actualClass! !

!MethodDeletionChangeRecord methodsFor: 'accessing' stamp: 'jmv 1/31/2013 22:55'!
changeClassName
	^methodReference classIsMeta
		ifFalse: [ methodReference classSymbol ]
		ifTrue: [ methodReference classSymbol, ' class' ]! !

!MethodDeletionChangeRecord methodsFor: 'accessing' stamp: 'jmv 1/31/2013 22:42'!
changeType
	^ #method! !


!SHRange methodsFor: 'accessing' stamp: 'jmv 1/31/2013 22:40'!
rangeType
	^type! !


!VariableNode methodsFor: 'testing' stamp: 'jmv 1/31/2013 22:41'!
varNodeType
	"This code attempts to reconstruct the type from its encoding in code.
		This allows one to test, for instance, (aNode type = LdInstType)."
	| type |
	code < 0 ifTrue: [^code negated].
	code >= 256 ifTrue: [^code // 256].
	type := CodeBases findFirst: [:one | code < one].
	^type = 0 ifTrue: [5] ifFalse: [type - 1]! !


!ChangeList methodsFor: 'menu actions' stamp: 'jmv 1/31/2013 23:00'!
compareToCurrentVersion
	"If the current selection corresponds to a method in the system, then spawn a window showing the diffs as text"

	| change class s1 s2 differDesc diffWords |
	listIndex = 0
		ifTrue: [^ self].
	change _ changeList at: listIndex.
	((class _ change changeClass) notNil
			and: [class includesSelector: change methodSelector])
		ifTrue: [
			s1 _ (class sourceCodeAt: change methodSelector) asString.
			s2 _ change string.
			s1 = s2
				ifTrue: [^ self inform: 'Exact Match'].
			diffWords _ self shouldDiffWords.
			differDesc _ diffWords
				ifTrue: [ 'Words']
				ifFalse: [ 'Lines'].
			(TextModel
				withText: (
					(DifferenceFinder
						displayPatchFrom: s1 to: s2
						tryWords: diffWords
						prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [class]))
							font: Preferences standardCodeFont))
				openLabel: 'Comparison to Current Version: ', differDesc, 
					(self showingAnyKindOfPrettyDiffs ifTrue: [', using prettyPrint'] ifFalse: [''])]
		ifFalse: [self flash]! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 1/31/2013 23:00'!
currentVersionsOfSelections
	"Opens a message-list browser on the current in-memory versions of all methods that are currently seleted"
	| aList |

	aList _ OrderedCollection new.
	1 to: changeList size do: [ :i |
		(listSelections at: i) ifTrue: [
			| aClass aChange |
			aChange _ changeList at: i.
			(aChange changeType == #method
				and: [(aClass _ aChange changeClass) notNil
				and: [aClass includesSelector: aChange methodSelector]])
					ifTrue: [
						aList add: (
							MethodReference new
								setStandardClass: aClass  
								methodSymbol: aChange methodSelector
						)
					]]].
	^ aList! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 1/31/2013 23:00'!
destroyCurrentCodeOfSelections
	"Actually remove from the system any in-memory methods with class and selector identical to items current selected.  This may seem rather arcane but believe me it has its great uses, when trying to split out code.  To use effectively, first file out a change set that you wish to split off.  Then open a ChangeList browser on that fileout.  Now look through the methods, and select any of them which you want to remove completely from the system, then issue this command.  For those methods where you have made changes to pre-existing versions, of course, you won't want to remove them from the system, so use this mechanism with care!!"

	|  aClass aChange aList |
	aList _ OrderedCollection new.
	1 to: changeList size do:
		[:index |
			(listSelections at: index) ifTrue:
				[aChange _ changeList at: index.
				(aChange changeType == #method
					and: [(aClass _ aChange changeClass) notNil
					and: [aClass includesSelector: aChange methodSelector]])
						ifTrue:
							[aList add: {aClass. aChange methodSelector}]]].

	aList size > 0 ifTrue: [
		(self confirm: 'Warning!! This will actually remove ', aList size printString,  ' method(s) from the system!!') ifFalse: [^ self]].
	aList do: [ :aPair |
		Transcript newLine; show: 'Removed: ', aPair first printString, '.', aPair second.
		aPair first removeSelector: aPair second ]! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 1/31/2013 22:52'!
removeDoIts
	"Remove doits from the receiver, other than initializes. 1/26/96 sw"

	Cursor wait showWhile: [
		| newChangeList newList |
		newChangeList _ OrderedCollection new.
		newList _ OrderedCollection new.

		changeList with: list do: [ :chRec :str |
			(chRec changeType ~~ #doIt or: [str endsWith: 'initialize'])
				ifTrue: [
					newChangeList add: chRec.
					newList add: str]].
		newChangeList size < changeList size
			ifTrue: [
				changeList _ newChangeList.
				list _ newList.
				listIndex _ 0.
				self clearSelections ].
	].
	self changed: #list.

	! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 1/31/2013 22:55'!
removeEmptyClassComments

	Cursor wait showWhile: [
		| newChangeList newList |
		newChangeList _ OrderedCollection new.
		newList _ OrderedCollection new.
		changeList with: list do: [ :chRec :strNstamp | | keep |
			keep _ true.
			(chRec changeType == #classComment and: [
					Smalltalk includesKey: chRec changeClassName]) ifTrue: [
				keep _ chRec text notEmpty
			].
			keep ifTrue:[
				newChangeList add: chRec.
				newList add: strNstamp]].
		newChangeList size < changeList size ifTrue: [
			changeList _ newChangeList.
			list _ newList.
			listIndex _ 0.
			self clearSelections ].
	].
	self changed: #list! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 1/31/2013 23:00'!
selectEquivalentMethods
	"Selects all method definitions for which there is already an equivalent method in the current image, 
	(meaning that the difference is cosmetic and not in behavior)"
	Cursor wait showWhile: [
		1 to: changeList size do: [ :i | 
			| change class |
			change _ changeList at: i.
			listSelections at: i put:
				((change changeType == #method and: [
					(class _ change changeClass) notNil]) and: [
						(class includesSelector: change methodSelector) and: [
							| cmWithNode |
							cmWithNode _ [class basicCompile: change string notifying: nil trailer: class defaultMethodTrailer ifFail: nil] 
								on: SyntaxErrorNotification do: [ :ex | ex return ].
							(cmWithNode notNil and: [
								| current inChange |
								current _ (class compiledMethodAt: change methodSelector) copyWithTrailerBytes: #(0).
								inChange _ cmWithNode method copyWithTrailerBytes: #(0).
								current = inChange or: [
									| currentCmWithNode |
									currentCmWithNode _ [class basicCompile: (class decompilerClass new decompile: change methodSelector in: class) decompileString
											notifying: nil trailer: class defaultMethodTrailer ifFail: nil] on: SyntaxErrorNotification do: [ :ex | ex return ].
									(currentCmWithNode notNil and: [
										current _ currentCmWithNode method copyWithTrailerBytes: #(0).
										current = inChange])
								]
							])
						]]
				)]].
	self changed: #allSelections.
	self changed: #annotation! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 1/31/2013 23:00'!
selectNewMethods
	"Selects all method definitions for which there is no counterpart method in the current image"

	Cursor read showWhile: [
		| change class |
		1 to: changeList size do: [ :i | 
			change _ changeList at: i.
			listSelections at: i put:
				((change changeType == #method and:
					[((class _ change changeClass) isNil) or:
						[(class includesSelector: change methodSelector) not]]))]].
	self changed: #allSelections.
	self changed: #annotation! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 1/31/2013 22:52'!
selectRemovalsOfSent
	"Selects all method removal for sent methods"

	Cursor read showWhile: [
		1 to: changeList size do: [ :i | | change |
			change _ changeList at: i.
			listSelections at: i put:
				(change changeType = #doIt and: [
					change string includesSubString: 'removeSelector: #' ] and: [
						Smalltalk isThereAReferenceTo: (change string copyAfterLast: $#) asSymbol ]) ]].
	self changed: #allSelections.
	self changed: #annotation! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 1/31/2013 23:00'!
selectUnchangedMethods
	"Selects all method definitions for which there is already a method in the current image, whose source is exactly the same.  9/18/96 sw"

	Cursor read showWhile: [
		| change class |
		1 to: changeList size do: [ :i | 
			change _ changeList at: i.
			listSelections at: i put:
				((change changeType == #method and:
					[(class _ change changeClass) notNil]) and:
						[(class includesSelector: change methodSelector) and:
							[change string = (class sourceCodeAt: change methodSelector) asString ]])]].
	self changed: #allSelections.
	self changed: #annotation! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 1/31/2013 23:00'!
xtraVersion
	| change class |
	listIndex = 0
		ifTrue: [^ nil ].
	change _ changeList at: listIndex.
	((class _ change changeClass) notNil
			and: [class includesSelector: change methodSelector])
		ifFalse: [ ^nil ].
	^change! !

!ChangeList methodsFor: 'viewing access' stamp: 'jmv 1/31/2013 23:00'!
annotation
	"Answer the string to be shown in an annotation pane.  Make plain that the annotation is associated with the current in-image version of the code, not of the selected disk-based version, and if the corresponding method is missing from the in-image version, mention that fact."

	| change count selectedCount ann1 ann2 aClass |
	change _ self currentChange.
	
	change isNil ifTrue: [
		count _ listSelections size.
		selectedCount _ listSelections count: [ :flag | flag ].
		^ 'Total items: ', count printString, ' - Selected items: ', selectedCount printString ].

	change changeType == #classDefinition ifTrue: [
		ann1 _ change isMetaClassChange ifTrue: [ 'Metaclass' ] ifFalse: [ 'Class' ].
		ann2 _ (Smalltalk includesKey: change changeClassName) ifTrue: [ ' already exists' ] ifFalse: [ ' not in system' ].
		^ann1, ann2 ].
	
	(self selectedMessageName isNil or: [self selectedClassOrMetaClass isNil])
		ifTrue: [^ '------'].

	^ change methodSelector notNil
		ifFalse: [ super annotation]
		ifTrue: [
			((aClass _ change changeClass) isNil or: [(aClass includesSelector: change methodSelector) not])
				ifTrue: [
					change changeClassName, ' >> ', change methodSelector, ' is not present in the system.']
				ifFalse: [
					'current version: ', super annotation]]! !

!ChangeList methodsFor: 'viewing access' stamp: 'jmv 1/31/2013 23:00'!
contentsDiffedFromCurrent
	"Answer the contents diffed forward from current (in-memory) method version"

	|  aChange aClass  name aSelector |
	listIndex = 0
		ifTrue: [^ ''].
	aChange _ changeList at: listIndex.
	((aChange changeType == #method 
			and: [(aClass _ aChange changeClass) notNil]) 
			and: [aClass includesSelector: aChange methodSelector]) ifTrue: [
		aSelector _ aChange methodSelector.
		(aClass notNil and: [aClass includesSelector: aSelector])
			ifFalse: [ ^aChange text copy ].
		^DifferenceFinder
				displayPatchFrom: (aClass sourceCodeAt: aSelector)
				to: aChange text
				tryWords: self shouldDiffWords
				prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [aClass]) ].
	(aChange changeType == #classDefinition and: [
			name _ aChange changeClassName.
			Smalltalk includesKey: name]) ifTrue: [
		aClass _ Smalltalk at: name.
		aChange isMetaClassChange ifTrue: [ aClass _ aClass class ].
		^DifferenceFinder
				displayPatchFrom: aClass definition to: aChange text tryWords: true].
	^(changeList at: listIndex) text! !

!ChangeList methodsFor: 'viewing access' stamp: 'jmv 1/31/2013 23:00'!
diffedVersionContents
	"Answer diffed version contents, maybe pretty maybe not"

	| change class earlier later |
	(listIndex = 0
			or: [changeList size < listIndex])
		ifTrue: [^ ''].
	change _ changeList at: listIndex.
	later _ change text.
	class _ change changeClass.
	(listIndex = changeList size or: [class == nil])
		ifTrue: [^ later].

	earlier _ (changeList at: listIndex + 1) text.

	^DifferenceFinder
		displayPatchFrom: earlier to: later
		tryWords: self shouldDiffWords
		prettyPrintedIn: (self showingAnyKindOfPrettyDiffs ifTrue: [class])! !

!ChangeList methodsFor: 'viewing access' stamp: 'jmv 1/31/2013 23:00'!
selectedClassOrMetaClass
	| c |
	^ (c _ self currentChange) ifNotNil: [c changeClass]! !


!ChangeListWindow methodsFor: 'menu building' stamp: 'jmv 1/31/2013 23:17'!
listMenu
	"Fill aMenu up so that it comprises the primary changelist-browser menu"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'change list'.
	aMenu addStayUpIcons.
	aMenu addList: #(
	('fileIn selections'							fileInSelections							
			'import the selected items into the image'																		model)
	('fileOut selections...	'					fileOutSelections						
			'create a new file containing the selected items'																model)
	-
	('compare to current'						compareToCurrentVersion			
			'open a separate window which shows the text differences between the on-file version and the in-image version.' model)
	('toggle diffing (D)'							toggleDiffing							
			'start or stop showing diffs in the code pane.'																	model)
	-
	('select unchanged methods'				selectUnchangedMethods				
			'select methods in the file whose in-image versions are the same as their in-file counterparts'					model)
	('select methods equivalent to current'		selectEquivalentMethods				
			'select methods in the file whose in-image versions have the same behavior as their in-file counterparts'		model)
	('select new methods'						selectNewMethods						
			'select methods in the file that do not current occur in the image'												model)
	('select changes for absent classes'		selectAllForAbsentClasses		
			'select methods in the file for classes that are not defined in the image'										model)
	('select all changes for this class'			selectAllForThisClass			
			'select all methods in the file that belong to the currently-selected class'										model)
	('select removals of sent methods'			selectRemovalsOfSent					
			'select all method removals of methods that have some sender in the image'									model)
	-
	('select all (a)'								selectAll									
			'select all the items in the list'																					model)
	('deselect all'								deselectAll								
			'deselect all the items in the list'																				model)
	('invert selections'							invertSelections							
			'select every item that is not currently selected, and deselect every item that *is* currently selected'		model)
	-
	('browse all versions of single selection'	browseVersions							
			'open a version browser showing the versions of the currently selected method')
	('browse current versions of selections'	browseCurrentVersionsOfSelections 
			'open a message-list browser showing the current (in-image) counterparts of the selected methods')
	('destroy current methods of selections'	destroyCurrentCodeOfSelections	
			'remove (*destroy*) the in-image counterparts of all selected methods'										model)
	-
	('remove doIts'								removeDoIts								
			'remove all items that are doIts rather than definitions'															model)
	('remove older versions'					removeOlderMethodVersions			
			'remove all but the most recent versions of methods in the list'												model)
	('remove up-to-date versions'				removeUpToDate		
			'remove all items whose code is the same as the counterpart in-image code'									model)
	('remove empty class comments'			removeEmptyClassComments			
			'remove all empty class comments'																			model)
	('remove selected items'					removeSelections						
			'remove the selected items from the change-list'																model)
	('remove unselected items'					removeNonSelections					
			'remove all the items not currently selected from the change-list'												model)).
	^ aMenu! !


!ChangeRecord methodsFor: 'access' stamp: 'jmv 1/31/2013 22:55'!
fileOutOn: aFileStream
	"File the receiver out on the given file stream"

	| aString |
	type == #method
		ifTrue: [
			aFileStream newLine; nextPut: $!!.
			aString _  class asString
							, (meta ifTrue: [' class methodsFor: ']
									ifFalse: [' methodsFor: '])
							, category asString printString.
			stamp ifNotNil: [
				aString _ aString, ' stamp: ''', stamp, ''''].
			aFileStream nextChunkPut: aString.
			aFileStream newLine ].

	type == #preamble ifTrue: [ aFileStream nextPut: $!! ].

	type == #classComment
		ifTrue: [
			aFileStream nextPut: $!!.
			aFileStream nextChunkPut: class asString, ' commentStamp: ', stamp storeString.
			aFileStream newLine ].
		
	type == #classDefinition ifTrue: [
		aFileStream nextPut: $!!.
		aFileStream nextChunkPut: 
			'classDefinition: ', 
			(self isMetaClassChange ifTrue: [self changeClassName, ' class'] ifFalse: [self changeClassName]) printString,
			' category: ', self category printString.
		aFileStream newLine ].

	aFileStream nextChunkPut: self string.
	
	type == #method ifTrue: [ aFileStream nextChunkPut: ' '; newLine ].
	type == #classComment ifTrue: [ aFileStream newLine ].
	aFileStream newLine! !

!ChangeRecord methodsFor: 'access' stamp: 'jmv 1/31/2013 23:01'!
originalChangeSetForSelector: methodSelector
	"Returns the original changeset which contained this method version.  If it is contained in the .sources file, return #sources.  If it is in neither (e.g. its changeset was deleted), return nil.  (The selector is passed in purely as an optimization.)"

	| likelyChangeSets originalChangeSet |
	(file localName findTokens: '.') last = 'sources'
		ifTrue: [^ #sources].
	likelyChangeSets _ ChangeSorter allChangeSets select: [ :cs |
		(cs atSelector: methodSelector class: self changeClass) ~~ #none].
	originalChangeSet _ likelyChangeSets
		detect: [ :cs | cs containsMethodAtPosition: position ]
		ifNone: nil.
	^ originalChangeSet  "(still need to check for sources file)"! !

!ChangeRecord methodsFor: 'initialization' stamp: 'jmv 1/31/2013 23:01'!
fileIn
	"File the receiver in.  If I represent a method or a class-comment, file the method in and make a note of it in the recent-submissions list; if I represent a do-it, then, well, do it."
	Cursor read showWhile: [ | s |
		type == #method ifTrue: [
			self changeClass ifNotNil: [ :methodClass |
				methodClass
					compile: self text
					classified: category
					withStamp: stamp
					notifying: nil.
				self methodSelector ifNotNil: [ :aSelector |
					Utilities
						noteMethodSubmission: aSelector
						forClass: methodClass ]]].
		type == #doIt ifTrue: [
			((s _ self string) beginsWith: '----') ifFalse: [ Smalltalk actualCompilerClass evaluate: s ]].
		type == #classDefinition ifTrue: [ Smalltalk actualCompilerClass evaluate: self string ].
		type == #classComment ifTrue: [ | cls |
			(cls _ Smalltalk at: class asSymbol)
				comment: self text
				stamp: stamp.
			Utilities
				noteMethodSubmission: #Comment
				forClass: cls ]].! !


!ClassDeletionChangeRecord methodsFor: 'fileIn/fileOut' stamp: 'jmv 1/31/2013 23:01'!
fileIn

	doItOnlyIfInBaseSystem
		ifTrue: [
			Smalltalk removeClassNamedIfInBaseSystem: self changeClass name]
		ifFalse: [
			self changeClass removeFromSystem ]! !


!CodeFile methodsFor: 'change record types' stamp: 'jmv 1/31/2013 22:55'!
classComment: chgRec

	(self getClass: chgRec changeClassName) classComment: chgRec! !

!CodeFile methodsFor: 'change record types' stamp: 'jmv 1/31/2013 22:55'!
method: chgRec
	(self getClass: chgRec changeClassName) methodChange: chgRec! !

!CodeFile methodsFor: 'reading' stamp: 'jmv 1/31/2013 22:52'!
buildFrom: aStream
	| chgRec changes |
	changes _ (ChangeList new scanFile: aStream from: 0 to: aStream size) changeList.
	('Processing ', self name) 
		displayProgressAt: Sensor mousePoint
		from: 1
		to: changes size
		during: [ :bar |
			1 to: changes size do:[:i|
				bar value: i.
				chgRec := changes at: i.
				chgRec class == MethodDeletionChangeRecord
					ifTrue: [ self removedMethod: chgRec command with: chgRec ]
					ifFalse: [ self perform: (chgRec changeType copyWith: $:) asSymbol with: chgRec ].
			].
		]! !


!CodePackage methodsFor: 'testing' stamp: 'jmv 1/31/2013 23:01'!
includesChangeRecord: aChangeRecord
	^ aChangeRecord changeClass notNil and:
		[self
			includesMethodCategory: aChangeRecord category
			ofClass: aChangeRecord changeClass]! !


!MethodDeletionChangeRecord methodsFor: 'fileIn/fileOut' stamp: 'jmv 1/31/2013 22:56'!
fileOutOn: stream
	"File the receiver out on the given file stream"
	
	doItOnlyIfInBaseSystem
		ifTrue: [
			stream nextPut: $!!; nextChunkPut: 'methodMoveToSomePackage: ', self changeClassName, ' ', self methodSelector; newLine.
			stream nextChunkPut: self command; newLine ]
		ifFalse: [
			stream nextPut: $!!; nextChunkPut: 'methodRemoval: ', self changeClassName, ' ', self methodSelector; newLine.
			stream nextChunkPut: self command; newLine ]! !

!MethodDeletionChangeRecord methodsFor: 'services' stamp: 'jmv 1/31/2013 22:56'!
command

	^doItOnlyIfInBaseSystem
		ifTrue: [ self changeClassName, ' removeSelectorIfInBaseSystem: ', self methodSelector ]
		ifFalse: [ self changeClassName, ' removeSelector: ', self methodSelector ]! !


!SHParserST80 methodsFor: 'accessing' stamp: 'jmv 1/31/2013 22:42'!
last3Ranges
	| r s lastI |
	s _ ranges size.
	r _ ranges last.
	lastI _ r rangeType = #excessCode
		ifTrue: [ s - 1 ]
		ifFalse: [ s].
	^{ 
		(lastI > 2 ifTrue: [ranges at: lastI-2]).
		(lastI > 1 ifTrue: [ranges at: lastI-1]).
		(ranges at: lastI)
	}! !

!SHParserST80 methodsFor: 'accessing' stamp: 'jmv 1/31/2013 22:42'!
lastRange
	| r i s |
	s _ ranges size.
	s = 0 ifTrue: [ ^nil ].
	r _ ranges last.
	^r rangeType = #excessCode
		ifFalse: [ r ]
		ifTrue: [
			i _ s-1.
			i > 0 ifTrue: [ ranges at: i ]]! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 1/31/2013 22:42'!
replaceStringForRangesWithType: aSymbol with: aString in: aText 
	"Answer aText if no replacements, or a copy of aText with 
	each range with a type of aSymbol replaced by aString"
	| answer toReplace increaseInLength |

	"We don't handle format and conversion for debuggers"
	disableFormatAndConvert ifTrue: [ ^aText ].
	
	toReplace := (self rangesIn: aText setWorkspace: false) 
		select: [:each | each rangeType = aSymbol].
	toReplace isEmpty ifTrue: [^aText].
	answer := aText copy.
	increaseInLength := 0.

	(toReplace asArray sort: [:a :b | a start <= b start]) 
		do: [:each | | end start thisIncrease | 
			start := each start + increaseInLength.
			end := each end + increaseInLength.
			answer replaceFrom: start to: end with: aString.
			thisIncrease := aString size - each length.
			increaseInLength := increaseInLength + thisIncrease ].
	^answer! !

!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 1/31/2013 22:42'!
setAttributesIn: aText fromRanges: ranges in: anInterval
	"modified by jmv to keep existing attributes if they answer true to #isParagraphAttribute"
	| attributes defaultAttributes paragraphAttributes attr newRuns newValues lastAttr lastCount beginning chunkRuns end | 		

	anInterval
		ifNil: [
			beginning _ 1.
			end _ aText size ]
		ifNotNil: [
			beginning _ anInterval first.
			end _ anInterval last ].
	defaultAttributes := self attributesFor: #default.
	paragraphAttributes := Array new: end-beginning+1.
	1 to: paragraphAttributes size do: [ :i |
		paragraphAttributes at: i put:
			((aText attributesAt: i+beginning-1) select: [ :each | each isParagraphAttribute ])].
	attributes := Array new: end-beginning+1.
	1 to: attributes size do: [ :i | attributes at: i put: (paragraphAttributes at: i), defaultAttributes].
	ranges do: [ :range |
		(attr := self attributesFor: range rangeType)
			ifNotNil: [ range start to: range end do: [:i |
				attributes at: i put: (paragraphAttributes at: i), attr]]].
	newRuns := OrderedCollection new: attributes size // 10.
	newValues := OrderedCollection new: attributes size // 10.
	1 to: attributes size do: [:i |
		attr := attributes at: i.
		i = 1 
			ifTrue: [
				newRuns add: 1.
				lastCount := 1.
				lastAttr := newValues add: attr]
			ifFalse:[
				attr == lastAttr
					ifTrue: [
						lastCount := lastCount + 1.
						newRuns at: newRuns size put: lastCount]
					ifFalse: [
						newRuns add: 1.
						lastCount := 1.
						lastAttr := newValues add: attr]]].
	chunkRuns _ (RunArray runs: newRuns values: newValues) coalesce.
	aText
		basicReplaceAttributesFrom: beginning
		to: end
		with: chunkRuns! !


!SmalltalkCompleter methodsFor: 'entries' stamp: 'jmv 1/31/2013 22:44'!
computeEntries

	| allSource contextClass id p last3Ranges range prevRange receiverClass prevPrevRange |
	allSource _ model actualContents string.
	p _ (model is: #hasTextProvider)
		ifTrue: [ model textProvider ]
		ifFalse: [ model ].
	contextClass _ (p respondsTo: #selectedClassOrMetaClass) ifTrue: [
		p selectedClassOrMetaClass ].

	"Instead of creating a new string, maybe we could pass the last position to parse to Shout..."
	parser _ SHParserST80 new.
	parser
		workspace: ((model isMemberOf: Workspace) ifTrue: [ model ]);
		classOrMetaClass: contextClass;
		source: (allSource copyFrom: 1 to: position).
	parser parse.
	last3Ranges _ parser last3Ranges.
	range _ last3Ranges third.
	range ifNil: [ ^entries _ #() ].

	"If parsing breaks before position, then we don't know what to suggest, therefore don't open Completion"
	range end = position ifFalse: [ ^entries _ #() ].

	prefix _ allSource copyFrom: range start to: range end.
	
	(parser isMessage: range rangeType) ifTrue: [
		"If previous range is a constant or a well known identifier, we might filter messages"
		prevRange _ last3Ranges second.
		prevPrevRange _ last3Ranges first.
		receiverClass _ nil.
		"3 if -> ifNil: but not ifTrue:
		3=4 -> ifNil: or ifTrue:"
		(prevRange notNil and: [ prevPrevRange isNil or: [ (#(binary keyword) includes: prevPrevRange rangeType) not]]) ifTrue: [
			id _ (allSource copyFrom: prevRange start to: prevRange end).
			receiverClass _ prevRange rangeType caseOf: {
				[ #globalVar ] -> [ (Smalltalk at: id asSymbol) class ].
				[ #self ] -> [ contextClass ].
				[ #super ] -> [ contextClass superclass ].
				[ #true ] -> [ True ].
				[ #false ] -> [ False ].
				[ #nil ] -> [ UndefinedObject ].
				[ #character ] -> [ id first class ].
				[ #number ] -> [ (Compiler evaluate: id) class ].
				[ #string ] -> [ (Compiler evaluate: id) class ].
				[ #symbol ] -> [ (Compiler evaluate: id) class ].
				[ #stringSymbol ] -> [ (Compiler evaluate: id) class ].
				"thisContext could mean ContextPart or BlockClosure..."
				"[ #thisContext ] -> [ ContextPart ]"
			} otherwise: [ nil ]
		].
		^self computeMessageEntries: receiverClass ].

	(parser isPartialOrFullIdentifier: range rangeType) ifTrue: [
		^self computeIdentifierEntries ].
	
	"If we don't know what to do, do nothing"
	entries _ #()! !


!VariableNode methodsFor: 'testing' stamp: 'jmv 1/31/2013 22:44'!
index
	"This code attempts to reconstruct the index from its encoding in code."
	code < 0 ifTrue:[^nil].
	code > 256 ifTrue:
		[self assert: index = (code \\ 256).
		^code \\ 256].
	code >= (CodeBases at: self varNodeType) ifTrue:
		[self assert: index = (code - (CodeBases at: self varNodeType)).
		^code - (CodeBases at: self varNodeType)].
	self assert: index = (code - self varNodeType).
	^code - self varNodeType! !

!VariableNode methodsFor: 'tiles' stamp: 'jmv 1/31/2013 22:43'!
variableGetterBlockIn: aContext

	| temps idx ivars |
	"Do not call it index, it would be shadowed"
	(self varNodeType = 4 and: [self key isVariableBinding]) ifTrue: [
		^[self key value]
	].
	aContext ifNil: [^nil].
	self isSelfPseudoVariable ifTrue: [^[aContext receiver]].
	self varNodeType = 1 ifTrue: [
		ivars := aContext receiver class allInstVarNames.
		idx := ivars indexOf: self name ifAbsent: [^nil].
		^[aContext receiver instVarAt: idx]
	].
	self varNodeType = 2 ifTrue: [
		temps := aContext tempNames.
		idx := temps indexOf: self name ifAbsent: [^nil].
		^[aContext tempAt: idx]
	].
	^nil
! !

!VariableNode methodsFor: 'code generation (new scheme)' stamp: 'jmv 1/31/2013 22:43'!
emitCodeForStorePop: stack encoder: encoder
	self varNodeType ~= 1 ifTrue:
		[self halt].
	encoder genStorePopInstVar: index.
	stack pop: 1! !

!methodRemoval: VariableNode #type!
VariableNode removeSelector: #type!
!methodRemoval: SHRange #type!
SHRange removeSelector: #type!
!methodRemoval: MethodDeletionChangeRecord #methodClass!
MethodDeletionChangeRecord removeSelector: #methodClass!
!methodRemoval: MethodDeletionChangeRecord #methodClassName!
MethodDeletionChangeRecord removeSelector: #methodClassName!
!methodRemoval: MethodDeletionChangeRecord #type!
MethodDeletionChangeRecord removeSelector: #type!
!methodRemoval: ClassDeletionChangeRecord #methodClass!
ClassDeletionChangeRecord removeSelector: #methodClass!
!methodRemoval: ClassDeletionChangeRecord #methodClassName!
ClassDeletionChangeRecord removeSelector: #methodClassName!
!methodRemoval: ClassDeletionChangeRecord #type!
ClassDeletionChangeRecord removeSelector: #type!
!methodRemoval: ChangeRecord #methodClass!
ChangeRecord removeSelector: #methodClass!
!methodRemoval: ChangeRecord #methodClassName!
ChangeRecord removeSelector: #methodClassName!
!methodRemoval: ChangeRecord #type!
ChangeRecord removeSelector: #type!
!methodRemoval: ChangeList #removeExistingMethodVersions!
ChangeList removeSelector: #removeExistingMethodVersions!
!methodRemoval: ChangeList #selectMethodsForAbsentClasses!
ChangeList removeSelector: #selectMethodsForAbsentClasses!
!methodRemoval: ChangeList #selectMethodsForThisClass!
ChangeList removeSelector: #selectMethodsForThisClass!
