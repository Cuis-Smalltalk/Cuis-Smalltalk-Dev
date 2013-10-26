'From Cuis 4.0 of 16 November 2011 [latest update: #1144] on 21 March 2012 at 6:53:56 pm'!
!classDefinition: 'ChangeSet class' category: #'Tools-Changes'!
ChangeSet class
	instanceVariableNames: 'current installing '!
!classDefinition: #ChangeSorter category: #'Tools-Changes'!
CodeProvider subclass: #ChangeSorter
	instanceVariableNames: 'parent myChangeSet currentClassName currentSelector priorChangeSetList '
	classVariableNames: 'AllChangeSets '
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ChangeSet methodsFor: 'testing' stamp: 'jmv 3/21/2012 17:16'!
           canHavePreambleAndPostscript
	"Don't allow preambles and postscripts in changesets for Packages,
	because packages don't support them. Use prerequisites and class initialize methods instead."
	
	^ self isForBaseSystem! !

!ChangeSet methodsFor: 'testing' stamp: 'jmv 3/21/2012 17:15'!
   isForBaseSystem
	^(name beginsWith: 'ChangesToPackage') not! !


!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 3/21/2012 18:32'!
                installing: aCodePackage do: aBlock

	installing _ aCodePackage packageName.
	aBlock ensure: [ installing _ nil ]! !


!ChangeSorter methodsFor: 'access' stamp: 'jmv 3/21/2012 17:16'!
        currentCanHavePreambleAndPostscript
	^ myChangeSet canHavePreambleAndPostscript! !


!ChangeSorter class methodsFor: 'removing' stamp: 'jmv 3/21/2012 17:43'!
  zapAllChangeSets
	self allChangeSets copy do: [ :cs2 | self removeChangeSet: cs2 ]! !


!ChangeListWindow methodsFor: 'GUI building' stamp: 'jmv 3/21/2012 16:59'!
                             optionalModelButtonTuples

	^#(
		(11		'select all' 			selectAll				'select all entries')
		(14		'deselect all'		deselectAll			'deselect all entries')
		(19		'file in selections' 	fileInSelections		'file in all selected entries')
	)! !

!ChangeListWindow methodsFor: 'menu building' stamp: 'jmv 3/21/2012 16:58'!
       listMenu
	"Fill aMenu up so that it comprises the primary changelist-browser menu"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'change list'.
	aMenu addStayUpIcons.
	aMenu addList: #(
	('fileIn selections'							fileInSelections							'import the selected items into the image'																		model)
	('fileOut selections...	'					fileOutSelections						'create a new file containing the selected items'																model)
	-
	('compare to current'						compareToCurrentVersion			'open a separate window which shows the text differences between the on-file version and the in-image version.' model)
	('toggle diffing (D)'							toggleDiffing							'start or stop showing diffs in the code pane.'																	model)
	-
	('select unchanged methods'				selectUnchangedMethods				'select methods in the file whose in-image versions are the same as their in-file counterparts'					model)
	('select methods equivalent to current'		selectEquivalentMethods				'select methods in the file whose in-image versions have the same behavior as their in-file counterparts'		model)
	('select new methods'						selectNewMethods						'select methods in the file that do not current occur in the image'												model)
	('select methods for absent classes'		selectMethodsForAbsentClasses		'select methods in the file for classes that are not defined in the image'										model)
	('select methods for this class'				selectMethodsForThisClass			'select all methods in the file that belong to the currently-selected class'										model)
	('select removals of sent methods'			selectRemovalsOfSent					'select all method removals of methods that have some sender in the image'									model)
	-
	('select all (a)'								selectAll									'select all the items in the list'																					model)
	('deselect all'								deselectAll								'deselect all the items in the list'																				model)
	('invert selections'							invertSelections							'select every item that is not currently selected, and deselect every item that *is* currently selected'		model)
	-
	('browse all versions of single selection'	browseVersions							'open a version browser showing the versions of the currently selected method')
	('browse current versions of selections'	browseCurrentVersionsOfSelections 'open a message-list browser showing the current (in-image) counterparts of the selected methods')
	('destroy current methods of selections'	destroyCurrentCodeOfSelections	'remove (*destroy*) the in-image counterparts of all selected methods'										model)
	-
	('remove doIts'								removeDoIts								'remove all items that are doIts rather than methods'															model)
	('remove older versions'					removeOlderMethodVersions			'remove all but the most recent versions of methods in the list'												model)
	('remove up-to-date versions'				removeExistingMethodVersions		'remove all items whose code is the same as the counterpart in-image code'									model)
	('remove empty class comments'			removeEmptyClassComments			'remove all empty class comments'																			model)
	('remove selected items'					removeSelections						'remove the selected items from the change-list'																model)
	('remove unselected items'					removeNonSelections					'remove all the items not currently selected from the change-list'												model)).
	^ aMenu! !


!ChangeSet methodsFor: 'testing' stamp: 'jmv 3/21/2012 17:26'!
   methodsWithoutClassifications
	"Return a collection representing methods in the receiver which have not been categorized"

	| slips notClassified |

	notClassified _ {'as yet unclassified' asSymbol. #all}.
	slips _ OrderedCollection new.
	self changedClasses do: [ :aClass |
		(self methodChangesAtClass: aClass name) associationsDo: [ :mAssoc | | aSelector |
			(aClass selectors includes:  (aSelector _ mAssoc key)) ifTrue: [
				(notClassified includes: (aClass organization categoryOfElement: aSelector))
					ifTrue: [slips add: aClass name , ' ' , aSelector]]]].
	^ slips

	"
	Smalltalk browseMessageList: (ChangeSet changeSetForBaseSystem methodsWithoutClassifications) name: 'unclassified methods'
	"! !

!ChangeSet methodsFor: 'testing' stamp: 'jmv 3/21/2012 17:34'!
                        okayToRemove
	"Should suggest a fileOut if changeSet is dirty... Implement this!!"
	^ "self okayToRemoveInforming: true" true! !

!ChangeSet methodsFor: 'moving changes' stamp: 'jmv 3/21/2012 17:26'!
                        methodsWithInitialsOtherThan: myInits
	"Return a collection of method refs whose author appears to be different from the given one"
	| slips |
	slips _ OrderedCollection new.
	self changedClasses do: [ :aClass |
		(self methodChangesAtClass: aClass name) associationsDo: [ :mAssoc | | method |
				(#(remove addedThenRemoved) includes: mAssoc value) ifFalse: [
					method _ aClass compiledMethodAt: mAssoc key ifAbsent: nil.
					method ifNotNil: [ | aTimeStamp |
						((aTimeStamp _ Utilities timeStampForMethod: method) notNil and: [
							(aTimeStamp beginsWith: myInits) not])
								ifTrue: [slips add: aClass name , ' ' , mAssoc key]]]]].
	^ slips

	"
	Smalltalk browseMessageList: (ChangeSet changeSetForBaseSystem methodsWithInitialsOtherThan: 'sw') name: 'authoring problems'
	"! !

!ChangeSet methodsFor: 'moving changes' stamp: 'jmv 3/21/2012 17:27'!
                              methodsWithoutComments
	"Return a collection representing methods in the receiver which have no precode comments"

	| slips |
	slips _ OrderedCollection new.
	self changedClasses do:
		[:aClass |
		(self methodChangesAtClass: aClass name) associationsDo: 
				[:mAssoc | (#(remove addedThenRemoved) includes: mAssoc value) ifFalse:
					[(aClass selectors includes:  mAssoc key) ifTrue:
						[(aClass firstPrecodeCommentFor: mAssoc key) isEmptyOrNil
								ifTrue: [slips add: aClass name , ' ' , mAssoc key]]]]].
	^ slips

	"
	Smalltalk browseMessageList: (ChangeSet changeSetForBaseSystem methodsWithoutComments) name: 'methods lacking comments'
	"! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 3/21/2012 17:31'!
             checkForConversionMethods
	"See if any conversion methods are needed"
	| tell choice list smart renamed listAdd listDrop msgSet |

	Preferences conversionMethodsAtFileOut ifFalse: [^ self].	"Check preference"
	structures ifNil: [^ self].

	list _ OrderedCollection new.
	renamed _ OrderedCollection new.
	self changedClasses do: [ :class | | oldStruct newStruct need sel rec |
		need _ (self atClass: class includes: #new) not.
		need ifTrue: ["Renamed classes."
			(self atClass: class includes: #rename) ifTrue: [
				rec _ changeRecords at: class name.
				rec priorName ifNotNil: [
					(structures includesKey: rec priorName) ifTrue: [
						renamed add: class.  need _ false]]]].
		need ifTrue: [need _ (self atClass: class includes: #change)].
		need ifTrue: [oldStruct _ structures at: class name 
									ifAbsent: [need _ false.  #()]].
		need ifTrue: [
			newStruct _ (Array with: class classVersion), (class allInstVarNames).
			need _ (oldStruct ~= newStruct)].
		need ifTrue: [sel _ #convertToCurrentVersion:refStream:.
			(#(add change) includes: (self atSelector: sel class: class)) ifFalse: [
				list add: class]].
		].

	list isEmpty & renamed isEmpty ifTrue: [^ self].
	"Ask user if want to do this"
	tell _ 'If there might be instances of ', (list asArray, renamed asArray) printString,
		'\in a project (.pr file) on someone''s disk, \please ask to write a conversion method.\' withNewLines,
		'After you edit the conversion method, you''ll need to fileOut again.\' withNewLines,
		'The preference conversionMethodsAtFileOut in category "fileout" controls this feature.'.
	choice _ (PopUpMenu labels: 
'Write a conversion method by editing a prototype
These classes are not used in any object file.  fileOut my changes now.
I''m too busy.  fileOut my changes now.
Don''t ever ask again.  fileOut my changes now.') startUpWithCaption: tell. 
	choice = 4 ifTrue: [Preferences disable: #conversionMethodsAtFileOut].
	choice = 2 ifTrue: ["Don't consider this class again in the changeSet"
			list do: [:cls | structures removeKey: cls name ifAbsent: nil].
			renamed do: [:cls | | nn |
				nn _ (changeRecords at: cls name) priorName.
				structures removeKey: nn ifAbsent: nil]].
	choice ~= 1 ifTrue: [^ self].	"exit if choice 2,3,4"

	listAdd _ self askAddedInstVars: list.	"Go through each inst var that was added"
	listDrop _ self askRemovedInstVars: list.	"Go through each inst var that was removed"
	list _ (listAdd, listDrop) asSet asArray.

	smart _ SmartRefStream on: (RWBinaryOrTextStream on: '12345').
	smart structures: structures.
	smart superclasses: superclasses.
	"We assume that it is ok to store any conversion methods in whatever changeSet the class should go in!!"
	"
	(restore _ ChangeSet current) == self ifFalse: [
		self class newChanges: self].
	"
	msgSet _ smart conversionMethodsFor: list.
		"each new method is added to self (a changeSet).  Then filed out with the rest."
	self askRenames: renamed addTo: msgSet using: smart.	"renamed classes, add 2 methods"
"	restore == self ifFalse: [self class newChanges: restore]."
	msgSet messageList isEmpty ifTrue: [^ self].
	self inform: 'Remember to fileOut again after modifying these methods.'.
	MessageSetWindow open: msgSet label: 'Conversion methods for ', self name.! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 3/21/2012 17:07'!
                          preambleString
	"Answer the string representing the preamble"

	^ preamble
		ifNotNil: [ preamble actualContents asString ]! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 3/21/2012 17:27'!
                              preambleTemplate
	"Answer a string that will form the default contents for a change set's preamble.
	Just a first stab at what the content should be."

	^ String streamContents: [:strm |
		strm nextPutAll: '"Change Set:'.  "NOTE: fileIn recognizes preambles by this string."
		strm tab;tab; nextPutAll: self name.
		strm newLine; nextPutAll: 'Date:'; tab; tab; tab; nextPutAll: Date today printString.
		strm newLine; nextPutAll: 'Author:'; tab; tab; tab; nextPutAll: Preferences defaultAuthorName.
		strm newLine; newLine; nextPutAll: '<your descriptive text goes here>"']
"
ChangeSet changeSetForBaseSystem preambleTemplate
"! !


!ChangeSorterWindow methodsFor: 'GUI building' stamp: 'jmv 3/21/2012 17:38'!
                          buildMorphicWindow
	"Add a set of change sorter views to the given top view offset by the given amount. To create a single change sorter, call this once with an offset of 0@0. To create a dual change sorter, call it twice with offsets of 0@0 and 0.5@0."

	| list1 list2 list3 upperPanes |
	model myChangeSet ifNil: [
		self flag: #ojo. "Or whatever was last changed, or is top of list, or whatever"
		model myChangeSet: ChangeSet changeSetForBaseSystem ].

	list1 _ (PluggableListMorphByItem
				model: model
				listGetter: #changeSetList
				indexGetter: #currentCngSet
				indexSetter: #showChangeSetNamed:
				mainView: self
				menuGetter: #changeSetMenu
				keystrokeAction: #changeSetListKey:from:)
			autoDeselect: false.

	list2 _ PluggableListMorphByItem
				model: model
				listGetter: #classList
				indexGetter: #currentClassName
				indexSetter: #currentClassName:
				mainView: self
				menuGetter: #classListMenu
				keystrokeAction: #classListKey:from:.

	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: list1 proportionalWidth: 0.5;
		addAdjusterAndMorph: list2 proportionalWidth: 0.5.

	list3 _ PluggableListMorphByItem
				model: model
				listGetter: #messageList
				indexGetter: #currentSelector
				indexSetter: #currentSelector:
				mainView: self
				menuGetter: #messageMenu
				keystrokeAction: #messageListKey:from:.

	self layoutMorph
		addMorph: upperPanes proportionalHeight: 0.25;
		addAdjusterAndMorph: list3 proportionalHeight: 0.25;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.5.

	self setLabel: model labelString! !

!ChangeSorterWindow methodsFor: 'menu building' stamp: 'jmv 3/21/2012 17:50'!
                               changeSetMenu
	"Set up aMenu to hold commands for the change-set-list pane.  This could be for a single or double changeSorter"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: model.
	aMenu title: 'Change Set'.
	aMenu addStayUpIcons.

	aMenu add: 'rename change set (r)' 			action: #rename.
	aMenu add: 'file out (o)' 						action: #fileOut.
	model currentCanHavePreambleAndPostscript ifTrue: [
		aMenu addLine.
		model currentHasPreamble
			ifTrue: [
				aMenu add: 'edit preamble (p)' 			action: #addPreamble.
				aMenu add: 'remove preamble' 			action: #removePreamble]
			ifFalse: [aMenu add: 'add preamble (p)' 		action: #addPreamble].
		model currentHasPostscript
			ifTrue: [
				aMenu add: 'edit postscript...' 			action: #editPostscript.
				aMenu add: 'remove postscript' 		action: #removePostscript]
			ifFalse: [aMenu add: 'add postscript...' 		action: #editPostscript].
	].
	aMenu addLine.
	aMenu add: 'destroy change set (x)' 			action: #remove.
	aMenu addLine.
	aMenu add: 'more...' 							target: self				action: #offerShiftedChangeSetMenu.
	^ aMenu! !

!ChangeSorterWindow methodsFor: 'menu building' stamp: 'jmv 3/21/2012 18:38'!
  classListMenu
	"Fill aMenu with items appropriate for the class list"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu title: 'class list'.
	aMenu addStayUpIcons.
	aMenu addList: #(
			-
			('delete class from change set (d)'		forgetClass					''		model)
			('remove class from system (x)'		removeClass					''		model)
			-
			('browse full (b)'						browseMethodFull)
			('browse hierarchy (h)'					browseHierarchy)
			('browse protocol (p)'					browseFullProtocol)
			-
			('inst var refs...'							browseInstVarRefs)
			('inst var defs...'						browseInstVarDefs)
			('class var refs...'						browseClassVarRefs)
			('class vars'								browseClassVariables)
			('class refs (N)'							browseClassRefs)).
	^ aMenu! !

!ChangeSorterWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 3/21/2012 17:50'!
                changeSetListKey: aChar from: view
	"Respond to a Command key.  I am a model with a listView that has a list of changeSets."

	aChar == $D ifTrue: [^ model toggleDiffing]. 
	aChar == $o ifTrue: [^ model fileOut].
	aChar == $r ifTrue: [^ model rename].
	aChar == $x ifTrue: [^ model remove].

	^ self messageListKey: aChar from: view! !


!ChangeSet class methodsFor: 'instance creation' stamp: 'jmv 3/21/2012 18:35'!
                               new

	self error: 'ChangeSets are created calling #existingOrNewChangeSetNamed:'! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 18:15'!
                  classAdded: aClass inCategory: aCategoryName

	(self changeSetForSystemCategory: aCategoryName) ifNotNil: [ :changeSet |
		changeSet classAdded: aClass inCategory: aCategoryName ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 18:11'!
               classCommented: aClass

	(self changeSetForClass: aClass) ifNotNil: [ :changeSet |
		changeSet classCommented: aClass ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 18:12'!
           classDefinitionChangedFrom: oldClass to: newClass

	"In case the class is moved from one package to another, both change sets should be affected.
	But there's no need to do it here, as #classRecategorized:from:to: is also called."
	(self changeSetForClass: newClass) ifNotNil: [ :changeSet |
		changeSet classDefinitionChangedFrom: oldClass to: newClass ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 18:41'!
                               classRecategorized: aClass from: oldCategory to: newCategory

	| changeSet |
	"Ambos deberian ser afectados?"
	changeSet _ self changeSetForSystemCategory: oldCategory.
	changeSet print.

	changeSet _ self changeSetForSystemCategory: newCategory.
	changeSet print.
	self flag: #ojo.
	"Atencion, afectar los 2 solo si son distintas!! (NO las categorias, sino los changesets!!!!!!!!!!)"
	changeSet ifNotNil: [
		changeSet classRecategorized: aClass from: oldCategory to: newCategory ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 18:15'!
classRemoved: aClass fromCategory: aCategoryName

	(self changeSetForSystemCategory: aCategoryName) ifNotNil: [ :changeSet |
		changeSet classRemoved: aClass fromCategory: aCategoryName ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 18:16'!
       classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	(self changeSetForSystemCategory: aCategoryName) ifNotNil: [ :changeSet |
		changeSet classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 18:12'!
   classReorganized: aClass

	(self changeSetForClass: aClass) ifNotNil: [ :changeSet |
		changeSet classReorganized: aClass ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 18:13'!
       methodAdded: aMethod selector: aSymbol inClass: aClass requestor: requestor

	(self changeSetForMethod: aMethod) ifNotNil: [ :changeSet |
		changeSet methodAdded: aMethod selector: aSymbol inClass: aClass requestor: requestor ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 18:13'!
                               methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass requestor: requestor

	(self changeSetForMethod: aMethod) ifNotNil: [ :changeSet |
		changeSet methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass requestor: requestor ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 18:14'!
               methodChangedFrom: oldMethod to: newMethod selector: aSymbol inClass: aClass requestor: requestor

	(self changeSetForMethod: newMethod) ifNotNil: [ :changeSet |
		changeSet methodChangedFrom: oldMethod to: newMethod selector: aSymbol inClass: aClass requestor: requestor ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 18:14'!
                 methodRemoved: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass

	(self changeSetForMethodCategory: aCategoryName ofClass: aClass) ifNotNil: [ :changeSet |
		changeSet methodRemoved: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass ]! !

!ChangeSet class methodsFor: 'system change notifications' stamp: 'jmv 3/21/2012 18:41'!
                       selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass

	"Ambos deberian ser afectados?"
	| changeSet |
	changeSet _ self changeSetForMethodCategory: oldCategory ofClass: aClass.
	oldCategory print.
	changeSet print.

	changeSet _ self changeSetForMethodCategory: newCategory ofClass: aClass.
	newCategory print.
	changeSet print.
self flag: #ojo.
"ojo pueden ser nil (p.ej. during install)"
	"Atencion, afectar los 2 solo si son distintas (no las categorias, sino los changesets!!!!!!!!!!)!!"
	changeSet ifNotNil: [
		changeSet selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass ]! !

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 3/21/2012 18:30'!
             changeSetForPackage: aCodePackage

	| csName |
	csName _ installing = aCodePackage packageName
		ifTrue: [ 'InstallPackage', installing ]
		ifFalse: ['ChangesToPackage', aCodePackage name].
	^ChangeSorter existingOrNewChangeSetNamed: csName! !


!CodeFileBrowserWindow methodsFor: 'menu building' stamp: 'jmv 3/21/2012 18:04'!
                         codeFileListMenu

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addList: #(
		('find class... (f)'				findClass)
		-
		('fileIn'						fileIn								''		model)
		('fileOut'					fileOut							''		model)
		('remove existing'			removeUnmodifiedClasses		''		model)).
	^ aMenu! !


!CodePackage class methodsFor: 'searching' stamp: 'jmv 3/21/2012 10:47'!
                   packageOfMethodCategory: categoryName ofClass: aClass ifNone: errorBlock

	^ InstalledPackages
		detect: [:ea | ea includesMethodCategory: categoryName ofClass: aClass]
		ifNone: errorBlock! !


!CodePackageFile methodsFor: 'initialize' stamp: 'jmv 3/21/2012 17:38'!
                     installFileStream: aFileStream packageName: pkName fullName: fullFileName

	self buildFileStream: aFileStream packageName: pkName fullName: fullFileName.
	aFileStream reset.
	self install: aFileStream! !

!CodePackageFile methodsFor: 'services' stamp: 'jmv 3/21/2012 18:22'!
             install: aFileStream
	"Installs the package. Replace all existing code in the PackageInfo, removing any code that is not included in us."
	| localName newCodePackage |

	"Give reasonable warnings if there is stuff that can't be properly cleaned. Give the user the chance to abort."
	'=============' print.
	('classesToRemove: ', classesToRemove printString) print.
	'=============' print.
	'methodsToRemove: ' print.
	methodsToRemove do: [ :methodReference | methodReference print ].
	'=============' print.
	
	"Tirar warning si hay que borrar cosas que no se puede, si hay referencias, etc. Quizas si vamos a borrar el ultimo implementor de un mensaje enviado?"

	"Crear, instalar y devolver una instancia de PackageInfo"
	newCodePackage _ CodePackage newNamed: packageName.
	CodePackage register: newCodePackage.

	"Esto crea el change set y carga ahi. OJO. En ese CS, hay que borrar todo lo que hay que borrar"
	"These were created in #fromFileStream: ... reuse?"
	localName _ FileDirectory localNameFor: fullName.
	ChangeSorter installing: newCodePackage do: [
		aFileStream fileInAnnouncing: 'Installing ', localName, '...'.
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ].
		Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine ].
	
	"Tirar undeclared al transcript. warning si quedaron undeclared
	Es feo que tire an transcript undeclareds que despues no lo son..."
	Smalltalk cleanOutUndeclared.
	Undeclared print.

	"Descartar la instancia de CodePackageFile"
	^newCodePackage! !


!CodeProvider methodsFor: 'categories' stamp: 'jmv 3/21/2012 17:39'!
                  changeCategory
	"Present a menu of the categories of messages for the current class, 
	and let the user choose a new category for the current message"

	self selectedClassOrMetaClass ifNotNil: [ :cls |
		self selectedMessageName ifNotNil: [ :sel |
			(self letUserReclassify: sel in: cls) ifTrue: [
				self methodCategoryChanged]]]! !


!ChangeSorter methodsFor: 'access' stamp: 'jmv 3/21/2012 17:35'!
             labelString
	"The label for my entire window.  The large button that displays my name is gotten via mainButtonName"

	^ 'ChangeSet: ', myChangeSet name! !

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 3/21/2012 17:36'!
                           removePrompting: doPrompt
	"Completely destroy my change set.  Check if it's OK first, and if doPrompt is true, get the user to confirm his intentions first."

	| message aName changeSetNumber msg |

	"Tiene sentido? Preguntar cosas? Sugerir hacer fileOut?"
	self flag: #ojo.

	aName _ myChangeSet name.
	myChangeSet okayToRemove ifFalse: [^ self]. "forms current changes for some project"
	(myChangeSet isEmpty or: [doPrompt not]) ifFalse:
		[message _ 'Are you certain that you want to 
remove (destroy) the change set
named  "', aName, '" ?'.
		(self confirm: message) ifFalse: [^ self]].

	doPrompt ifTrue:
		[msg _ myChangeSet hasPreamble
			ifTrue:
				[myChangeSet hasPostscript
					ifTrue:
						['a preamble and a postscript']
					ifFalse:
						['a preamble']]
			ifFalse:
				[myChangeSet hasPostscript
					ifTrue:
						['a postscript']
					ifFalse:
						['']].
		msg isEmpty ifFalse:
			[(self confirm: 
'Caution!!  This change set has
', msg, ' which will be
lost if you destroy the change set.
Do you really want to go ahead with this?') ifFalse: [^ self]]].

	"Go ahead and remove the change set"
	changeSetNumber _ myChangeSet name initialIntegerOrNil.
	changeSetNumber ifNotNil: [SystemVersion current unregisterUpdate: changeSetNumber].
	ChangeSorter removeChangeSet: myChangeSet.

	self flag: #ojo. "O el que corresponda"
	self showChangeSet: ChangeSet changeSetForBaseSystem.! !

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 3/21/2012 17:56'!
                         update
	"recompute all of my panes"

	self updateIfNecessary! !

!ChangeSorter methodsFor: 'changeSet menu' stamp: 'jmv 3/21/2012 17:36'!
                      updateIfNecessary
	"Recompute all of my panes."

	| newList |

	myChangeSet ifNil: [^ self].  "Has been known to happen though shouldn't"
	myChangeSet isMoribund ifTrue: [
		self changed: #changeSetList.
		^ self showChangeSet: ChangeSet changeSetForBaseSystem ].

	newList _ self changeSetList.
	(priorChangeSetList == nil or: [priorChangeSetList ~= newList])
		ifTrue: [
			priorChangeSetList _ newList.
			self changed: #changeSetList]! !


!ChangeSorter class methodsFor: 'class initialization' stamp: 'jmv 3/21/2012 17:19'!
               initialize
	"Initialize the class variables"

	AllChangeSets
		ifNil: [AllChangeSets _ OrderedCollection new].

	"ChangeSorter initialize"

	FileList registerFileReader: self.
! !

!ChangeSorter class methodsFor: 'fileIn/Out' stamp: 'jmv 3/21/2012 18:03'!
services

	^ {}

	! !


!PasteUpMorph methodsFor: 'world menu' stamp: 'jmv 3/21/2012 17:23'!
   findAChangeSorter: evt
	"Locate a change sorter, open it, and bring it to the front.  Create one if necessary"
	self
		findAWindowSatisfying: [ :aWindow |
			aWindow model isMemberOf: ChangeSorter]
		orMakeOneUsing: [ ChangeSorterWindow open: ChangeSorter new label: nil ]! !


!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 3/21/2012 17:40'!
                               checkForPreamble: chunk
	"As we don't support preambles and postscripts in Packages, assume any preamble or postscript belongs in the BaseSystem.
	Note: In packages, replace preamble by prerequisites, and postscript by class initialize methods."

	| changeSet newPreamble newPostscript |
	changeSet _ ChangeSet changeSetForBaseSystem.
	(chunk beginsWith: '"Change Set:')
		ifTrue: [
			newPreamble _ changeSet preambleString
				ifNil: [ chunk ]
				ifNotNil: [ :oldPreamble |
					oldPreamble, '.', String newLine, chunk ].
			changeSet preambleString: newPreamble.
			'Preamble added to ChangeSet ', changeSet name.
			].
	(chunk beginsWith: '"Postscript:')
		ifTrue: [
			newPostscript _ changeSet postscriptString
				ifNil: [ chunk ]
				ifNotNil: [ :oldPostscript |
					oldPostscript, '.', String newLine, chunk ].
			changeSet postscriptString: newPostscript.
			'Postscript added to ChangeSet ', changeSet name.
			].
							
! !


!GZipReadStream class methodsFor: 'fileIn/Out' stamp: 'jmv 3/21/2012 18:04'!
      fileReaderServicesForFile: fullName suffix: suffix 
	| services |
	(suffix = 'gz') | (suffix = '*')
		ifFalse: [^ #()].
	services _ OrderedCollection new.
	(suffix = '*') | (fullName asLowercase endsWith: '.cs.gz')
		ifTrue: [
			services add: self serviceFileIn ].
	services addAll: self services.
	^ services! !


!SmartRefStream methodsFor: 'import image segment' stamp: 'jmv 3/21/2012 17:21'!
                   mapClass: newClass origName: originalName
	"See if instances changed shape.  If so, make a fake class for the old shape and return it.  Remember the original class name."

	| newName oldInstVars fakeClass |
	newClass isMeta ifTrue: [^ newClass].
	newName _ newClass name.
	(steady includes: newClass) & (newName == originalName) ifTrue: [^ newClass].
		"instances in the segment have the right shape"
	oldInstVars _ structures at: originalName ifAbsent: [
			self error: 'class is not in structures list'].	"Missing in object file"
	fakeClass _ Object subclass: ('Fake37', originalName) asSymbol
		instanceVariableNames: oldInstVars allButFirst
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Obsolete'.
	ChangeSet changeSetForBaseSystem removeClassChanges: fakeClass name.	"reduce clutter"
	^ fakeClass
! !


!SystemChangeNotifier methodsFor: 'private' stamp: 'jmv 3/21/2012 09:37'!
                               triggerEvent: anEventSelector withArguments: anArgumentList

	self isBroadcasting ifTrue: [
		"Solo un par de pruebas..."
		(anEventSelector printString, ' -> ', anArgumentList printString) print.
		^super triggerEvent: anEventSelector withArguments: anArgumentList ]! !


!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 3/21/2012 17:43'!
       reduceCuis
	"
	Smalltalk reduceCuis
	"
	| keep n unused newDicts oldDicts |

	self nominallyUnsent: #reduceCuis.
	
	"Remove icons"
	ClassicTheme beCurrent.
	World backgroundImageData: nil.
	Preferences useNoIcons.
	Theme current initialize.
	Theme content: nil.
	Color shutDown.
	FormCanvas clearFormsCache.

	Transcript clear.
	Clipboard default initialize.
	
	Smalltalk removeClassNamed: #ColorPickerMorph.
	Smalltalk removeClassNamed: #SketchMorph.

	"Remove some methods, even if they have senders."
"	ColorPickerMorph class removeSelector: #buildEyedropperIcon."
	CursorWithAlpha class removeSelector: #buildBiggerNormal.
"	SketchMorph class removeSelector: #buildPaintingIcon."
	Theme removeSelector: #miscellaneousIcons.
	Utilities removeSelector: #vmStatisticsReportString.
	SystemDictionary removeSelector: #recreateSpecialObjectsArray.

	World submorphsDo: [ :a | a delete ].
	StrikeFont removeMostFonts.
	StrikeFont saveSpace.
	Smalltalk garbageCollect.

	"????
	Smalltalk organization removeCategoriesMatching: 'Signal Processing*'.
	SystemOrganization removeSystemCategory: 'LinearAlgebra'.
	Smalltalk organization removeCategoriesMatching: 'Sound-*'
	"

	Beeper setDefault: nil.
	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.

	keep := OrderedCollection new.
	keep addAll: #(ZipConstants GZipConstants ZipFileConstants ChronologyConstants SpaceTally).
	unused := Smalltalk unusedClasses copyWithoutAll: keep.
	[
		#hereWeGo print.
		unused do: [:c | 
			c print.
			(Smalltalk at: c) removeFromSystem]. 
		n := Smalltalk removeAllUnSentMessages.
		unused := Smalltalk unusedClasses copyWithoutAll: keep.
		n > 0 or: [ 
			unused notEmpty ]] whileTrue.
	ChangeSorter zapAllChangeSets.
	Smalltalk garbageCollect.


	Smalltalk organization removeEmptyCategories.
	Symbol rehash.

	"Shrink method dictionaries."
	Smalltalk garbageCollect.
	oldDicts _ MethodDictionary allInstances.
	newDicts _ Array new: oldDicts size.
	oldDicts withIndexDo: [:d :index | 
		newDicts at: index put: d rehashWithoutBecome ].
	oldDicts elementsExchangeIdentityWith: newDicts.
	oldDicts _ newDicts _ nil.

   "Sanity checks"
"   Undeclared
   Smalltalk cleanOutUndeclared
   Smalltalk browseUndeclaredReferences
   Smalltalk obsoleteClasses
   Smalltalk obsoleteBehaviors 
   Smalltalk browseObsoleteMethodReferences
   SmalltalkImage current fixObsoleteReferences
   Smalltalk browseAllUnimplementedCalls"! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 3/21/2012 18:46'!
             changesMenu
        "Build the changes menu for the world."

	| menu |
	menu _ self menu: 'Changes...'.
	self fillIn: menu from: {
		{ 'Change Sorter' . {self. #openChangeSorter1}.  'Open a 3-paned changed-set viewing tool'}.
		nil.

		{ 'Browse my Changes' . { Smalltalk . #browseMyChanges }.
				'Browse all of my changes since the last time #condenseSources was run.'}.
		{ 'Browse recent Submissions' . { #myWorld . #openRecentSubmissionsBrowser:}.
				'Make an open recent-submissions browser be the front-window, expanding a collapsed one or creating a new one if necessary.  A recent-submissions browser is a message-list browser that shows the most recent methods that have been submitted, latest first.  If you submit changes within that browser, it will keep up-to-date, always showing the most recent submissions at the top of the browser.'}.

		{ 'Recently logged Changes...' . { ChangeList . #browseRecentLog}.'Open a change-list browser on the latter part of the changes log.  You can use this browser to recover logged changes which were not saved in your image, in the event of a crash or other interruption.'}.

		nil.
		{ 'Save World as morph file' . {self. #saveWorldInFile}. 'Save a file that, when reloaded, reconstitutes the current World.'}.
	}.
	^ menu! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 3/21/2012 18:47'!
                        openMenu
	"Build the open window menu for the world."

	| menu |
	menu _ self menu: 'Open...'.

	ExtraOpenCommands ifNotNil: [
		self fillIn: menu from: ExtraOpenCommands ].

	self fillIn: menu from: {
		{'Browser' . { self . #openBrowser}. 'A five-paned tool that lets you see all the code in the system'}.
		{'Workspace' . {self . #openWorkspace}. 'A window for evaluating Smalltalk expressions' }.
		{'Text Editor' . {self . #openTextEditor}. 'A window for composing text' }.
		{'File List' . {self . #openFileList} . 'A tool allowing you to browse any file' }.
		{'Transcript' . {self . #openTranscript}. 'A window used to report messages sent to Transcript' }.
		{ 'Process Browser' . { ProcessBrowserWindow . #openProcessBrowser } }.
		{ 'Emergency Evaluator'. { Transcripter. #emergencyEvaluator } }.
		"{'inner world' . { WorldWindow . #test1} }."
		nil.
		{'Message Names' . { self . #openMessageNames} . 'A tool for finding and editing methods that contain any given keyword in their names.'}.
		nil.
		{'Change Sorter' . {self . #openChangeSorter1} . 'A tool allowing you to view the methods in a Change Set' }.
		nil.
		{'SUnit Test Runner' . {TestRunnerWindow . #openTestRunner} . 'A tool allowing you to compare and manipulate two change sets concurrently' }.
	}.
	^menu! !


!Theme methodsFor: 'menus' stamp: 'jmv 3/21/2012 18:47'!
                             miscellaneousIcons

	"Everything else."

	^ {
		#('restore display (r)' 'set display depth...' 'move windows onscreen' 'Utilities saveDisplay.') -> #displayIcon.
		#('changes...' 'dual change sorter' 'change sets with this method' 'find a change sorter' 'recent changes in file' 'Undo / Redo history') -> #changesIcon.
		#('print PS to file...' ) -> #printIcon.
		#('find again (g)' 'full stack (k)') -> #systemIcon.
		#('print it (p)' 'check change set for slips') -> #printIcon.
		#('accept (s)' 'make changes go to me (m)') -> #acceptIcon.
		#('cancel (l)' ) -> #cancelIcon.
		#('debug...' 'debug it' 'toggle break on entry') -> #debugIcon.
		#('close' 'close all debuggers' 'close top window') -> #closeIcon.
		#('collapse' 'hide taskbar' 'collapse all windows') -> #collapseIcon.
		#('expand / contract' 'show taskbar' 'expand all windows') -> #expandIcon.
		#('menu') -> #windowMenuIcon.
		#('browse all' 'browser' 'browse it (b)' 'MessageTally UI and browse' 'browse recent submissions' 'browse full (b)' 'find changed browsers...' 'browse (b)' 'browse my changes') -> #editFindReplaceIcon.
		#('workspace' 'workspace with contents') -> #terminalIcon.
		#('styled text editor' 'text editor' 'edit this list' 'edit postscript...' 'add postscript...') -> #textEditorIcon.
		#('file list' 'find a fileList') -> #systemFileManagerIcon.
		#('transcript' 'find a transcript' 'Transcript clear.' 'log to transcript') -> #printerIcon.
		#('process browser' 'vm statistics' 'MessageTally all Processes') -> #systemMonitorIcon.
		#('emergency evaluator' 'conflicts with other change sets' 'check for slips' 'conflicts with change set opposite' 'conflicts with category opposite') -> #emblemImportantIcon.
		#('change sorter') -> #halfRefreshIcon.
		#('SUnit Test Runner') -> #weatherFewCloudsIcon.
		#('system fonts...' 'set font... (k)') -> #preferencesDesktopFontIcon.
		#('full screen on') -> #viewFullscreenIcon.
		#('full screen off') -> #exitFullscreenIcon.
		#('set desktop color...') -> #wallpaperIcon.
		#('preferences...' 'what to show...') -> #preferencesIcon.
		#('command-key help') -> #keyboardShortcutsIcon.
		#('world menu help') -> #globeIcon.
		#('useful expressions' 'class comments with it' 'check for uncommented methods' 'check for uncommented classes') -> #chatIcon.
		#('set author initials...' 'check for other authors' 'check for any other authors') -> #usersIcon.
		#('space left') -> #removableMediaIcon.
		#('start drawing all again' 'window color...') -> #graphicsIcon.
		#('start stepping again') -> #mediaPlaybackStartIcon.
		#('file out current change set' 'fileOut' 'file out (o)') -> #fileOutIcon.
		#('recently logged changes...' 'versions (v)' 'recent classes... (r)' 'trim history' 'profile messages (m)') -> #clockIcon.
		#('senders of it (n)' 'senders of... (n)' 'local senders of...' 'senders (n)') -> #mailForwardIcon.
		#('implementors of it (m)' 'implementors of... (m)' 'implementors of sent messages') -> #developmentIcon.
		#('references to it (N)') -> #addressBookIcon.
		#('class var refs...' 'class refs (N)' 'class variables' 'class vars' 'local implementors of...' 'subclass template') -> #classIcon.
		#('inst var refs...' 'inst var defs...' 'sample instance' 'inspect Pointers (P)') -> #instanceIcon.
		#('Use Selection for Find (h)' 'rename class ...' 'rename...' 'change title...') -> #saveAsIcon.
		#('method source with it' 'browse method (O)' 'check for uncategorized methods') -> #scriptIcon.
		#('method strings with it (E)') -> #genericTextIcon.
		#('browse hierarchy (h)' 'move to top' 'promote to top of list') -> #goTopIcon.
		#('move up' 'make next-to-topmost') -> #goUpIcon.
		#('move to bottom' 'send to back' 'send top window to back') -> #goBottomIcon.
		#('inheritance (i)' 'move down') -> #goDownIcon.
		#('browse protocol (p)' 'spawn sub-protocol') -> #spreadsheetIcon.
		#('spawn full protocol') -> #speadsheetTemplateIcon.
		#('alphabetize') -> #fontXGenericIcon.
		#('browse' 'show category (C)' 'categorize all uncategorized' 'select change set...' 'view affected class categories') -> #packageIcon.
		#('remove from current change set' 'remove empty categories' 'subtract other side (-)' 'remove from this browser') -> #listRemoveIcon.
		#('add to current change set' 'add all meths to current chgs' 'add preamble (p)') -> #listAddIcon.
		#('toggle diffing (D)' 'toggle selections') -> #switchIcon.
		#('reorganize' 'create inst var accessors' 'ChangeSorter reorderChangeSets.' 'reorder all change sets' 'by name' 'by size' 'by date') -> #sendReceiveIcon.
		#('unsent methods' 'unreferenced class vars' 'unreferenced inst vars' 'Undeclared inspect.' 'Undeclared removeUnreferencedKeys; inspect.' 'ChangeSorter removeEmptyUnnamedChangeSets.' 'check for unsent messages') -> #junkIcon.
		#('update' 'turn on auto-update (a)' 'update list (u)') -> #updateIcon.
		#('find changed windows...') -> #newWindowIcon.
		#('make undraggable') -> #pushPinIcon.
		#('Utilities saveScreenshot.') -> #stillCameraIcon.
		#('add new directory') -> #newFolderIcon.
		#('select all' 'deselect all') -> #selectAllIcon.
		#('sort by date') -> #dateIcon.
		#('justified') -> #formatJustifyFillIcon.
		#('centered') -> #formatJustifyCenterIcon.
		#('set alignment...' 'leftFlush') -> #formatJustifyLeftIcon.
		#('rightFlush') -> #formatJustifyRightIcon.
		#('signal Semaphore (S)') -> #haloHelpIcon.
		#('Change Paragraph Style...' 'Change Character Style...' 'Remove Character Style' 'Replace all uses of Paragraph Style...' 'Replace all uses of Character Style...') -> #fontXGenericIcon.
	}! !

!methodRemoval: TheWorldMenu #openChangeSorter2!
TheWorldMenu removeSelector: #openChangeSorter2!
!methodRemoval: SystemDictionary #lastRemoval!
SystemDictionary removeSelector: #lastRemoval!
!methodRemoval: GZipReadStream class #installChangeSet:!
GZipReadStream class removeSelector: #installChangeSet:!
!methodRemoval: GZipReadStream class #serviceInstall!
GZipReadStream class removeSelector: #serviceInstall!
!methodRemoval: FileStream #fileIntoNewChangeSet!
FileStream removeSelector: #fileIntoNewChangeSet!
!methodRemoval: CodeFileBrowser #fileIntoNewChangeSet!
CodeFileBrowser removeSelector: #fileIntoNewChangeSet!
!methodRemoval: ChangeSorter class #assuredChangeSetNamed:!
ChangeSorter class removeSelector: #assuredChangeSetNamed:!
!methodRemoval: ChangeSorter class #basicNewChangeSet:!
ChangeSorter class removeSelector: #basicNewChangeSet:!
!methodRemoval: ChangeSorter class #basicNewChangeSetLike:!
ChangeSorter class removeSelector: #basicNewChangeSetLike:!
!methodRemoval: ChangeSorter class #gatherChangeSets!
ChangeSorter class removeSelector: #gatherChangeSets!
!methodRemoval: ChangeSorter class #installChangeSet:!
ChangeSorter class removeSelector: #installChangeSet:!
!methodRemoval: ChangeSorter class #newChangeSet!
ChangeSorter class removeSelector: #newChangeSet!
!methodRemoval: ChangeSorter class #newChangeSet:!
ChangeSorter class removeSelector: #newChangeSet:!
!methodRemoval: ChangeSorter class #newChangesFromStream:named:!
ChangeSorter class removeSelector: #newChangesFromStream:named:!
!methodRemoval: ChangeSorter class #secondaryChangeSet!
ChangeSorter class removeSelector: #secondaryChangeSet!
!methodRemoval: ChangeSorter class #serviceInstall!
ChangeSorter class removeSelector: #serviceInstall!
ChangeSorter initialize!
!methodRemoval: ChangeSorter #checkThatSidesDiffer:!
ChangeSorter removeSelector: #checkThatSidesDiffer:!
!methodRemoval: ChangeSorter #copyAllToOther!
ChangeSorter removeSelector: #copyAllToOther!
!methodRemoval: ChangeSorter #copyClassToOther!
ChangeSorter removeSelector: #copyClassToOther!
!methodRemoval: ChangeSorter #copyMethodToOther!
ChangeSorter removeSelector: #copyMethodToOther!
!methodRemoval: ChangeSorter #fileOutClass!
ChangeSorter removeSelector: #fileOutClass!
!methodRemoval: ChangeSorter #methodConflictsWithOtherSide!
ChangeSorter removeSelector: #methodConflictsWithOtherSide!
!methodRemoval: ChangeSorter #moveClassToOther!
ChangeSorter removeSelector: #moveClassToOther!
!methodRemoval: ChangeSorter #moveMethodToOther!
ChangeSorter removeSelector: #moveMethodToOther!
!methodRemoval: ChangeSorter #newCurrent!
ChangeSorter removeSelector: #newCurrent!
!methodRemoval: ChangeSorter #newSet!
ChangeSorter removeSelector: #newSet!
!methodRemoval: ChangeSorter #parent!
ChangeSorter removeSelector: #parent!
!methodRemoval: ChangeSorter #parent:!
ChangeSorter removeSelector: #parent:!
!methodRemoval: ChangeSorter #submergeIntoOtherSide!
ChangeSorter removeSelector: #submergeIntoOtherSide!
!methodRemoval: ChangeSorter #subtractOtherSide!
ChangeSorter removeSelector: #subtractOtherSide!
!classDefinition: #ChangeSorter category: #'Tools-Changes'!
CodeProvider subclass: #ChangeSorter
	instanceVariableNames: 'myChangeSet currentClassName currentSelector priorChangeSetList'
	classVariableNames: 'AllChangeSets'
	poolDictionaries: ''
	category: 'Tools-Changes'!
!methodRemoval: ChangeList #selectAllConflicts!
ChangeList removeSelector: #selectAllConflicts!
!methodRemoval: ChangeList #selectConflicts!
ChangeList removeSelector: #selectConflicts!
!methodRemoval: ChangeList #selectConflicts:!
ChangeList removeSelector: #selectConflicts:!
!methodRemoval: ChangeList #selectConflictsWith!
ChangeList removeSelector: #selectConflictsWith!
!methodRemoval: ClassChangeRecord #assimilateAllChangesIn:!
ClassChangeRecord removeSelector: #assimilateAllChangesIn:!
!methodRemoval: ClassChangeRecord #forgetChangesIn:!
ClassChangeRecord removeSelector: #forgetChangesIn:!
!methodRemoval: ChangeSet class #current!
ChangeSet class removeSelector: #current!
!methodRemoval: ChangeSet class #defaultName!
ChangeSet class removeSelector: #defaultName!
!methodRemoval: ChangeSet class #uniqueNameLike:!
ChangeSet class removeSelector: #uniqueNameLike:!
!methodRemoval: Class #removeFromChanges!
Class removeSelector: #removeFromChanges!
!methodRemoval: ChangeSet #absorbClass:from:!
ChangeSet removeSelector: #absorbClass:from:!
!methodRemoval: ChangeSet #absorbMethod:class:from:!
ChangeSet removeSelector: #absorbMethod:class:from:!
!methodRemoval: ChangeSet #absorbStructureOfClass:from:!
ChangeSet removeSelector: #absorbStructureOfClass:from:!
!methodRemoval: ChangeSet #adoptSelector:forClass:!
ChangeSet removeSelector: #adoptSelector:forClass:!
!methodRemoval: ChangeSet #assimilateAllChangesFoundIn:!
ChangeSet removeSelector: #assimilateAllChangesFoundIn:!
!methodRemoval: ChangeSet #atSelector:class:put:!
ChangeSet removeSelector: #atSelector:class:put:!
!methodRemoval: ChangeSet #compileAll:from:!
ChangeSet removeSelector: #compileAll:from:!
!methodRemoval: ChangeSet #containsClass:!
ChangeSet removeSelector: #containsClass:!
!methodRemoval: ChangeSet #expungeEmptyClassChangeEntries!
ChangeSet removeSelector: #expungeEmptyClassChangeEntries!
!methodRemoval: ChangeSet #forgetAllChangesFoundIn:!
ChangeSet removeSelector: #forgetAllChangesFoundIn:!
!methodRemoval: ChangeSet #forgetChangesForClass:in:!
ChangeSet removeSelector: #forgetChangesForClass:in:!
!methodRemoval: ChangeSet #hasAnyChangeForSelector:!
ChangeSet removeSelector: #hasAnyChangeForSelector:!
!methodRemoval: ChangeSet #noteChangeClassCategory:!
ChangeSet removeSelector: #noteChangeClassCategory:!
!methodRemoval: ChangeSet #okayToRemoveInforming:!
ChangeSet removeSelector: #okayToRemoveInforming:!
!methodRemoval: ChangeSet #removeClassAndMetaClassChanges:!
ChangeSet removeSelector: #removeClassAndMetaClassChanges:!
!methodRemoval: ChangeSet #setPreambleToSay:!
ChangeSet removeSelector: #setPreambleToSay:!
!methodRemoval: ChangeSet #structures!
ChangeSet removeSelector: #structures!
!methodRemoval: ChangeSet #summaryString!
ChangeSet removeSelector: #summaryString!
!methodRemoval: ChangeSet #summaryStringDelta:!
ChangeSet removeSelector: #summaryStringDelta:!
!methodRemoval: ChangeSet #superclasses!
ChangeSet removeSelector: #superclasses!
!classRemoval: #DualChangeSorter!
Smalltalk removeClassNamed: #DualChangeSorter!
!classRemoval: #DualChangeSorterWindow!
Smalltalk removeClassNamed: #DualChangeSorterWindow!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Theme current class beCurrent!

