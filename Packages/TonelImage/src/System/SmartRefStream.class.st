"
Ordinary ReferenceStreams assume that the names and order of instance variables is exactly the same when an object file is written and read.  
	SmartRefStream allows object files to be read even after instance variables have changed or the entire class has been renamed.

When an object file is written, no one knows how the classes will change in the future.  Therefore, all conversion must be done when the file is read.  The key is to store enough information in the file about the names of the instance variables of all outgoing classes.  

SmartRefStream works best with only one tree of objects per file.  You can nextPut: more than once, but each object tree gets its own class structure description, which is big.  

Conversion of old objects is done by a method in each class called (convertToCurrentVersion: varDict refStream: smartRefStrm).  At fileOut time, ChangeSet>>checkForConversionMethods creates a prototype of this method (if Preference #conversionMethodsAtFileOut is true).  The programmer must edit this method to (1) test if the incoming object needs conversion, (2) put non-nil values into any new inst vars that need them, and (3) save the data of any inst vars that are being deleted. 

Determining which old version is represented by the incoming object can be done in several ways: noticing that a current inst var is nil when it should have data, noticing that there is an older inst var name in the variable dictionary (varDict), checking kinds of objects in one or more inst vars, or retrieving the classVersion of the incoming object from the ref stream.  

If a class is renamed, a method goes into SmartRefStream telling the new name.  The conversion method of the new class must be prepared to accept instances of the old class also.  If no inst var names have changed, the conversion method does nothing.

An example:  
	Suppose we change the representation of class Rectangle from ('origin' 'corner') to ('origin' 'extent').  Suppose lots of Rectangle instances are already out on files (in .pr project files, especially).  
	The programmer changes the class definition, modifies all the methods, and filesOut.  A series of dialogs appear, asking if instances Rectangle might be in an object file, if 'extent' needs to be non-nil (yes), and if the info in 'corner' needs to be preserved (yes).  This method appears:

Rectangle >> convertToCurrentVersion: varDict refStream: smartRefStrm
	"These variables are automatically stored into the new instance: #('origin').
	Test for this particular conversion.  Get values using expressions like (varDict at: 'foo')."

	"New variables: #('extent').  If a non-nil value is needed, please assign it."
	"These are going away #('corner').  Possibly store their info in some other variable?"
	"Move your code above the ^ super...  Delete extra comments."
	^ super convertToCurrentVersion: varDict refStream: smartRefStrm

The programmer modifies it to be:

Rectangle >> convertToCurrentVersion: varDict refStream: smartRefStrm

(varDict includesKey: 'extent') ifFalse: ["old version!"
	"Create the new extent, and preserve the info from the old corner"
	extent := (varDict at: 'corner') - origin.
	].
^ super convertToCurrentVersion: varDict refStream: smartRefStrm

	This conversion method stays in the system and is ready to convert the old format of Rectangle whenever one is encountered in an object file.  Note that the subclasses of Rectangle, (B3DViewport, CharacterBlock, and Quadrangle) do not need conversion methods.  Their instances will be converted by the code in Rectangle.  

	Files written by SmartRefStream are in standard fileout format.  You can mix raw objects with code to be filed in.  The file starts out in the normal fileOut format.  Definitions of new classes on the front.

structures 	Dictionary of (#Rectangle -> #(<classVersionInteger> 'origin' 'corner')).  Inst 
				var names are strings.
steady 		Set of Classes who have the same structure now as on the incoming file.
				Includes classes with same inst vars except for new ones added on the end.
reshaped 	Dictionary of Classes who have a different structure now from the incoming file.  
				Includes those with same inst vars but new version number.
				(old class name -> method selector to fill in data for version to version)
renamed	Dictionary of Classes who have a different name.  Make an instance of the new
			class, and send it the conversion call.
				(old class name symbol -> new class name).  
renamedConv	Dictionary of conversion selector for Classes who have a different name.
				(old class name symbol -> conversion selector).  
topCall		Tells if next or nextPut: are working on the top object in the tree.  
			nil if outside, the top object if deep inside.

See DataStream.typeIDFor: for where the tangle of objects is clipped, so the whole system will not be written on the file.

No object that is written on the file is ever a class.  All class definitions are filed in.  A class may be stored inside an ImageSegment that itself is stored in a SmartRefStream.

There is a separate subclass for doing veryDeepCopy (in memory).  Currently, any object for which objectToStoreOnDataStream return an object other than self, does this:  The new object (a DiskProxy) is traced.  When it comes time to go through the fields of the old object, they are not found as keys in references (DiskProxies are there instead).  So the old field value is left in the new object.  That is OK for Class, MetaClass, DisplayScreen.  But the DiskProxies are evaluated, which takes a lot of time.

Some metaclasses are put into the structures table.  This is for when a block has a receiver that is a class.  See checkFatalReshape:.

ImageSegments:
	A ReferenceStream is used to enumerate objects to put inside an ImageSegment.
	A SmartRefStream is used to store the ImageSegment.  Roots are nil, and the segment is a wordArray.  We are encoding the outPointers.  Structures contains all classes from both places.

--Ted Kaehler and Bob Arning.

"
Class {
	#name : 'SmartRefStream',
	#superclass : 'ReferenceStream',
	#instVars : [
		'structures',
		'steady',
		'renamed',
		'renamedConv',
		'superclasses',
		'progressBar',
		'objCount'
	],
	#category : 'System-Object Storage',
	#package : 'System',
	#tag : 'Object Storage'
}

{ #category : 'as yet unclassified' }
SmartRefStream class >> cleanUpCategories [
	| list valid removed newList newVers |
	"Look for all conversion methods that can't be used any longer.  Delete them."
	" SmartRefStream cleanUpCategories "

	"Two part selectors that begin with convert and end with a digit."
	"convertasossfe0: varDict asossfeu0: smartRefStrm"
	list := Symbol selectorsContaining: 'convert'.
	list := list select: [:symb | (symb beginsWith: 'convert') & (symb allButLast last isDigit)
				ifTrue: [(symb numArgs = 2)]
				ifFalse: [false]].
	valid := 0.  removed := 0.
	list do: [:symb |
		(Smalltalk allClassesImplementing: symb) do: [:newClass |
			newList := (Array with: newClass classVersion), (newClass allInstVarNames).
			newVers := self new versionSymbol: newList.
			(symb endsWith: (':',newVers,':')) 
				ifFalse: [
					"method is useless because can't convert to current shape"
					newClass removeSelector: symb.	"get rid of it"
					removed := removed + 1]
				ifTrue: [valid := valid + 1]]].
	Transcript
		newLine;
		show: 'Removed: ';
		print: removed; 
		show: '		Kept: ';
		print: valid;
		show: ' '
]

{ #category : 'as yet unclassified' }
SmartRefStream class >> read: aByteStream withClasses: structureArray [
	"Read an object off the stream, but first check structureArray against the current system."

	| me |
	me := self on: aByteStream.
	me noHeader.
	me structures: (structureArray at: 2).
	me superclasses: (structureArray at: 4).
	(me verifyStructure = 'conversion method needed') ifTrue: [^ nil].
	^ super next

]

{ #category : 'as yet unclassified' }
SmartRefStream class >> statsOfSubObjects: anObject [
	"Open a window with statistics on what objects would be written out with anObject.  Does not actually write on the disk.  Stats in the form:
	ScriptEditorMorph 51
		SortedCollection (21->LayoutMorph 15->SimpleButtonMorph 9->Array 4->CompoundTileMorph 2->StringMorph )"

	| dummy printOut |
	dummy := ReferenceStream on: (DummyStream on: nil).
		"Write to a fake Stream, not a file"
	"Collect all objects"
	dummy rootObject: anObject.	"inform him about the root"
	dummy nextPut: anObject.
	"(dummy references) is the raw data"
	printOut := dummy statisticsOfRefs.
	(TextModel new contents: printOut) 
		openLabel: 'ReferenceStream statistics'.
]

{ #category : 'as yet unclassified' }
SmartRefStream class >> subObjects: anObject ofClass: aClass [
	"Return a collection of all instances of aClass that would be written out with anObject.  Does not actually write on the disk.  Inspect the result and ask for 'references to this object'."

	| dummy coll |
	dummy := ReferenceStream on: (DummyStream on: nil).
		"Write to a fake Stream, not a file"
	"Collect all objects"
	dummy rootObject: anObject.	"inform him about the root"
	dummy nextPut: anObject.
	coll := OrderedCollection new.
	dummy references keysDo: [:each |
		each class == aClass ifTrue: [coll add: each]].
	^ coll
]

{ #category : 'as yet unclassified' }
SmartRefStream class >> tallyOfSubObjects: anObject [
	"Open a window with statistics on what objects would be written out with anObject.  Does not actually write on the disk.  Stats are simply the number of instances of each class:
	1450->Point   835->Rectangle   549->Array   300->String"

	| dummy bag |
	dummy := ReferenceStream on: (DummyStream on: nil).
		"Write to a fake Stream, not a file"
	"Collect all objects"
	dummy rootObject: anObject.	"inform him about the root"
	dummy nextPut: anObject.
	bag := Bag new.
	dummy references keysDo: [:key | bag add: key class name].
	"(bag sortedCounts) is the SortedCollection"
	(TextModel new contents: bag sortedCounts printString) 
		openLabel: 'ReferenceStream statistics'.
]

{ #category : 'import image segment' }
SmartRefStream >> applyConversionMethodsTo: objectIn className: className varMap: varMap [

	| anObject prevObject |

	self flag: #bobconv.	

	anObject := objectIn.
	[
		prevObject := anObject.
		anObject := anObject convertToCurrentVersion: varMap refStream: self.
		prevObject == anObject
	] whileFalse.
	^anObject

]

{ #category : 'class changed shape' }
SmartRefStream >> catalogValues: instVarList size: varsOnDisk [
	"Create a dictionary of (name -> value) for the inst vars of this reshaped object.  Indexed vars as (1 -> val) etc.  "

	| dict sz |
	dict := Dictionary new.
	2 to: instVarList size do: [:ind |
		dict at: (instVarList at: ind) put: self next].
	sz := varsOnDisk - (instVarList size - 1).
	1 to: sz do: [:ii | 
		dict at: ii put: self next].
	"Total number read MUST be equal to varsOnDisk!"
	sz > 0 ifTrue: [dict at: #SizeOfVariablePart put: sz].
	^ dict
]

{ #category : 'import image segment' }
SmartRefStream >> checkFatalReshape: setOfClasses [
	| suspects oldInstVars newInstVars bad className |
	"Inform the user if any of these classes were reshaped.  A block has a method from the old system whose receiver is of this class.  The method's inst var references might be wrong.  OK if inst vars were only added."

	self flag: #bobconv.	

	setOfClasses isEmpty ifTrue: [^ self].
	suspects := OrderedCollection new.
	setOfClasses do: [:aClass |
		className := renamed keyAtValue: aClass name ifAbsent: [aClass name].
		oldInstVars := (structures at: className ifAbsent: [#(0)]) allButFirst.		"should be there"
		newInstVars := aClass allInstVarNames.
		oldInstVars size > newInstVars size ifTrue: [bad := true].
		oldInstVars size = newInstVars size ifTrue: [
			bad := oldInstVars ~= newInstVars].
		oldInstVars size < newInstVars size ifTrue: [
			bad := oldInstVars ~= (newInstVars copyFrom: 1 to: oldInstVars size)].
		bad ifTrue: [suspects add: aClass]].

	suspects isEmpty ifFalse: [
		self inform: ('Imported foreign methods will run on instances of:\',
			suspects asArray printString, 
			'\whose shape has changed.  Errors may occur.') withNewLines].
]

{ #category : 'class changed shape' }
SmartRefStream >> conversionMethodsFor: classList [
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

	^ MethodSet new initializeMessageList: list.
]

{ #category : 'import image segment' }
SmartRefStream >> convert1: misShapenInst to: goodClass allVarMaps: allVarMaps [
	"Go through the normal instance conversion process and return a modern object."

	| className oldInstVars anObject varMap |

	self flag: #bobconv.	

	goodClass isVariable ifTrue: [
		goodClass error: 'shape change for variable class not implemented yet'
	].
	(misShapenInst class name beginsWith: 'Fake37') ifFalse: [self error: 'why mapping?'].
	className := (misShapenInst class name allButFirst: 6) asSymbol.
	oldInstVars := structures at: className.
	anObject := goodClass basicNew.

	varMap := Dictionary new.	"later, indexed vars as (1 -> val) etc."
	2 to: oldInstVars size do: [:ind |
		varMap at: (oldInstVars at: ind) put: (misShapenInst instVarAt: ind-1)].
	varMap at: #ClassName put: className.	"original"
	varMap at: #NewClassName put: goodClass name.	"new"
	self storeInstVarsIn: anObject from: varMap. 	"ones with the same names"
	allVarMaps at: misShapenInst put: varMap.
	^ anObject

]

{ #category : 'import image segment' }
SmartRefStream >> convert2: partiallyCorrectInst allVarMaps: allVarMaps [
	"Go through the normal instance conversion process and return a modern object."

	| className varMap |

	self flag: #bobconv.	

	varMap := allVarMaps at: partiallyCorrectInst.
	className := varMap at: #ClassName.	"original"
	^self applyConversionMethodsTo: partiallyCorrectInst className: className varMap: varMap.


]

{ #category : 'read write' }
SmartRefStream >> initKnownRenames [
	"Stuff like"
	"
	renamed
		at: #FlasherMorph put: #Flasher;
		yourself
	"
	renamed
		at: #FloatArray put: #Float32Array;
		yourself
]

{ #category : 'read write' }
SmartRefStream >> initShapeDicts [
	"Initialize me. "

	self flag: #bobconv.	

	"These must stay constant.  When structures read in, then things can change."
	steady := {Array. Dictionary. Association. String. SmallInteger} asSet.

	renamed ifNil: [
		renamed := Dictionary new.  "(old class name symbol -> new class name)"
		renamedConv := Dictionary new "(oldClassNameSymbol -> conversionSelectorInNewClass)"
	].
	self initKnownRenames
]

{ #category : 'read write' }
SmartRefStream >> instVarInfo: anObject [
	"Return the object to write on the outgoing file that contains the structure of each class we are about to write out.  Must be an Array whose first element is 'class structure'.  Its second element is a Dictionary of pairs of the form #Rectangle -> #(<classVersion> 'origin' 'corner').  "

	"Make a pass through the objects, not writing, but recording the classes.  Construct a database of their inst vars and any version info (classVersion)."

	| dummy refs cls newSupers |
	structures := Dictionary new.
	superclasses := Dictionary new.
	dummy := ReferenceStream on: (DummyStream on: nil).
		"Write to a fake Stream, not a file"
	"Collect all objects"
	dummy rootObject: anObject.	"inform him about the root"
	dummy nextPut: anObject.
	refs := dummy references.
	objCount := refs size.		"for progress bar"
		"Note that Dictionary must not change its implementation!  If it does, how do we read this reading information?"
	refs keysDo: [:each | 
		cls := each class.
		"cls isObsolete ifTrue: [self error: 'Trying to write ', cls name]."
		(cls class ~~ Metaclass) & (cls isObsolete not) ifTrue: [
			structures at: cls name put: false]].
	"Save work by only computing inst vars once for each class"
	newSupers := Set new.
	structures at: #Point put: false.	"writeRectangle: does not put out class pointer"
	structures at: #Rectangle put: false.
	structures at: #LargePositiveInteger put: false.	"used in slow case of WordArray"
	structures keysDo: [:nm | 
		cls := (nm endsWith: ' class') 
			ifFalse: [Smalltalk at: nm]
			ifTrue: [(Smalltalk at: nm substrings first asSymbol) class].
		cls allSuperclasses do: [:aSuper |
			structures at: aSuper name ifAbsent: [newSupers add: aSuper name]]].
			"Don't modify structures during iteration"
	newSupers do: [:nm | structures at: nm put: 3].	"Get all superclasses into list"
	structures keysDo: [:nm | "Nothing added to classes during loop"
		cls := (nm endsWith: ' class') 
			ifFalse: [Smalltalk at: nm]
			ifTrue: [(Smalltalk at: nm substrings first asSymbol) class].
		structures at: nm put: 
			((Array with: cls classVersion), (cls allInstVarNames)).
		superclasses at: nm ifAbsent: [
				superclasses at: nm put: cls superclass name]].
	^ (Array with: 'class structure' with: structures with: 'superclasses' with: superclasses)
]

{ #category : 'read write' }
SmartRefStream >> mapClass: incoming [
	"See if the old class named nm exists.  If so, return it.  If not, map it to a new class, and save the mapping in renamed.  "

	| cls oldVer sel nm |

	self flag: #bobconv.	


	nm := renamed at: incoming ifAbsent: [incoming].	"allow pre-mapping around collisions"
	(nm endsWith: ' class') 
		ifFalse: [cls := Smalltalk at: nm ifAbsent: nil.
			cls ifNotNil: [^ cls]]  	"Known class.  It will know how to translate the instance."
		ifTrue: [cls := Smalltalk at: nm substrings first asSymbol ifAbsent: nil.
			cls ifNotNil: [^ cls class]]. 	"Known class.  It will know how to translate the instance."
	oldVer := self versionSymbol: (structures at: nm).
	sel := nm asPlainString.
	sel at: 1 put: (sel at: 1) asLowercase.
	sel := sel, oldVer.	"i.e. #rectangleoc4"
	Symbol hasInterned: sel ifTrue: [:symb | 
		(self class canUnderstand: sel asSymbol) ifTrue: [
			cls := self perform: sel asSymbol]].	"This class will take responsibility"
	cls ifNil: [cls := self writeClassRenameMethod: sel was: nm
					fromInstVars: (structures at: nm).
			   cls class == String ifTrue: [cls := nil]].
	cls ifNotNil: [renamed at: nm put: cls name].
	^ cls

]

{ #category : 'import image segment' }
SmartRefStream >> mapClass: newClass origName: originalName [
	"See if instances changed shape.  If so, make a fake class for the old shape and return it.  Remember the original class name."

	| newName oldInstVars fakeClass |
	newClass isMeta ifTrue: [^ newClass].
	newName := newClass name.
	(steady includes: newClass) & (newName == originalName) ifTrue: [^ newClass].
		"instances in the segment have the right shape"
	oldInstVars := structures at: originalName ifAbsent: [
			self error: 'class is not in structures list'].	"Missing in object file"
	fakeClass := Object subclass: ('Fake37', originalName) asSymbol
		instanceVariableNames: oldInstVars allButFirst
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Obsolete'.
	ChangeSet changeSetForBaseSystem removeClassChanges: fakeClass name.	"reduce clutter"
	^ fakeClass

]

{ #category : 'read write' }
SmartRefStream >> moreObjects [
	"Return true if there appears to be another object following this one on the file."

	| byte |
	byteStream atEnd ifTrue: [^ false].	"off end of file"
	(byte := byteStream peek) ifNil: [^ false].	"off end of file"
	byte = 33 "$! asciiValue" ifTrue: [^ false].
	byte = 0 ifTrue: [^ false].
	^ true
]

{ #category : 'read write' }
SmartRefStream >> next [
	"Really write three objects: (version, class structure, object). But only when called from the outside.  "

	| version ss object |
	^ topCall
		ifNil: [ 
			topCall := #marked.
			version := super next.
			version class == SmallInteger ifFalse: [^ version].	
				"version number, else just a regular object, not in our format, "
			ss := super next.
			ss class == Array ifFalse: [^ ss].  "just a regular object"
			(ss at: 1) = 'class structure' ifFalse: [^ ss].
			structures := ss at: 2.
			superclasses := (ss size > 3 and: [(ss at: 3) = 'superclasses']) 
				ifTrue: [ss at: 4]		"class name -> superclass name"
				ifFalse: [Dictionary new].
			(self verifyStructure = 'conversion method needed') ifTrue: [^ nil].
			object := super next.	"all the action here"

			topCall := nil.	"reset it"
			object]
		ifNotNil: [
			super next]

]

{ #category : 'read write' }
SmartRefStream >> nextPut: anObject [
	"Really write three objects: (version, class structure, object).  But only when called from the outside.  If any instance-specific classes are present, prepend their source code.  byteStream will be in fileOut format.
	You can see an analysis of which objects are written out by doing: 
	(SmartRefStream statsOfSubObjects: anObject)
	(SmartRefStream tallyOfSubObjects: anObject)
	(SmartRefStream subObjects: anObject ofClass: aClass)"

| info |
topCall
	ifNil: [
		topCall := anObject.
		'Please wait while objects are counted' 
			displayProgressAt: Sensor mousePoint
			from: 0 to: 10
			during: [ :barBlock | info := self instVarInfo: anObject].
		byteStream binary.
		'Writing an object file' displayProgressAt: Sensor mousePoint
			from: 0 to: objCount*4	"estimate"
			during: [ :barBlock |
				objCount := 0.
				progressBar := barBlock.
				self setStream: byteStream reading: false.
					"set basePos, but keep any class renames"
				super nextPut: ReferenceStream versionCode.
				super nextPut: info.
				super nextPut: anObject.		"<- the real writing"
				].
					"Note: the terminator, $!, is not doubled inside object data"
		"references is an IDict of every object that got written"
		byteStream nextPut: $! codePoint; nextPut: 10; nextPut: 10.
		byteStream padToEndIfCantTruncate.
		topCall := progressBar := nil]	"reset it"
	ifNotNil: [
		super nextPut: anObject.
		progressBar ifNotNil: [progressBar value: (objCount := objCount + 1)]].

]

{ #category : 'read write' }
SmartRefStream >> nextPutObjOnly: anObject [
	"Really write three objects: (version, class structure, object).  But only when called from the outside.  Not in fileOut format.  No class definitions will be written for instance-specific classes.  Error if find one.  (Use nextPut: instead)"

	| info |
	topCall
		ifNil: [
			topCall := anObject.
			super nextPut: ReferenceStream versionCode.
			'Please wait while objects are counted' displayProgressAt: Sensor mousePoint
				from: 0 to: 10
				during: [ :barBlock |
					info := self instVarInfo: anObject].
			'Writing an object file' displayProgressAt: Sensor mousePoint
				from: 0 to: objCount*4	"estimate"
				during: [ :barBlock |
					objCount := 0.
					progressBar := barBlock.
					super nextPut: info.
					super nextPut: anObject.	"<- the real writing"
					"Class inst vars not written here!"].
			"references is an IDict of every object that got written
			(in case you want to take statistics)"
			"Transcript cr; show: structures keys printString."		"debug"
			topCall := progressBar := nil]	"reset it"
		ifNotNil: [
			super nextPut: anObject.
			progressBar ifNotNil: [progressBar value: (objCount := objCount + 1)]].
]

{ #category : 'read write' }
SmartRefStream >> noHeader [
	"Signal that we've already dealt with the version and structure array, and are now reading objects."

	topCall := #marked.

]

{ #category : 'read write' }
SmartRefStream >> readInstance [
	"Read the contents of an arbitrary instance.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
Three cases for files from older versions of the system:
1) Class has not changed shape, read it straight.
2) Class has changed instance variables (or needs fixup).  Call a particular method to do it.
3) There is a new class instead.  Find it, call a particular method to read.
	All classes used to construct the structures dictionary *itself* need to be in 'steady' and they must not change!  See setStream:"
	| instSize className refPosn |

	instSize := (byteStream nextUint32BigEndian: true) - 1.
	refPosn := self getCurrentReference.
	className := self next asSymbol.
	^ self readInstanceSize: instSize clsname: className refPosn: refPosn

]

{ #category : 'read write' }
SmartRefStream >> readInstanceSize: instSize clsname: className refPosn: refPosn [
	"The common code to read the contents of an arbitrary instance.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
Three cases for files from older versions of the system:
1) Class has not changed shape, read it straight.
2) Class has changed instance variables (or needs fixup).  Call a particular method to do it.
3) There is a new class instead.  Find it, call a particular method to read.
	All classes used to construct the structures dictionary *itself* need to be in 'steady' and they must not change!  See setStream:"
	| anObject newName newClass dict oldInstVars |

	self flag: #bobconv.	

	self setCurrentReference: refPosn.  "remember pos before readDataFrom:size:"
	newName := renamed at: className ifAbsent: [className].
	newClass := Smalltalk at: newName.
	(steady includes: newClass) & (newName == className) ifTrue: [
	 	anObject := newClass isVariable "Create it here"
			ifFalse: [newClass basicNew]
			ifTrue: [newClass basicNew: instSize - (newClass instSize)].
		anObject := anObject readDataFrom: self size: instSize.
		self setCurrentReference: refPosn.  "before returning to next"
		^ anObject].
	oldInstVars := structures at: className ifAbsent: [
			self error: 'class is not in structures list'].	"Missing in object file"
	anObject := newClass createFrom: self size: instSize version: oldInstVars.
		"only create the instance"
	self beginReference: anObject.
	dict := self catalogValues: oldInstVars size: instSize.
		"indexed vars as (1 -> val) etc."
	dict at: #ClassName put: className.	"so conversion method can know it"

	"Give each superclass a chance to make its changes"
	self storeInstVarsIn: anObject from: dict.	"ones with the same names"

	anObject := self applyConversionMethodsTo: anObject className: className varMap: dict.

	self setCurrentReference: refPosn.  "before returning to next"
	^ anObject
]

{ #category : 'read write' }
SmartRefStream >> readShortInst [
	"Instance has just one byte of size.  Class symbol is encoded in two bytes of file position.  See readInstance."
	| instSize className refPosn |

	instSize := (byteStream next) - 1.	"one byte of size"
	refPosn := self getCurrentReference.
	className := self readShortRef.	"class symbol in two bytes of file pos"
	^ self readInstanceSize: instSize clsname: className refPosn: refPosn

]

{ #category : 'read write' }
SmartRefStream >> readWordLike [
	| refPosn className newName newClass anObject |
	"Can be used by any class that is bits and not bytes (WordArray, Bitmap, SoundBuffer, etc)."

	refPosn := self getCurrentReference.
	className := self next asSymbol.
	newName := renamed at: className ifAbsent: [className].
	newClass := Smalltalk at: newName.
	anObject := newClass newFromStream: byteStream.
	"Size is number of long words."
	self setCurrentReference: refPosn.  "before returning to next"
	^ anObject

]

{ #category : 'read write' }
SmartRefStream >> renamed [

	self flag: #bobconv.	


	^ renamed
]

{ #category : 'read write' }
SmartRefStream >> renamedConv [
	self flag: #bobconv.	


	^ renamedConv
]

{ #category : 'import image segment' }
SmartRefStream >> reshapedClassesIn: outPointers [
	"Look for classes in the outPointer array that have changed shape.  Make a fake class for the old shape.  Return a dictionary mapping Fake classes to Real classes.  Substitute fake classes for real ones in outPointers."

	| mapFakeClassesToReal fakeCls originalName |

	self flag: #bobconv.	


	mapFakeClassesToReal := IdentityDictionary new.
	outPointers withIndexDo: [:outp :ind | 
		outp isBehavior ifTrue: [
			originalName := renamedConv at: ind ifAbsent: [outp name].
				"in DiskProxy>>comeFullyUpOnReload: we saved the name at the index"
			fakeCls := self mapClass: outp origName: originalName.
			fakeCls == outp ifFalse: [
				mapFakeClassesToReal at: fakeCls put: outp.
				outPointers at: ind put: fakeCls]]].
	^ mapFakeClassesToReal
]

{ #category : 'read write' }
SmartRefStream >> setStream: aStream [
	"Initialize me. "

	self flag: #bobconv.	

	super setStream: aStream.
	self initShapeDicts.


]

{ #category : 'read write' }
SmartRefStream >> setStream: aStream reading: isReading [
	"Initialize me. "

	self flag: #bobconv.	

	super setStream: aStream reading: isReading.
	isReading ifFalse: [^ false].
	self initShapeDicts.


]

{ #category : 'class changed shape' }
SmartRefStream >> storeInstVarsIn: anObject from: dict [
	"For instance variables with the same names, store them in the new instance.  Values in variable-length part also.  This is NOT the normal inst var transfer!  See Object.readDataFrom:size:.  This is for when inst var names have changed and some additional conversion is needed.  Here we handle the unchanged vars.  "

	(anObject class allInstVarNames) withIndexDo: [:varName :index |
		(dict includesKey: varName) ifTrue: [
			anObject instVarAt: index put: (dict at: varName)]].
	"variable part"
	(dict includesKey: #SizeOfVariablePart) ifFalse: [^ anObject].
	1 to: (dict at: #SizeOfVariablePart) do: [:index | 
		anObject basicAt: index put: (dict at: index)].
	^ anObject
]

{ #category : 'read write' }
SmartRefStream >> structures [
	^ structures
]

{ #category : 'accessing' }
SmartRefStream >> structures: anObject [
	structures := anObject
]

{ #category : 'read write' }
SmartRefStream >> superclasses [
	^superclasses
]

{ #category : 'accessing' }
SmartRefStream >> superclasses: anObject [
	superclasses := anObject
]

{ #category : 'read write' }
SmartRefStream >> verifyStructure [
	"Compare the incoming inst var name lists with the existing classes.  Prepare tables that will help to restructure those who need it (renamed, reshaped, steady).    If all superclasses are recorded in the file, only compare inst vars of this class, not of superclasses.  They will get their turn.  "


	| newClass newList oldList converting |

	self flag: #bobconv.	

	converting := OrderedCollection new.
	structures keysDo: [:nm "an old className (symbol)" |
		"For missing classes, there needs to be a method in SmartRefStream like 
			#rectangleoc2 that returns the new class."
		newClass := self mapClass: nm.	   "does (renamed at: nm put: newClass name)"
		newClass class == String ifTrue: [^ newClass].  "error, fileIn needed"
		newList := (Array with: newClass classVersion), (newClass allInstVarNames).
		oldList := structures at: nm.
		newList = oldList 
			ifTrue: [steady add: newClass]  "read it in as written"
			ifFalse: [converting add: newClass name]
	].
	false & converting isEmpty not ifTrue: ["debug" 
			self inform: 'These classes are being converted from existing methods:\' withNewLines,
				converting asArray printString].

]

{ #category : 'read write' }
SmartRefStream >> versionSymbol: instVarList [
	"Create the symbolic code (like a version number) for this class in some older version.  First initials of all the inst vars, followed by the class version number.  Returns a string, caller makes it into a compound selector.  "

	| str |
	str := instVarList size = 1 ifFalse: [''] ifTrue: ['x'].		"at least one letter"
	2 to: instVarList size do: [:ind |
		str := str, (instVarList at: ind) first asPlainString].
	str := str, instVarList first printString.	"the number"
	^ str

" | list | list := (Array with: Paragraph classVersion), (Paragraph alistInstVarNames).
(SmartRefStream  on: (DummyStream on: nil)) versionSymbol: list
"
]

{ #category : 'class changed shape' }
SmartRefStream >> writeClassRename: newName was: oldName [
	"Write a method that tells which modern class to map instances to."
	| oldVer sel code |

	oldVer := self versionSymbol: (structures at: oldName).
	sel := oldName asPlainString.
	sel at: 1 put: (sel at: 1) asLowercase.
	sel := sel, oldVer.	"i.e. #rectangleoc4"

	code := WriteStream on: (String new: 500).
	code nextPutAll: sel; newLine.
	code newLine; tab; nextPutAll: '^ ', newName.	"Return new class"

	self class compile: code contents classified: 'conversion'.


]

{ #category : 'class changed shape' }
SmartRefStream >> writeClassRenameMethod: sel was: oldName fromInstVars: oldList  [
	"The class coming is unknown.  Ask the user for the existing class it maps to.  If got one, write a method, and restart the obj fileIn.  If none, write a dummy method and get the user to complete it later.  "

	| tell choice newName answ code oldVer newList newVer instSel |
	self flag: #bobconv.
	tell := 'Reading an instance of ' , oldName 
				, '.
Which modern class should it translate to?'.
	answ := (PopUpMenu 
				labels: 'Let me type the name now
Let me think about it
Let me find a conversion file on the disk') 
					startUpWithCaption: tell.
	answ = 1 
		ifTrue: [
			tell := 'Name of the modern class {1} should translate to:' format: {oldName}.
			choice := self request: tell initialAnswer: ''.	"class name"
			choice isEmpty 
				ifTrue: [answ := 'conversion method needed']
				ifFalse: [
					newName := choice.
					answ := Smalltalk at: newName asSymbol
								ifAbsent: ['conversion method needed'].
					answ class == String 
						ifFalse: [renamed at: oldName asSymbol put: answ name]]].
	answ = 3 | (answ = 0) 
		ifTrue: [
			byteStream close.
			^'conversion method needed'].
	answ = 2 ifTrue: [answ := 'conversion method needed'].
	answ = 'conversion method needed' 
		ifTrue: [
			byteStream close.
			newName := 'PutNewClassHere'].
	answ class == String 
		ifFalse: 
			[oldVer := self versionSymbol: (structures at: oldName).
			newList := (Array with: answ classVersion) , answ allInstVarNames.
			newVer := self versionSymbol: newList.
			instSel := 'convert' , oldVer , ':' , newVer , ':'].
	code := WriteStream on: (String new: 500).
	code
		nextPutAll: sel;
		newLine.
	answ class == String 
		ifFalse: [
			code
				newLine;
				tab;
				nextPutAll: 'reshaped at: #' , oldName , ' put: #' , instSel , '.'.
			code
				newLine;
				tab;
				tab;
				nextPutAll: '"Be sure to define that conversion method in class ' 
							, answ name , '"'].
	code
		newLine;
		tab;
		nextPutAll: '^ ' , newName.	"Return new class"
	self class compile: code contents classified: 'conversion'.
	newName = 'PutNewClassHere' 
		ifTrue: [
			self 
				inform: 'Please complete the following method and 
then read-in the object file again.'.
			Smalltalk browseAllImplementorsOf: sel asSymbol].
	self flag: #violateBasicLayerPrinciples.
	"SmartRefStream should not refer to UI!!!!! (sd)"

	"The class version number only needs to change under one specific circumstance.  That is when the first letters of the instance variables have stayed the same, but their meaning has changed.  A conversion method is needed, but this system does not know it.  
	If this is true for class Foo, define classVersion in Foo class.  
	Beware of previous object fileouts already written after the change in meaning, but before bumping the version number.  They have the old (wrong) version number, say 2.  If this is true, your method must be able to test the data and successfully read files that say version 2 but are really 3."
	^answ
]

{ #category : 'class changed shape' }
SmartRefStream >> writeConversionMethod: sel class: newClass was: oldName fromInstVars: oldList to: newList [
	"The method convertToCurrentVersion:refStream: was not found in newClass.  Write a default conversion method for the author to modify."

	| code newOthers oldOthers copied |

	code := WriteStream on: (String new: 500).
	code nextPutAll: 'convertToCurrentVersion: varDict refStream: smartRefStrm'; newLine; tab.
	newOthers := newList asOrderedCollection "copy".
	oldOthers := oldList asOrderedCollection "copy".
	copied := OrderedCollection new.
	newList do: [:instVar |
		(oldList includes: instVar) ifTrue: [
			instVar isInteger ifFalse: [copied add: instVar].
			newOthers remove: instVar.
			oldOthers remove: instVar]].
	code nextPutAll: '"These variables are automatically stored into the new instance '.
	code nextPutAll: copied asArray printString; nextPut: $. .
	code newLine; tab; nextPutAll: 'This method is for additional changes.'; 
		nextPutAll: ' Use statements like (foo := varDict at: ''foo'')."'; newLine; newLine; tab.
	newOthers isEmpty & oldOthers isEmpty ifTrue: [^ self].
		"Instance variables are the same.  Only the order changed.  No conversion needed."
	newOthers notEmpty ifTrue: [
		code nextPutAll: '"New variables: ', newOthers asArray printString, '  If a non-nil value is needed, please assign it."\' withNewLines].
	oldOthers notEmpty ifTrue: [
		code nextPutAll: '	"These are going away ', oldOthers asArray printString, '.  Possibly store their info in some other variable?"'].

	code newLine; tab.
	code nextPutAll: '^ super convertToCurrentVersion: varDict refStream: smartRefStrm'.
	newClass compile: code contents classified: 'object fileIn'.


	"If you write a conversion method beware that the class may need a version number change.  This only happens when two conversion methods in the same class have the same selector name.  (A) The inst var lists of the new and old versions intials as some older set of new and old inst var lists.  or (B) Twice in a row, the class needs a conversion method, but the inst vars stay the same the whole time.  (For an internal format change.)
	If either is the case, fileouts already written with the old (wrong) version number, say 2.  Your method must be able to read files that say version 2 but are really 3, until you expunge the erroneous version 2 files from the universe."
]

{ #category : 'class changed shape' }
SmartRefStream >> writeConversionMethodIn: newClass fromInstVars: oldList to: newList renamedFrom: oldName [
	"The method convertToCurrentVersion:refStream: was not found in newClass.  Write a default conversion method for the author to modify.  If method exists, append new info into the end."

	| code newOthers oldOthers copied newCode |

	newOthers := newList asOrderedCollection "copy".
	oldOthers := oldList asOrderedCollection "copy".
	copied := OrderedCollection new.
	newList do: [:instVar |
		(oldList includes: instVar) ifTrue: [
			instVar isInteger ifFalse: [copied add: instVar].
			newOthers remove: instVar.
			oldOthers remove: instVar]].
	code := WriteStream on: (String new: 500).
	code newLine; newLine; tab; nextPutAll: '"From ', Smalltalk versionString, ' [', Smalltalk lastUpdateString;
			nextPutAll: '] on ', Date today printString, '"'; newLine.
	code tab; nextPutAll: '"These variables are automatically stored into the new instance: '.
	code nextPutAll: copied asArray printString; nextPut: $.; newLine.
	code tab; nextPutAll: 'Test for this particular conversion.'; 
		nextPutAll: '  Get values using expressions like (varDict at: ''foo'')."'; newLine; newLine.
	newOthers isEmpty & oldOthers isEmpty & oldName isNil ifTrue: [^ self].
		"Instance variables are the same.  Only the order changed.  No conversion needed."
	newOthers notEmpty ifTrue: [
		code tab; nextPutAll: '"New variables: ', newOthers asArray printString, 
			'.  If a non-nil value is needed, please assign it."'; newLine].
	oldOthers notEmpty ifTrue: [
		code tab; nextPutAll: '"These are going away ', oldOthers asArray printString, 
			'.  Possibly store their info in some other variable?"'; newLine].
	oldName ifNotNil: [
		code tab; nextPutAll: '"Test for instances of class ', oldName, '.'; newLine.
		code tab; nextPutAll: 'Instance vars with the same name have been moved here."'; newLine.
		].
	code tab; nextPutAll: '"Move your code above the ^ super...  Delete extra comments."'; newLine. 

	(newClass includesSelector: #convertToCurrentVersion:refStream:) 
		ifTrue: ["append to old methods"
			newCode := (newClass sourceCodeAt: #convertToCurrentVersion:refStream:),
				code contents]
		ifFalse: ["new method"
			newCode := 'convertToCurrentVersion: varDict refStream: smartRefStrm',
				code contents, 
				'	^ super convertToCurrentVersion: varDict refStream: smartRefStrm'].
	newClass compile: newCode classified: 'object fileIn'.


	"If you write a conversion method beware that the class may need a version number change.  This only happens when two conversion methods in the same class have the same selector name.  (A) The inst var lists of the new and old versions intials as some older set of new and old inst var lists.  or (B) Twice in a row, the class needs a conversion method, but the inst vars stay the same the whole time.  (For an internal format change.)
	If either is the case, fileouts already written with the old (wrong) version number, say 2.  Your method must be able to read files that say version 2 but are really 3, until you expunge the erroneous version 2 files from the universe."
]
