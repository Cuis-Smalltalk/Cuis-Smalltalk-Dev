'From Cuis6.1 [latest update: #6107] on 30 November 2023 at 1:17:58 pm'!

!Random commentStamp: 'jmv 11/30/2023 13:07:01' prior: 0!
The abstract class for all random number generators.

If you just want a quick random integer, use:
		10 atRandom
Every integer interval can give a random number:
		(6 to: 12) atRandom
SequenceableCollections can give randomly selected elements:
		'pick one of these letters randomly' atRandom
SequenceableCollections also respond to shuffled, as in:
		($A to: $Z) shuffled

The correct way to use class Random is to store one in an instance or class variable:
		myGenerator := Random new.
Then use it every time you need another Float in the interval [0, 1)
		myGenerator next
You can also generate a positive integer
		myGenerator nextInt: 10!


!Trie commentStamp: 'jmv 11/30/2023 13:07:43' prior: 0!
I am an efficient collection of Strings. I behave a bit like a Dictionary, with the restriction that keys are instances of String.
Notes:
	- check for inclusion is extremely fast
	- iteration is always done in collation order, contents are always sorted without performance cost
	- Behaves both like a Set (#add:, #remove:, #includes, #do) and a Dictionary (#at:put:, #at:, #at:ifAbsent:)
	
| t |
t := Trie new.
t add: 'car'.
t at: 'car' put: Float pi.
t at: 'cat' put: Date today.
t explore.
(t includesKey: 'car') print.
(t includes: 'cat') print

| t |
t := Trie new.
Smalltalk allImplementedMessages do: [ :s | t add: s ].
t explore!


!DataStream commentStamp: 'jmv 11/30/2023 13:17:36' prior: 0!
This is the save-to-disk facility. A DataStream can store one or more objects in a persistent form.

To handle objects with sharing and cycles, you must use a
ReferenceStream instead of a DataStream.  (Or SmartRefStream.)  ReferenceStream is typically
faster and produces smaller files because it doesn't repeatedly write the same Symbols.

Here is the way to use DataStream and ReferenceStream:
	rr := ReferenceStream fileNamed: 'test.obj'.
	rr nextPut: <your object>.
	rr close.

To get it back:
	rr := ReferenceStream fileNamed: 'test.obj'.
	<your object> := rr next.
	rr close.

Each object to be stored has two opportunities to control what gets stored.  On the high level, objectToStoreOnDataStream allows you to substitute another object on the way out.  The low level hook is storeDataOn:. The read-in counterparts to these messages are comeFullyUpOnReload and (class) readDataFrom:size:. See these methods, and the class DiskProxy, for more information about externalizing and internalizing.

NOTE: A DataStream should be treated as a write-stream for writing.  It is a read-stream for reading.  It is not a ReadWriteStream.
!


!ReferenceStream commentStamp: 'jmv 11/30/2023 13:08:17' prior: 0!
This is a way of serializing a tree of objects into disk file. A ReferenceStream can store
one or more objects in a persistent form, including sharing and cycles.

Here is the way to use ReferenceStream:
     ReferenceStream dumpOnFile: ('test1.obj' asFileEntry) object: myObj . 

To get it back:
     myObj  := ReferenceStream restoreFromFile: ('test1.obj' asFileEntry ). 

ReferenceStreams can now write "weak" references. nextPutWeak:
writes a "weak" reference to an object, which refers to that object
*if* it also gets written to the stream by a normal nextPut:.

A ReferenceStream should be treated as a read-stream *or* as a write-stream, *not* as a read/write-stream. The reference-remembering mechanism would probably do bad things if you tried to read and write from the same ReferenceStream.

Instance variables
 references -- an IdentityDictionary mapping objects already written
	to their byteStream positions. If asked to write any object a
	second time, we just write a reference to its stream position.
	This handles shared objects and reference cycles between objects.
	To implement "weak references" (for Aliases), the references
	dictionary also maps objects not (yet?) written to a Collection
	of byteStream positions with hopeful weak-references to it. If
	asked to definitely write one of these objects, we'll fixup those
	weak references.
 objects -- an IdentityDictionary mapping relative byte stream positions to
	objects already read in. If asked to follow a reference, we
	return the object already read.
	This handles shared objects and reference cycles between objects.
 currentReference -- the current reference position. Positon relative to the 
	start of object data in this file.  (Allows user to cut and paste smalltalk 
	code from the front of the file without effecting the reference values.)  
	This variable is used to help install each new object in "objects" as soon
	as it's created, **before** we start reading its contents, in
	case any of its content objects reference it.
 fwdRefEnds -- A weak reference can be a forward reference, which
	requires advance-reading the referrent. When we later come to the
	object, we must get its value from "objects" and not re-read it so
	refs to it don't become refs to copies. fwdRefEnds remembers the
	ending byte stream position of advance-read objects.
 skipping -- true if <what?>

If the object is referenced before it is done being created, it might get created twice.  Just store the object the moment it is created in the 'objects' dictionary.  If at the end, comeFullyUpOnReload returns a different object, some refs will have the temporary object (this is an unlikely case).  At the moment, no implementor of comeFullyUpOnReload returns a different object except DiskProxy, and that is OK.
!


!SmartRefStream commentStamp: 'jmv 11/30/2023 13:08:34' prior: 0!
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

(varDict includesKey: 'extent') ifFalse: ["old version!!"
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
!


!AbstractVectorCanvas commentStamp: 'jmv 11/30/2023 13:13:58' prior: 0!
A VectorGraphics canvas specially built for Morphic.

self runningWorld backgroundImage displayAt: 0@0. (VectorCanvas onForm: Display) fullDraw: self runningWorld. Display copy inspect.
Display writePNGfileNamed: 'm3z.png'

On any Morph:
| c |
c := VectorCanvas onForm: Display.
self allOwnersReverseDo: [ :m | c into: m ].
c fullDraw: self.
Display forceToScreen!


!VectorEngineWithPlugin commentStamp: 'jmv 11/30/2023 13:16:57' prior: 0!
"Just an experiment to exercise drawing operations."

| m |
m := Sample01Star new.
m morphPosition: 150@120.
(VectorCanvas onForm: Display copy) fullDraw: m; inspect.
Display forceToScreen.

"Not sure if fallback code should use a pluginSmalltalkCode ivar, calling the VectorEnginePlugin in smalltalk, or duplicate that code here for fallback code (this would be better if those start to diverge because of optimizations)"!


!EventSensor methodsFor: 'private-I/O' stamp: 'jmv 11/30/2023 13:07:23'!
fetchMoreEventsDiscardingMouseEvents: discardMouseEvents
	"Fetch more events from the VM"
	| eventBuffer type |

	"Reset input semaphore so clients can wait for the next events after this one."
	inputSemaphore isSignaled
		ifTrue: [
			hasInputSemaphore := true.
			inputSemaphore initSignals ].

	"DebuggingAid""
	dd := Time localMillisecondClock - lastEventPoll.
	"

	eventBuffer := Array new: 8.
	[
		self primGetNextEvent: eventBuffer.
		"DebuggingAid""
		eventBuffer first = 2 ifTrue: [
			k1 := eventBuffer third.
			u1 := eventBuffer sixth.
			pressType := eventBuffer fourth.
			typeS := 'unknown'.
			pressType = EventSensor eventKeyDown ifTrue: [
				typeS := #keyDown].
			pressType = EventSensor eventKeyUp ifTrue: [
				typeS := #keyUp].
			pressType = EventSensor eventKeyChar ifTrue: [
				typeS := #keystroke].
			dd >100 ifTrue: ['-' print. dd := 0 ].
			{ k1 < 256 ifTrue: [Character numericValue: k1] ifFalse: [ '[]']. UnicodeCodePoint codePoint: k1. k1. k1 hex. '-----'. 
				UnicodeCodePoint codePoint: u1. u1. u1 hex. '----'. pressType. typeS } print.
			].
		"
		type := eventBuffer at: 1.
		type = EventSensor eventTypeNone ]
			whileFalse: [
				self
					processSensorEvent: eventBuffer
					discardingMouseEvents: discardMouseEvents ].

	"Remember the last time that I checked for events."
	lastEventPoll := Time localMillisecondClock.! !


!SmartRefStream methodsFor: 'class changed shape' stamp: 'jmv 11/30/2023 13:09:06'!
writeConversionMethod: sel class: newClass was: oldName fromInstVars: oldList to: newList
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
	If either is the case, fileouts already written with the old (wrong) version number, say 2.  Your method must be able to read files that say version 2 but are really 3, until you expunge the erroneous version 2 files from the universe."! !


!Scanner methodsFor: 'public access' stamp: 'jmv 11/30/2023 13:13:29'!
typedScanTokens: textOrString 
	"Answer an Array that has been tokenized with literals mapped to literals,
	 special characters mapped to symbols and variable names and keywords
	 to strings."
	| s |
	self initScannerForTokenization.
	self scan: textOrString asPlainString readStream.
	s := (Array new: 16) writeStream.
	[tokenType == #doIt] whileFalse: [
		(token == #- 
			and: [(self typeTableAt: hereChar) == #xDigit]) ifTrue: [
				self scanToken.
				token := token negated].
		s nextPut: token.
		self scanToken].
	^s contents

	"Scanner new typedScanTokens: (Scanner sourceCodeAt: #typedScanTokens:)"! !

