"
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

"
Class {
	#name : 'ReferenceStream',
	#superclass : 'DataStream',
	#instVars : [
		'references',
		'objects',
		'currentReference',
		'fwdRefEnds',
		'blockers',
		'skipping'
	],
	#category : 'System-Object Storage',
	#package : 'System',
	#tag : 'Object Storage'
}

{ #category : 'as yet unclassified' }
ReferenceStream class >> dumpOnFile: aFileEntry object: anObject [
	"Warning: If the file given by aFileEntry exists, it will be overwritten."
	
	aFileEntry forceWriteStreamDo: [ :stream |
		(self on: stream) nextPut: anObject ].
]

{ #category : 'as yet unclassified' }
ReferenceStream class >> example2 [
"Here is the way to use DataStream and ReferenceStream:
	rr := ReferenceStream fileNamed: ''test.obj''.
	rr nextPut: <your object>.
	rr close.

To get it back:
	rr := ReferenceStream fileNamed: ''test.obj''.
	<your object> := rr next.
	rr close.
"
"An example and test of DataStream/ReferenceStream.
	 11/19/92 jhm: Use self testWith:."
	"ReferenceStream example2"
	| input sharedPoint |

	"Construct the test data."
	input := Array new: 9.
	input at: 1 put: nil.
	input at: 2 put: true.
	input at: 3 put: false.
	input at: 4 put: #(-4 -4.0 'four' four).
	input at: 5 put: (Form extent: 63 @ 50 depth: 8).
		(input at: 5) fillWithColor: `Color lightOrange`.
	input at: 6 put: 1024 @ -2048.
	input at: 7 put: input. "a cycle"
	input at: 8 put: (Array with: (sharedPoint := 0 @ -30000)).
	input at: 9 put: sharedPoint.

	"Write it out, read it back, and return it for inspection."
	^ self testWith: input

]

{ #category : 'as yet unclassified' }
ReferenceStream class >> restoreFromFile: aFileEntry [
"
 Note about importing String objects from Squeak: 
	
To make sure Strings can be moved from Squeak use 
           aString utf8Encoded 
to convert the strings to ByteArrays in Squeak before you export the object collection.

In Cuis you need to do then
           UnicodeString fromUtf8Bytes: aByteArray
to get them back.
"
	^aFileEntry readStreamDo: [ :stream |
		(self on: stream) next ].
]

{ #category : 'as yet unclassified' }
ReferenceStream class >> versionCode [
    "Answer a number representing the 'version' of the ReferenceStream facility; this is stashed at the beginning of ReferenceStreams, as a secondary versioning mechanism (the primary one is the fileTypeCode).   At present, it serves for information only, and is not checked for compatibility at reload time, but could in future be used to branch to variant code. "

	" 1 = original version 1992"
	" 2 = HyperSqueak.  PathFromHome used for Objs outside the tree.  SqueakSupport SysLibrary for shared
		globals like Display and StrikeFonts.  File has version number, class structure, then an IncomingObjects
		manager.  8/16/96 tk.  
			Extended to SmartRefStream.  class structure also keeps superclasse chain.  Does analysis on
		structure to see when translation methods are needed.  Embedable in file-ins.  (factored out
		HyperSqueak support)  Feb-May 97 tk"
	" 3 = Reference objects are byte offsets relative to the start of the object portion of the file.  Rectangles with
		values -2048 to 2047 are encoded compactly."
	" 4 = If UniClasses (subclasses of Player) have class instance variables, append their values in the form
		(#Class43 (val1 val2 vla3)).  An array of those.  Can still read version 3."
	"Cuis Smalltalk. Can not really manage UniClasses, as there are no Etoys system in place. No class Player
		either. Anyway, keep answering 4 (instead of 3) as it is quite possible that some other subtletles have
		changed since version 3."
	" 5 = Store SmallIntegers as two 32 bit words. Not doing this was a serious bug in 64 bit systems."
	^ 5
]

{ #category : 'writing' }
ReferenceStream >> addSpecialReference: aDiskProxy [
	"See senders. Added to avoid breaking encapsulation (assuming that #references would answer the actual collection)"
	references at: aDiskProxy put: #none
]

{ #category : 'writing' }
ReferenceStream >> beginInstance: aClass size: anInteger [
	"This is for use by storeDataOn: methods.  Cf. Object>>storeDataOn:."
	"Addition of 1 seems to make extra work, since readInstance has to compensate.  Here for historical reasons dating back to Kent Beck's original implementation in late 1988.
	In ReferenceStream, class is just 5 bytes for shared symbol.
	SmartRefStream puts out the names and number of class's instances variables for checking.
6/10/97 16:09 tk: See if we can put on a short header. Type = 16. "

	| short ref |
	short := true.	"All tests for object header that can be written in 4 bytes"
	anInteger <= 254 ifFalse: [short := false].	"one byte size"
	ref := references at: aClass name ifAbsent: [short := false. nil].
	ref isInteger ifFalse: [short := false].
	short ifTrue: [short := (ref < 65536) & (ref > 0) "& (ref ~= self vacantRef)"].  "vacantRef is big"
	short ifTrue: [
		byteStream skipBack.
		short := byteStream next = 9.
		byteStream skip: 0].	"ugly workaround"
	short 
		ifTrue: ["passed all the tests!"
			byteStream skipBack; nextPut: 16; "type = short header"
				nextPut: anInteger + 1;	"size is short"
				nextUint16Put: ref bigEndian: true ]
		ifFalse: [
			"default to normal longer object header"
			byteStream nextUint32Put: anInteger + 1 bigEndian: true.
			self nextPut: aClass name].
]

{ #category : 'reading' }
ReferenceStream >> beginReference: anObject [
	"Remember anObject as the object we read at the position recorded by
	 noteCurrentReference:. This must be done after instantiating anObject but
	 before reading any of its contents that might (directly or indirectly) refer to
	 it. (It's ok to do this redundantly, which is convenient for #next.)
	 Answer the reference position."

	objects at: currentReference ifAbsent: [
		objects at: currentReference put: anObject.
		^ currentReference].
	(skipping includes: currentReference) ifFalse: [
		"If reading just to skip it, don't record this copy."
		objects at: currentReference put: anObject
	].
	^ currentReference		"position relative to start of data portion of file"
]

{ #category : 'writing' }
ReferenceStream >> blockers [

	^ blockers
]

{ #category : 'writing' }
ReferenceStream >> blockers: anIdentDict [
	"maps objects -> nil if they should not be written.  object -> anotherObject if they need substitution."

	anIdentDict class == IdentityDictionary ifFalse: [self error: 'must be IdentityDictionary'].
	blockers := anIdentDict
]

{ #category : 'reading' }
ReferenceStream >> getCurrentReference [
    "PRIVATE -- Return the currentReference posn.  Always a relative position.  So user can cut and paste the Smalltalk source code at the beginning of the file."

    ^ currentReference	"relative position"
]

{ #category : 'aux' }
ReferenceStream >> isAReferenceType: typeID [
	"Answer is:
		- true means a regular object reference (and the referenced object is in elsewhere in the stream)
		- false means either
				- an immediate (SmallInteger, Character, but not SmallFloat64) 
				- or a special object (nil, true, false) 
				- or a Class (actually an error condition. See #next and #nextPut:)
	"
	^ (#(0 0 0 0 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 1 1 0 1 1 0) at: typeID) = 1
]

{ #category : 'reading' }
ReferenceStream >> maybeBeginReference: internalObject [
	"See if need to record a reference.  In case in the file twice"

	(self isAReferenceType: (self typeIDFor: internalObject))
			ifTrue: [self beginReference: internalObject].
			"save the final object and give it out next time."
	^ internalObject
]

{ #category : 'reading' }
ReferenceStream >> next [
	"Answer the next object in the stream.  If this object was already read, don't re-read it.  File is positioned just before the object."
	| curPosn skipToPosn haveIt theObject wasSkipping |

	haveIt := true.
	curPosn := byteStream position - basePos.
	theObject := objects at: curPosn ifAbsent: [haveIt := false].
		"probe in objects is done twice when coming from objectAt:.  This is OK."
	skipToPosn := fwdRefEnds at: curPosn ifAbsent: nil.
	haveIt ifFalse: [ ^ super next].

	skipToPosn ifNotNil: [
		"Skip over the object and return the already-read-in value."
		byteStream position: skipToPosn + basePos		"make absolute"
	] ifNil: [
		"File is not positioned correctly.  Read object and throw it away."
		wasSkipping := skipping includes: curPosn.
		skipping add: curPosn.
		"fake _" super next.
		wasSkipping ifFalse: [skipping remove: curPosn ifAbsent: nil].
	].
	^ theObject
		
]

{ #category : 'writing' }
ReferenceStream >> nextPutWeak: anObject [
    "Write a weak reference to anObject to the receiver stream. Answer anObject.
     If anObject is not a reference type of object, then just put it normally.
     A 'weak' reference means: If anObject gets written this stream via nextPut:,
     then its weak references will become normal references. Otherwise they'll
     read back as nil. -- "
    | typeID referencePosn |

    "Is it a reference type of object? If not, just write it normally."
    typeID := self typeIDFor: anObject.
    (self isAReferenceType: typeID) ifFalse: [^ self nextPut: anObject].

    "Have we heard of and maybe even written anObject before?"
    referencePosn := references at: anObject ifAbsent: [
			references at: anObject put: OrderedCollection new].

    "If referencePosn is an Integer, it's the stream position of anObject.
     Else it's a collection of hopeful weak-references to anObject."
    referencePosn isInteger ifFalse:
        [referencePosn add: byteStream position - basePos.		"relative"
        referencePosn := self vacantRef].
    self outputReference: referencePosn.		"relative"

    ^ anObject
]

{ #category : 'reading' }
ReferenceStream >> noteCurrentReference: typeID [
	"PRIVATE -- If we support references for type typeID, remember
	 the current byteStream position so beginReference: can add the
	 next object to the 'objects' dictionary of reference positions,
	 then return true. Else return false."
	| answer |

	(answer := self isAReferenceType: typeID)
		ifTrue: [self setCurrentReference: (byteStream position - 1) - basePos "relative"
				"subtract 1 because we already read the object's type ID byte"].
	^ answer
]

{ #category : 'reading' }
ReferenceStream >> objectAt: anInteger [
    "PRIVATE -- Read & return the object at a given stream position.
     If we already read it, just get it from the objects dictionary.
     (Reading it again wouldn't work with cycles or sharing.)
     If not, go read it and put it in the objects dictionary.
     NOTE: This resolves a cross-reference in the ReferenceStream:
       1. A backward reference to an object already read (the normal case).
       2. A forward reference which is a sated weak reference (we record where
          the object ends so when we get to it normally we can fetch it from
          'objects' and skip over it).
       3. A backward reference to a 'non-reference type' per the long NOTE in
          nextPut: (we compensate here--seek back to re-read it and add the object
          to 'objects' to avoid seeking back to read it any more times).
       4. While reading a foward weak reference (case 2), we may recursively hit an
          ordinary backward reference to an object that we haven't yet read because
          we temporarily skipped ahead. Such a reference is forward in time so we
          treat it much like case 2.
     11/16-24/92 jhm: Handle forward refs. Cf. class comment and above NOTE.
	08:57 tk   anInteger is a relative position"
    | savedPosn refPosn anObject |

    ^ objects at: anInteger "relative position.  case 1: It's in 'objects'"
        ifAbsent:   "do like super objectAt:, but remember the fwd-ref-end position"
            [savedPosn := byteStream position.		"absolute"
            refPosn := self getCurrentReference.	"relative position"

            byteStream position: anInteger + basePos.	"was relative"
            anObject := self next.

            (self isAReferenceType: (self typeIDFor: anObject))
                ifTrue:  [fwdRefEnds at: anInteger put: byteStream position - basePos] "cases 2, 4"
                ifFalse: [objects at: anInteger put: anObject]. "case 3"

            self setCurrentReference: refPosn.		"relative position"
            byteStream position: savedPosn.		"absolute"
            anObject]
]

{ #category : 'writing' }
ReferenceStream >> objectIfBlocked: anObject [
	"See if this object is blocked -- not written out and another object substituted."

	^ blockers at: anObject ifAbsent: [anObject]
]

{ #category : 'writing' }
ReferenceStream >> references [
	"Values can be:
		- integer: regular reference. Value is position in the output stream.
		- #none: special value for DiskProxy. Value is just a marker.
		- A collection of (still) weak only references. They are turned into regular references when the first regular reference to the key is dumped.
	See #tryToPutReference:typeID:"
	
	"Warning: Methods that need to deal with the internals of dumping weak references, or methods that need to actually modify the references dictionary CAN NOT use this method!"

	"Do not include provisory references created in #nextPutWeak that never became normal references,
	because the referenced object was never added from a call to #nextPut:"
	^ references select: [ :value | value isNumber or: [ value == #none ]]
]

{ #category : 'writing' }
ReferenceStream >> replace: original with: proxy [
	"We may wish to remember that in some field, the original object is being replaced by the proxy.  For the hybred scheme that collects with a DummyStream and writes an ImageSegment, it needs to hold onto the originals so they will appear in outPointers, and be replaced."

	blockers at: original put: proxy
]

{ #category : 'writing' }
ReferenceStream >> reset [
	"PRIVATE -- Reset my internal state.
	   11/15-17/92 jhm: Added transients and fwdRefEnds.
	   7/11/93 sw: Give substantial initial sizes to avoid huge time spent growing.
	   9/3/93 sw: monster version for Sasha"

	super reset.
	references := IdentityDictionary newWithRoomForMoreThan: 4096 * 5.
"	objects := IdentityDictionary newWithRoomForMoreThan: 4096 * 5.
	fwdRefEnds := IdentityDictionary new.
"
	blockers ifNil: [blockers := IdentityDictionary new].
 
]

{ #category : 'reading' }
ReferenceStream >> setCurrentReference: refPosn [
    "PRIVATE -- Set currentReference to refPosn.  Always a relative position."

    currentReference := refPosn		"relative position"
]

{ #category : 'initialization' }
ReferenceStream >> setStream: aStream [
	"PRIVATE -- Initialization method."

	super setStream: aStream.
	references := IdentityDictionary newWithRoomForMoreThan: 4096 * 5.
	objects := IdentityDictionary newWithRoomForMoreThan: 4096 * 5.
	fwdRefEnds := IdentityDictionary new.
	skipping := IdentitySet new.
	blockers ifNil: [blockers := IdentityDictionary new].	"keep blockers we just passed in"

]

{ #category : 'writing' }
ReferenceStream >> setStream: aStream reading: isReading [
	"PRIVATE -- Initialization method."

	super setStream: aStream reading: isReading.
	"isReading ifFalse: [  when we are sure"
	references := IdentityDictionary newWithRoomForMoreThan: 4096 * 5.
	isReading ifTrue: [
		objects := IdentityDictionary newWithRoomForMoreThan: 4096 * 5.
		skipping := IdentitySet new.
		fwdRefEnds := IdentityDictionary new].
	blockers ifNil: [blockers := IdentityDictionary new].	"keep blockers we just passed in"

]

{ #category : 'statistics' }
ReferenceStream >> statisticsOfRefs [
	"Analyze the information in references, the objects being written out"

	| parents n kids nm ownerBags tallies owners objParent normalReferences |
	normalReferences := self references.	"Exclude unrealized weaks"
	parents := IdentityDictionary newWithRoomForMoreThan: normalReferences size * 2.
	n := 0.
	'Finding Owners...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: normalReferences size
	during: [ :barBlock |
	normalReferences keysDo:
		[ :parent | barBlock value: (n := n+1).
		kids := parent class isFixed
			ifTrue: [(1 to: parent class instSize) collect: [:i | parent instVarAt: i]]
			ifFalse: [parent class isBits ifTrue: [#()]
					 ifFalse: [(1 to: parent basicSize) collect: [:i | parent basicAt: i]]].
		(kids select: [:x | normalReferences includesKey: x])
			do: [:child | parents at: child put: parent]]].
	ownerBags := Dictionary new.
	tallies := Bag new.
	n := 0.
	'Tallying Owners...'
	displayProgressAt: Sensor mousePoint
	from: 0 to: normalReferences size
	during: [ :barBlock |
	normalReferences keysDo:  "For each class of obj, tally a bag of owner classes"
		[ :obj | barBlock value: (n := n+1).
		nm := obj class name.
		tallies add: nm.
		owners := ownerBags at: nm ifAbsent: [ownerBags at: nm put: Bag new].
		(objParent := parents at: obj ifAbsent: nil) ifNotNil: [
			owners add: objParent class name]]].
	^ String streamContents: [ :strm | 
		tallies sortedCounts do: [ :assn |
			n := assn key.  nm := assn value.
			owners := ownerBags at: nm.
			strm newLine; nextPutAll: nm; space; print: n.
			owners size > 0 ifTrue: [
				strm newLine; tab; print: owners sortedCounts]]]
]

{ #category : 'writing' }
ReferenceStream >> tryToPutReference: anObject typeID: typeID [
	"PRIVATE -- If we support references for type typeID, and if
	   anObject already appears in my output stream, then put a
	   reference to the place where anObject already appears. If we
	   support references for typeID but didn't already put anObject,
	   then associate the current stream position with anObject in
	   case one wants to nextPut: it again.
	 Return true after putting a reference; false if the object still
	   needs to be put.
	 : Added support for weak refs. Split out outputReference:.
	08:42 tk  references stores relative file positions."
	| referencePosn nextPosn |

	"Is it a reference type of object?"
	(self isAReferenceType: typeID) ifFalse: [^ false].

	"Have we heard of and maybe even written anObject before?"
	referencePosn := references at: anObject ifAbsent:
			["Nope. Remember it and let the sender write it."
			references at: anObject put: (byteStream position - basePos).	"relative"
			^ false].

	"If referencePosn is an Integer, it's the stream position of anObject."
	referencePosn isInteger ifTrue:
		[self outputReference: referencePosn.	"relative"
		^ true].

	referencePosn == #none ifTrue: ["for DiskProxy"
			references at: anObject put: (byteStream position - basePos).	"relative"
			^ false].


	"Else referencePosn is a collection of positions of weak-references to anObject.
	 Make them full references since we're about to really write anObject."
	references at: anObject put: (nextPosn := byteStream position) - basePos.	"store relative"
	referencePosn do: [:weakRefPosn |
			byteStream position: weakRefPosn + basePos.		"make absolute"
			self outputReference: nextPosn - basePos].	"make relative"
	byteStream position: nextPosn.		"absolute"
	^ false
]
