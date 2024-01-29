'From Cuis6.3 [latest update: #6169] on 8 January 2024 at 4:38:38 pm'!

!ReadWriteStream methodsFor: 'accessing' stamp: 'jmv 1/8/2024 16:36:44'!
next: n into: aCollection startingAt: startIndex
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than
	n elements have been read."
	| max |
	max := (readLimit - position) min: n.
	aCollection 
		replaceFrom: startIndex 
		to: startIndex+max-1
		with: collection
		startingAt: position+1.
	position := position + max.
	max = n
		ifTrue:[^aCollection]
		ifFalse:[^aCollection copyFrom: 1 to: startIndex+max-1]! !


!DataStream class methodsFor: 'as yet unclassified' stamp: 'jmv 1/8/2024 16:38:06'!
streamedRepresentationOf: anObject

	| file |
	file := (ReadWriteStream on: (ByteArray new: 5000)).
	(self on: file) nextPut: anObject.
	^file contents! !

!DataStream class methodsFor: 'as yet unclassified' stamp: 'jmv 1/8/2024 16:38:09'!
unStream: aByteArray

	^(self on: ((ReadWriteStream with: aByteArray) reset)) next! !


!SmartRefStream methodsFor: 'read write' stamp: 'jmv 1/8/2024 15:29:31'!
nextPut: anObject
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
					"Note: the terminator, $!!, is not doubled inside object data"
		"references is an IDict of every object that got written"
		byteStream nextPut: $!! codePoint; nextPut: 10; nextPut: 10.
		byteStream padToEndIfCantTruncate.
		topCall := progressBar := nil]	"reset it"
	ifNotNil: [
		super nextPut: anObject.
		progressBar ifNotNil: [progressBar value: (objCount := objCount + 1)]].
! !

!classRemoval: #RWBinaryOrTextStream stamp: 'jmv 1/8/2024 16:38:22'!
Smalltalk removeClassNamed: #RWBinaryOrTextStream!

