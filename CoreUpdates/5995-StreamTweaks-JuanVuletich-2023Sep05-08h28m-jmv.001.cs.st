'From Cuis 6.0 [latest update: #5995] on 5 September 2023 at 8:53:20 am'!

!PositionableStream methodsFor: 'mode' stamp: 'jmv 9/1/2023 17:02:03'!
useBytes
	self assert: collection class isBytes ! !


!Stream methodsFor: 'nonhomogeneous accessing' stamp: 'jmv 9/5/2023 08:29:24'!
nextString
	"Read a string from the receiver. The first byte is the length of the string, unless it is greater than 192, in which case the first four bytes encode the length."

	| bytes length |

	self binary.
	length := self next.		"first byte."
	length >= 192 ifTrue: [length := length - 192.
		1 to: 3 do: [:ii | length := length * 256 + self next]].
	bytes := ByteArray new: length.
	self nextInto: bytes.
	^bytes asString! !


!DataStream methodsFor: 'write and read' stamp: 'jmv 9/1/2023 17:03:35'!
readString

	^byteStream nextString! !


!SmartRefStream methodsFor: 'read write' stamp: 'jmv 9/5/2023 08:49:46'!
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
		byteStream useCharacters.
		byteStream nextPutAll: '!!'; newLine; newLine.
		byteStream padToEndIfCantTruncate.
		topCall := progressBar := nil]	"reset it"
	ifNotNil: [
		super nextPut: anObject.
		progressBar ifNotNil: [progressBar value: (objCount := objCount + 1)]].
! !

!methodRemoval: Stream #ascii stamp: 'jmv 9/5/2023 08:49:58'!
Stream removeSelector: #ascii!
