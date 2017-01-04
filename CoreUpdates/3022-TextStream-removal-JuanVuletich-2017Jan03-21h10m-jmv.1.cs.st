'From Cuis 5.0 of 7 November 2016 [latest update: #3021] on 3 January 2017 at 9:34:24 pm'!

!WriteStream methodsFor: 'accessing' stamp: 'jmv 1/3/2017 21:22:43'!
nextPutAllString: aString withAttributes: attributesArray

	| newEnd |
	(self isCompatibleWithContents: aString)
		ifFalse: [ ^ super nextPutAllString: aString withAttributes: attributesArray ].

	newEnd _ position + aString size.
	newEnd > writeLimit ifTrue: [
		self growTo: newEnd + 10].

	collection
		replaceFrom: position+1
		to: newEnd 
		withString: aString
		attributes: attributesArray
		startingAt: 1.
	position _ newEnd! !

!WriteStream methodsFor: 'private' stamp: 'jmv 1/3/2017 21:21:34'!
isCompatibleWithContents: aCollection

	collection class == aCollection class
		ifTrue: [ ^ true ].

	(aCollection isString and: [ collection is: #Text])
		ifTrue: [ ^ true ].

	^ false! !


!PositionableStream methodsFor: 'testing' stamp: 'jmv 1/3/2017 10:57:48'!
isText
	"Return true if the receiver is a Text stream"
	^collection is: #Text! !


!WriteStream methodsFor: 'accessing' stamp: 'jmv 1/3/2017 21:22:51'!
nextPutAll: aCollection

	| newEnd |
	(self isCompatibleWithContents: aCollection)
		ifFalse: [ ^ super nextPutAll: aCollection ].

	newEnd _ position + aCollection size.
	newEnd > writeLimit ifTrue: [
		self growTo: newEnd + 10].

	collection replaceFrom: position+1 to: newEnd  with: aCollection startingAt: 1.
	position _ newEnd.! !

!WriteStream methodsFor: 'private' stamp: 'jmv 1/3/2017 10:57:15'!
withAttribute: aTextAttribute do: streamBlock
	| pos1 val |

	(collection is: #Text) ifFalse: [
		^streamBlock value ].

	pos1 _ self position.
	val _ streamBlock value.
	collection addAttribute: aTextAttribute from: pos1+1 to: self position.
	^ val! !

!WriteStream methodsFor: 'private' stamp: 'jmv 1/3/2017 10:57:19'!
withAttributes: attributes do: streamBlock 
	| pos1 val |

	(collection is: #Text) ifFalse: [
		^streamBlock value ].

	pos1 _ self position.
	val _ streamBlock value.
	attributes do: [:attribute |
		collection
			addAttribute: attribute
			from: pos1 + 1
			to: self position].
	^ val! !

!methodRemoval: Text class #streamContents:!
Text class removeSelector: #streamContents:!
!classRemoval: #TextStream!
Smalltalk removeClassNamed: #TextStream!
