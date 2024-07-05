'From Cuis7.1 [latest update: #6496] on 5 July 2024 at 3:32:23 pm'!

!ArrayedCollection class methodsFor: 'aux' stamp: 'jmv 7/5/2024 15:24:22'!
bytesPerBasicElement
	"Answer the number of bytes that each of my basic elements requires.
	In other words:
		self basicSize * self class bytesPerBasicElement
	should equal the space required on disk by my variable sized representation."
	self isBytes ifTrue: [ ^1 ].
	self isDoubleBytes ifTrue: [ ^2 ].
	self isWords ifTrue: [ ^4 ].
	self isDoubleWords ifTrue: [ ^8 ].
	^Smalltalk wordSize.
! !


!AffineTransformation class methodsFor: 'aux' stamp: 'jmv 7/5/2024 15:24:45'!
bytesPerBasicElement
	"Answer the number of bytes that each of my basic elements requires.
	In other words:
		self basicSize * self class bytesPerBasicElement
	should equal the space required on disk by my variable sized representation."
	self isBytes ifTrue: [ ^1 ].
	self isDoubleBytes ifTrue: [ ^2 ].
	self isWords ifTrue: [ ^4 ].
	self isDoubleWords ifTrue: [ ^8 ].
	^Smalltalk wordSize.
! !


!ArrayedCollection methodsFor: 'objects from disk' stamp: 'jmv 7/5/2024 15:24:51'!
byteSize
	^self basicSize * self class bytesPerBasicElement
! !


!AffineTransformation methodsFor: 'objects from disk' stamp: 'jmv 7/5/2024 15:24:55'!
byteSize
	^self basicSize * self class bytesPerBasicElement! !


!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 7/5/2024 15:25:46'!
printSpaceAnalysis: threshold on: aStream
	"Warning!!
	This is most likely wrong. It was written for 32 bit V3 images.
	Please take a look at #spaceForInstancesOf:
	Review both methods, and any other one that might be related.
	"
	"
	SpaceTally new printSpaceAnalysis: 1 on:(FileStream forceNewFileNamed: 'STspace.text')
	"
	"sd-This method should be rewrote to be more coherent within the rest of the class 
	ie using preAllocate and spaceForInstanceOf:"

	"If threshold > 0, then only those classes with more than that number
	of instances will be shown, and they will be sorted by total instance space.
	If threshold = 0, then all classes will appear, sorted by name."

	| codeSpace instCount instSpace totalCodeSpace totalInstCount totalInstSpace eltSize n totalPercent percent |
	Smalltalk garbageCollect.
	totalCodeSpace := totalInstCount := totalInstSpace := n := 0.
	results := OrderedCollection new.
	'Taking statistics...'
		displayProgressAt: Sensor mousePoint
		from: 0 to: Smalltalk classNames size
		during: [ :barBlock |
			Smalltalk allClassesDo: [ :cl |
				codeSpace := cl spaceUsed.
				barBlock value: (n := n+1).
				Smalltalk garbageCollectMost.
				instCount := cl instanceCount.
				instSpace := 8 * instCount. "Object headers""Warning: The 3rd header word for big objects is not considered!!"
				cl isVariable
					ifTrue: [
						eltSize := cl bytesPerBasicElement.
						cl allInstancesDo: [ :x |
							instSpace := instSpace + (x basicSize * eltSize)]]
					ifFalse: [instSpace := instSpace + (cl instSize * instCount * 4)].
				results add: (SpaceTallyItem analyzedClassName: cl name codeSize: codeSpace instanceCount:  instCount spaceForInstances: instSpace).
				totalCodeSpace := totalCodeSpace + codeSpace.
				totalInstCount := totalInstCount + instCount.
				totalInstSpace := totalInstSpace + instSpace]].
	totalPercent := 0.0.

	aStream timeStamp.
	aStream
		nextPutAll: ('Class' padded: #right to: 30 with: $ );
		nextPutAll: ('code space' padded: #left to: 12 with: $ );
		nextPutAll: ('# instances' padded: #left to: 12 with: $ );
		nextPutAll: ('inst space' padded: #left to: 12 with: $ );
		nextPutAll: ('percent' padded: #left to: 8 with: $ ); newLine.

	threshold > 0 ifTrue: [
		"If inst count threshold > 0, then sort by space"
		results := (results select: [:s | s instanceCount >= threshold or: [s spaceForInstances > (totalInstSpace // 500)]])
			asArray sort: [:s :s2 | s spaceForInstances > s2 spaceForInstances]].

	results do: [:s |
		aStream
			nextPutAll: (s analyzedClassName padded: #right to: 30 with: $ );
			nextPutAll: (s codeSize printString padded: #left to: 12 with: $ );
			nextPutAll: (s instanceCount printString padded: #left to: 12 with: $ );
			nextPutAll: (s spaceForInstances printString padded: #left to: 14 with: $ ).
		percent := s spaceForInstances*100.0/totalInstSpace.
		totalPercent := totalPercent + percent.
		percent >= 0.1 ifTrue: [
			percent printOn: aStream integerDigits: 6 padWith: $  fractionDigits: 1 positiveIndicator: nil ].
		aStream newLine].

	aStream
		newLine; nextPutAll: ('Total' padded: #right to: 30 with: $ );
		nextPutAll: (totalCodeSpace printString padded: #left to: 12 with: $ );
		nextPutAll: (totalInstCount printString padded: #left to: 12 with: $ );
		nextPutAll: (totalInstSpace printString padded: #left to: 14 with: $ ).
	totalPercent printOn: aStream integerDigits: 6 padWith: $  fractionDigits: 1 positiveIndicator: nil! !

!methodRemoval: AffineTransformation #bytesPerBasicElement stamp: 'jmv 7/5/2024 15:31:05'!
AffineTransformation removeSelector: #bytesPerBasicElement!
!methodRemoval: ArrayedCollection #bytesPerBasicElement stamp: 'jmv 7/5/2024 15:31:03'!
ArrayedCollection removeSelector: #bytesPerBasicElement!
