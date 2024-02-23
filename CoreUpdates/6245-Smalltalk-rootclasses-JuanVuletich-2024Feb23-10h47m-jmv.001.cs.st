'From Cuis6.3 [latest update: #6244] on 23 February 2024 at 10:47:39 am'!

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 2/23/2024 10:36:18'!
rootClasses
	"
	Answer class hierarchy roots. This is, classes without superclass.
	Smalltalk rootClasses
	"
	^Array streamContents: [ :strm |
		self do: [ :root |
			(root isBehavior and: [root superclass isNil]) ifTrue: [	"Grab ProtoObject and any other alike"
				strm nextPut: root ]]]! !

