'From Cuis 5.0 of 7 November 2016 [latest update: #3430] on 19 September 2018 at 10:17:16 am'!

!ContentPack methodsFor: 'exporting' stamp: 'jmv 9/19/2018 10:08:59'!
exportInto: aDirectory

	aDirectory assureExistence.

	self associations do: [ :assoc |
		| klass thisDirectory fullPath | 
			klass _ assoc value class.
			thisDirectory _ aDirectory / assoc key.
			fullPath _  (aDirectory // assoc key) pathName.

		self flag: #note. "Add support for new file export type mappings here. --cbr"
		klass = ContentPack
			ifTrue: [ assoc value exportInto: thisDirectory ].
		
		klass = ColorForm
			ifTrue: [ assoc value writeBMPfileNamed: fullPath ].
			
		klass = Form
			ifTrue: [ assoc value writeBMPfileNamed: fullPath ]
	]! !

