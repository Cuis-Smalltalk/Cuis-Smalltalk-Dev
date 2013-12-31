'From Cuis 4.0 of 16 November 2011 [latest update: #1144] on 21 March 2012 at 9:20:42 am'!

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 3/21/2012 09:20'!
  fileOutMethodAdditionsFor: class on: stream
	"Write out all the method changes for this class."

	| changes |
	changes _ Set new.
	(self methodChangesAtClass: class name) associationsDo: [ :mAssoc |
		mAssoc value == #add
			ifTrue: [ changes add: mAssoc key ]].
	changes isEmpty ifFalse: [
		class fileOutChangedMessages: changes on: stream.
		stream newLine ]! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 3/21/2012 09:12'!
                fileOutMethodChangesFor: class on: stream
	"Write out all the method changes for this class."

	| changes |
	changes _ Set new.
	(self methodChangesAtClass: class name) associationsDo: [ :mAssoc |
		(mAssoc value == #remove
			or: [ mAssoc value == #addedThenRemoved 
				or: [ mAssoc value == #add ]])
			ifFalse: [ changes add: mAssoc key ]].
	changes isEmpty ifFalse: [
		class fileOutChangedMessages: changes on: stream.
		stream newLine ]! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 3/21/2012 09:13'!
                             fileOutOn: stream 
	"Write out all the changes the receiver knows about"

	| classList |
	(self isEmpty and: [ stream isKindOf: FileStream ])
		ifTrue: [ self inform: 'Warning: no changes to file out' ].
	classList _ ChangeSet superclassOrder: self changedClasses asOrderedCollection.

	"First put out rename, max classDef and comment changes."
	classList do: [ :aClass | self fileOutClassDefinition: aClass on: stream ].

	"Then put out all the method additions"
	classList do: [ :aClass | self fileOutMethodAdditionsFor: aClass on: stream ].

	"Then put out all the method changes"
	classList do: [ :aClass | self fileOutMethodChangesFor: aClass on: stream ].

	"Finally put out removals, final class defs and reorganization if any"
	classList reverseDo: [ :aClass |
		self fileOutMethodRemovalsFor: aClass on: stream.
		self fileOutPSFor: aClass on: stream ].

	self classRemoves sort do: [ :aClassName |
		stream nextPut: $!!; nextChunkPut: ('classRemoval: #', aClassName); newLine.
		stream nextChunkPut: 'Smalltalk removeClassNamed: #', aClassName; newLine ]! !


!MethodChangeRecord methodsFor: 'as yet unclassified' stamp: 'jmv 3/21/2012 09:09'!
                              noteChangeType: newChangeType

	"Change of an added method, is still an add"
	(changeType == #add and: [ newChangeType == #change ])
		ifTrue: [
			^self ].

	"Change of an added method, is still an add"
	(changeType == #addedThenRemoved and: [ newChangeType == #change ])
		ifTrue: [
			changeType _ #add.
			^self ].

	changeType _ newChangeType! !

!methodRemoval: ChangeSet #fileOutChangesFor:on:!
ChangeSet removeSelector: #fileOutChangesFor:on:!
