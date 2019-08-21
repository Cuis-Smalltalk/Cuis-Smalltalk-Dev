'From Cuis 5.0 [latest update: #3843] on 21 August 2019 at 9:20:26 am'!

!Behavior methodsFor: 'accessing class hierarchy' stamp: 'HAW 8/21/2019 00:20:09'!
addAllSubclassesTo: allSubclasses 

	self subclassesDo: [ :aSubclass | 
		allSubclasses add: aSubclass.
		aSubclass addAllSubclassesTo: allSubclasses ]! !


!Behavior methodsFor: 'accessing class hierarchy' stamp: 'HAW 8/21/2019 09:16:33'!
allSubclasses
	"Answer a Set of the receiver, the receiver's descendent's, and the  
	receiver's descendent's subclasses."

	| allSubclasses |
	
	allSubclasses := OrderedCollection new.
	self addAllSubclassesTo: allSubclasses.
	
	^allSubclasses ! !

!Behavior methodsFor: 'accessing class hierarchy' stamp: 'HAW 8/21/2019 09:16:38'!
withAllSubclasses
	"Answer a Set of the receiver, the receiver's descendent's, and the  
	receiver's descendent's subclasses."

	| allSubclasses |
	
	allSubclasses := OrderedCollection with: self.
	self addAllSubclassesTo: allSubclasses.
	
	^allSubclasses ! !

