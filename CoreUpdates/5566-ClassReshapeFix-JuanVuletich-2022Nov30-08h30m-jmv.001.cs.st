'From Cuis 6.0 [latest update: #5559] on 30 November 2022 at 8:44:48 am'!

!ClassBuilder methodsFor: 'class mutation' stamp: 'jmv 11/30/2022 08:42:55'!
mutateInstances: oldClass to: newClass
	"Mutate the old class and subclasses into newClass and subclasses.
	Note: This method is slightly different from: #mutate:toSuper: since
	here we are at the root of reshaping and have two distinct roots."

	self showProgressFor: oldClass.
	"Convert the subclasses"
	oldClass subclasses do: [ :oldSubclass | | newSubclass |
		newSubclass _ newClass subclasses detect: [ :c | c name = oldSubclass name ]. 
		self mutateInstances: oldSubclass to: newSubclass.
	].
	"And any obsolete ones"
	oldClass obsoleteSubclasses do: [ :oldSubclass | | newSubclass |
		oldSubclass ifNotNil: [
			newSubclass _ newClass subclasses detect: [ :c | c name = oldSubclass name ] ifNone: []. 
			newSubclass ifNotNil: [
				self mutateInstances: oldSubclass to: newSubclass ].
		].
	].
	self update: oldClass to: newClass.
	^newClass! !

