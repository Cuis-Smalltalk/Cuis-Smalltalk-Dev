'From Cuis 5.0 [latest update: #5012] on 29 December 2021 at 5:40:25 pm'!

!ClassBuilder methodsFor: 'class mutation' stamp: 'jmv 12/29/2021 17:34:55'!
createHierarchy: oldClass to: newClass
	"Mutate the old class and subclasses into newClass and subclasses.
	Note: This method is slightly different from: #mutate:toSuper: since
	here we are at the root of reshaping and have two distinct roots."

	self showProgressFor: oldClass.
	"Convert the subclasses"
	oldClass subclasses do:[:oldSubclass| | newSubclass |
		newSubclass := self reshapeClass: oldSubclass toSuper: newClass.
		self createHierarchy: oldSubclass to: newSubclass.
	].
	"And any obsolete ones"
	oldClass obsoleteSubclasses do:[:oldSubclass| | newSubclass |
		oldSubclass ifNotNil:[
			newSubclass := self reshapeClass: oldSubclass toSuper: newClass.
			self createHierarchy: oldSubclass to: newSubclass.
		].
	].
	^newClass! !

!ClassBuilder methodsFor: 'class mutation' stamp: 'jmv 12/29/2021 17:34:42'!
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
			newSubclass _ newClass subclasses detect: [ :c | c name = oldSubclass name ]. 
			self mutateInstances: oldSubclass to: newSubclass.
		].
	].
	self update: oldClass to: newClass.
	^newClass! !


!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 12/29/2021 17:36:28'!
recompile: force from: oldClass to: newClass mutate: forceMutation
	"Do the necessary recompilation after changing oldClass to newClass.
	If required (e.g., when oldClass ~~ newClass) mutate oldClass to newClass
	and all its subclasses. If forceMutation is true force a mutation even
	if oldClass and newClass are the same."

	oldClass
		ifNil: [^ newClass].

	(newClass == oldClass and:[force not and:[forceMutation not]]) ifTrue:[
		^newClass].

	currentClassIndex _ 0.
	maxClassIndex _ oldClass withAllSubclasses size.

	(oldClass == newClass and:[forceMutation not]) ifTrue:[
		"Recompile from newClass without mutating"
		self informUserDuring:[
			newClass withAllSubclassesDo: [ :cl |
				self showProgressFor: cl.
				cl compileAll]].
		^newClass].
	"Recompile and mutate oldClass to newClass"
	self informUserDuring: [
		self createHierarchy: oldClass to: newClass.
		self mutateInstances: oldClass to: newClass.
	].
	^oldClass "now mutated to newClass"! !

!ClassBuilder methodsFor: 'private' stamp: 'jmv 12/29/2021 17:35:58'!
showProgressFor: aClass
	"Announce that we're processing aClass"
	progress
		ifNil: [^ self].
	aClass isObsolete ifTrue:[^self].
	currentClassIndex _ currentClassIndex + 1.
	(aClass hasMethods and: [aClass wantsRecompilationProgressReported]) ifTrue: [
		[	"As we might be recompiling the  classes involved in showing progress, ignore errors. After all, they are harmless."
			progress value: ('Recompiling ', aClass name,' (', currentClassIndex printString,'/', maxClassIndex printString,')')
		] on: Error do: nil]! !

!methodRemoval: ClassBuilder #mutate:to: stamp: 'jmv 12/29/2021 17:39:52'!
ClassBuilder removeSelector: #mutate:to:!
