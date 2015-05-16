'From Cuis 4.2 of 25 July 2013 [latest update: #2323] on 14 May 2015 at 10:19:08.207784 am'!

!RunArray methodsFor: 'copying' stamp: 'jmv 5/14/2015 09:45'!
copy

	^self copyFrom: 1 to: self size! !


!Text methodsFor: 'copying' stamp: 'jmv 5/14/2015 10:00'!
postCopy

	string _ string copy.
	runs _ runs copy! !


!OrderedCollection methodsFor: 'copying' stamp: 'jmv 5/14/2015 09:45'!
copy

	^self copyFrom: 1 to: self size! !


!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 5/14/2015 09:16'!
newSubclassOf: newSuper type: type instanceVariables: instVars from: oldClass
	"Create a new subclass of the given superclass with the given specification."
	| newFormat newClass |
	"Compute the format of the new class"
	newFormat _ 
		self computeFormat: type 
			instSize: instVars size 
			forSuper: newSuper 
			ccIndex: (oldClass ifNil:[0] ifNotNil:[oldClass indexIfCompact]).

	newFormat
		ifNil: [^ nil].

	(oldClass == nil or:[oldClass isMeta not]) 
		ifTrue:[newClass _ self privateNewSubclassOf: newSuper from: oldClass]
		ifFalse:[newClass _ oldClass shallowCopy].

	newClass 
		superclass: newSuper
		methodDictionary: MethodDictionary new
		format: newFormat;
		setInstVarNames: instVars.

	oldClass ifNotNil:[
		newClass organization: oldClass organization.
		"Recompile the new class"
		oldClass hasMethods 
			ifTrue:[newClass compileAllFrom: oldClass].
		self recordClass: oldClass replacedBy: newClass.
	].

	(oldClass == nil or:[oldClass isObsolete not]) 
		ifTrue:[newSuper addSubclass: newClass]
		ifFalse:[newSuper addObsoleteSubclass: newClass].

	^newClass! !

!ClassBuilder methodsFor: 'private' stamp: 'jmv 5/14/2015 09:16'!
privateNewSubclassOf: newSuper from: oldClass
	"Create a new meta and non-meta subclass of newSuper using oldClass as template"
	"WARNING: This method does not preserve the superclass/subclass invariant!!"
	| newSuperMeta oldMeta newMeta |
	oldClass ifNil:[^self privateNewSubclassOf: newSuper].
	newSuperMeta _ newSuper ifNil:[Class] ifNotNil:[newSuper class].
	oldMeta _ oldClass class.
	newMeta _ oldMeta shallowCopy.
	newMeta 
		superclass: newSuperMeta
		methodDictionary: MethodDictionary new
		format: (self computeFormat: oldMeta typeOfClass 
					instSize: oldMeta instVarNames size 
					forSuper: newSuperMeta
					ccIndex: 0);
		setInstVarNames: oldMeta instVarNames;
		organization: oldMeta organization.
	"Recompile the meta class"
	oldMeta hasMethods 
		ifTrue:[newMeta compileAllFrom: oldMeta].
	"Record the meta class change"
	self recordClass: oldMeta replacedBy: newMeta.
	"And create a new instance"
	^newMeta adoptInstance: oldClass from: oldMeta! !


!ArrayedCollection methodsFor: 'sorting' stamp: 'jmv 5/14/2015 09:19'!
mergeSortFrom: startIndex to: stopIndex by: aBlock
	"Sort the given range of indices using the mergesort algorithm.
	Mergesort is a worst-case O(N log N) sorting algorithm that usually
	does only half as many comparisons as heapsort or quicksort."

	"Details: recursively split the range to be sorted into two halves,
	mergesort each half, then merge the two halves together. An extra 
	copy of the data is used as temporary storage and successive merge 
	phases copy data back and forth between the receiver and this copy.
	The recursion is set up so that the final merge is performed into the
	receiver, resulting in the receiver being completely sorted."

	self size <= 1 ifTrue: [^ self].  "nothing to do"
	startIndex = stopIndex ifTrue: [^ self].
	self assert: [startIndex >= 1 and: [startIndex < stopIndex]]. "bad start index"
	self assert: [stopIndex <= self size]. "bad stop index"
	self
		mergeSortFrom: startIndex
		to: stopIndex 
		src: self copy 
		dst: self 
		by: aBlock! !


!FloatArray methodsFor: 'arithmetic' stamp: 'jmv 5/14/2015 09:52'!
* anObject

	^self copy *= anObject! !

!FloatArray methodsFor: 'arithmetic' stamp: 'jmv 5/14/2015 09:52'!
+ anObject

	^self copy += anObject! !

!FloatArray methodsFor: 'arithmetic' stamp: 'jmv 5/14/2015 09:52'!
- anObject

	^self copy -= anObject! !

!FloatArray methodsFor: 'arithmetic' stamp: 'jmv 5/14/2015 09:52'!
/ anObject

	^self copy /= anObject! !

!FloatArray methodsFor: 'arithmetic' stamp: 'jmv 5/14/2015 09:52'!
negated

	^self copy *= -1! !


!EventSensor methodsFor: 'private-I/O' stamp: 'jmv 5/14/2015 09:51'!
queueEvent: evt
	"Queue the given event in the event queue (if any).
	Note that the event buffer must be copied since it
	will be reused later on."

	eventQueue nextPut: evt copy! !


!MouseEvent methodsFor: 'converting' stamp: 'jmv 5/14/2015 09:52'!
asMouseEnter

	^self copy setType: #mouseEnter! !

!MouseEvent methodsFor: 'converting' stamp: 'jmv 5/14/2015 09:53'!
asMouseLeave

	^self copy setType: #mouseLeave! !


!Scanner methodsFor: 'expression types' stamp: 'jmv 5/14/2015 09:53'!
checkpoint
	"Return a copy of all changeable state.  See revertToCheckpoint:"

	^ {self copy. source copy. currentComment copy}! !

!methodRemoval: UndefinedObject #clone!
UndefinedObject removeSelector: #clone!
!methodRemoval: SmallInteger #clone!
SmallInteger removeSelector: #clone!
!methodRemoval: Character #clone!
Character removeSelector: #clone!
!methodRemoval: Character #copy!
Character removeSelector: #copy!
!methodRemoval: MappedCollection #copy!
MappedCollection removeSelector: #copy!

!MappedCollection reorganize!
('accessing' at: at:put: contents size)
('adding' add:)
('enumerating' collect: do: select:)
('printing' storeOn:)
('private' setCollection:map: species)
!

!methodRemoval: Interval #copy!
Interval removeSelector: #copy!
!methodRemoval: Heap #copy!
Heap removeSelector: #copy!
!methodRemoval: Text #copy!
Text removeSelector: #copy!
!methodRemoval: Symbol #clone!
Symbol removeSelector: #clone!
!methodRemoval: Symbol #copy!
Symbol removeSelector: #copy!
!methodRemoval: Bitmap #copy!
Bitmap removeSelector: #copy!
!methodRemoval: SequenceableCollection #copy!
SequenceableCollection removeSelector: #copy!
!methodRemoval: Boolean #clone!
Boolean removeSelector: #clone!
!methodRemoval: Object #clone!
Object removeSelector: #clone!
