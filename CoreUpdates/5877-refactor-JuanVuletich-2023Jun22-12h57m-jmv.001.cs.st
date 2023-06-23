'From Cuis 6.0 [latest update: #5876] on 23 June 2023 at 3:52:21 pm'!

!ClassBuilder methodsFor: 'class format' stamp: 'jmv 6/23/2023 11:17:12'!
computeFormat: type fullInstancesSize: newInstSize
	"Compute the new format for making oldClass a subclass of newSuper.
	 Answer the format or nil if there is any problem."

	"Only for Spur!!"

	| isVar isPointers isWeak bitsUnitSize |
	type == #compiledMethod ifTrue: [
		newInstSize > 0 ifTrue: [
			self error: 'A compiled method class cannot have named instance variables'.
			^nil].
		^CompiledMethod format].
	newInstSize > 65535 ifTrue: [
		self error: 'Class has too many instance variables (', newInstSize printString,')'.
		^nil].
	type == #normal ifTrue:[isVar := isWeak := false. isPointers := true].
	type == #bytes ifTrue:[isVar := true. bitsUnitSize := 1. isPointers := isWeak := false].
	type == #shorts ifTrue:[isVar := true. bitsUnitSize := 2. isPointers := isWeak := false].
	type == #words ifTrue:[isVar := true. bitsUnitSize := 4. isPointers := isWeak := false].
	type == #longs ifTrue:[isVar := true. bitsUnitSize := 8. isPointers := isWeak := false].
	type == #variable ifTrue:[isVar := isPointers := true. isWeak := false].
	type == #weak ifTrue:[isVar := isWeak := isPointers := true].
	type == #ephemeron ifTrue:[isVar := false. isWeak := isPointers := true].
	type == #immediate ifTrue:[isVar := isWeak := isPointers := false].
	isVar ifNil: [ self error: 'Unsupported class format type: ', type. ^ nil ].
	(isPointers not and: [newInstSize > 0]) ifTrue: [
		self error: 'A non-pointer class cannot have named instance variables'.
		^nil].
	^self format: newInstSize variable: isVar bitsUnitSize: bitsUnitSize pointers: isPointers weak: isWeak! !


!ClassBuilder methodsFor: 'class format' stamp: 'jmv 6/23/2023 11:18:51'!
computeFormat: type instSize: newInstSize forSuper: newSuper ccIndex: ccIndex
	"Compute the new format for making oldClass a subclass of newSuper.
	Return the format or nil if there is any problem."

	| instSize isVar isWords isPointers isWeak |

	"Spur uses this version"
	Smalltalk isSpur ifTrue: [
		^ self computeFormat: type fullInstancesSize: (newInstSize + (newSuper ifNil:[0] ifNotNil:[newSuper instSize])) ].

	"This for preSpur images"
	type == #compiledMethod
		ifTrue:[^CompiledMethod format].
	instSize := newInstSize + (newSuper ifNil:[0] ifNotNil:[newSuper instSize]).
	instSize > 254 ifTrue:[
		self error: 'Class has too many instance variables (', instSize printString,')'.
		^nil].
	type == #normal ifTrue:[isVar := isWeak := false. isWords := isPointers := true].
	type == #bytes ifTrue:[isVar := true. isWords := isPointers := isWeak := false].
	type == #words ifTrue:[isVar := isWords := true. isPointers := isWeak := false].
	type == #variable ifTrue:[isVar := isPointers := isWords := true. isWeak := false].
	type == #weak ifTrue:[isVar := isWeak := isWords := isPointers := true].
	isVar ifNil: [ self error: 'Unsupported class format type: ', type. ^ nil ].
	(isPointers not and:[instSize > 0]) ifTrue:[
		self error:'A non-pointer class cannot have instance variables'.
		^nil].
	^(self format: instSize 
		variable: isVar 
		words: isWords 
		pointers: isPointers 
		weak: isWeak) + (ccIndex bitShift: 11)! !

!methodRemoval: ClassBuilder #computeFormat:instSize:forSuper: stamp: 'jmv 6/23/2023 11:18:55'!
ClassBuilder removeSelector: #computeFormat:instSize:forSuper:!
