'From Cuis 6.0 [latest update: #6028] on 14 September 2023 at 11:14:24 am'!

!Class methodsFor: 'initialization' stamp: 'jmv 9/14/2023 10:10:53'!
declareClassVariables: newVars
	"Declare class variables common to all instances.
	Answer whether recompilation is advisable."

	| conflicts |
	conflicts := false.
	classPool
		ifNotNil: [
			(classPool keys reject: [ :x | newVars includes: x ]) 
				do: [ :var | self removeClassVarName: var ]].
	(newVars reject: [:var | self classPool includesKey: var])
		do: [ :var | "adding"
			"check if new vars defined elsewhere"
			(self bindingOf: var)
				ifNotNil: [ 
					self error: var, ' is defined elsewhere'.
					conflicts := true ]].
	newVars size > 0
		ifTrue: [
			"in case it was nil"
			classPool := self classPool.
			newVars do: [:var |
				classPool declare: var from: 
					(Smalltalk at: #Undeclared ifAbsent: nil) ]].
	^ conflicts.! !


!Dictionary methodsFor: 'adding' stamp: 'jmv 9/14/2023 10:06:58'!
declare: key from: aDictionary
	"Add key to the receiver. If key already exists, do nothing. If aDictionary 
	includes key, then remove it from aDictionary and use its association as 
	the element of the receiver. Answer it.
	If the key wasn't in the receiver or aDictionary, use nil as the value.
	Note: We allow aDictionary to be nil, although it normally is not."

	| association |
	(self includesKey: key) ifTrue: [^ nil].
	(aDictionary notNil and: [aDictionary includesKey: key])
		ifTrue: [
			association := aDictionary associationAt: key.
			self add: association.
			aDictionary removeKey: key.
			^ association ]
		ifFalse: [
			self at: key put: nil.
			^ nil ]! !

