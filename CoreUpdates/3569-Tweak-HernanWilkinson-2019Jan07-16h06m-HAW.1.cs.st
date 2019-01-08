'From Cuis 5.0 of 7 November 2016 [latest update: #3562] on 7 January 2019 at 4:23:57 pm'!

!ClassBuilder methodsFor: 'validation' stamp: 'HAW 1/7/2019 16:20:34'!
doesClassNameStartWithUppercase: aClassName
	
	^ aClassName first isUppercase! !


!ClassBuilder methodsFor: 'validation' stamp: 'HAW 1/7/2019 16:20:34'!
validateClassName: aString
	"Validate the new class name"
	
	(self doesClassNameStartWithUppercase: aString) ifFalse:[
		self error: 'Class names must be capitalized'.
		^false].
	Smalltalk at: aString ifPresent:[:old|
		(old isKindOf: Behavior) ifFalse:[
			self notify: aString asText allBold, 
						' already exists!!\Proceed will store over it.' withNewLines]].
	^true! !


!ClassBuilder reorganize!
('class definition' class:instanceVariableNames:unsafe: name:subclassOf:type:instanceVariableNames:classVariableNames:poolDictionaries:category: name:subclassOf:type:instanceVariableNames:classVariableNames:poolDictionaries:category:unsafe: needsSubclassOf:type:instanceVariables:from: newSubclassOf:type:instanceVariables:from: recompile:from:to:mutate: silentlyMoveInstVarNamed:from:to:after:)
('class format' computeFormat:instSize:forSuper: computeFormat:instSize:forSuper:ccIndex: format:variable:bitsUnitSize:pointers:weak: format:variable:words:pointers:weak:)
('validation' doesClassNameStartWithUppercase: validateClass:forMoving:downTo: validateClass:forMoving:upTo: validateClassName: validateClassvars:from:forSuper: validateInstvars:from:forSuper: validateSubclass:canKeepLayoutFrom:forSubclassFormat: validateSubclassFormat:from:forSuper:extra: validateSuperclass:forSubclass:)
('private' informUserDuring: privateNewSubclassOf: privateNewSubclassOf:from: recordClass:replacedBy: reservedNames showProgressFor: tooDangerousClasses)
('public' class:instanceVariableNames: moveInstVarNamed:from:to:after: superclass:ephemeronSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: superclass:immediateSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: superclass:subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: superclass:variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: superclass:variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: superclass:variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: superclass:weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:)
('class mutation' mutate:to: reshapeClass:toSuper: update:to:)
('initialization' doneCompiling: initialize)
!

