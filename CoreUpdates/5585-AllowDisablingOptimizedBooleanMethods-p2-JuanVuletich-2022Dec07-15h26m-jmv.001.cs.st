'From Cuis 6.0 [latest update: #5583] on 7 December 2022 at 3:27:38 pm'!

!MessageNode class methodsFor: 'class initialization' stamp: 'jmv 12/7/2022 14:37:19'!
disableNonBooleanReceiversOfBooleanMessages
	"
	Evaluate this to disallow alternative Boolean classes.
	Boolean messages will be optimized by the Compiler, by assuming receiver is a Boolean.

	MessageNode disableNonBooleanReceiversOfBooleanMessages
	"
	Preferences at: #allowNonBooleanReceiversOfBooleanMessages put: false.
	Compiler recompileAll! !

!MessageNode class methodsFor: 'class initialization' stamp: 'jmv 12/7/2022 14:37:12'!
enableNonBooleanReceiversOfBooleanMessages
	"
	Evaluate this to allow alternative Boolean classes.
	Boolean messages will be actual method sends.

	MessageNode enableNonBooleanReceiversOfBooleanMessages
	"
	Preferences at: #allowNonBooleanReceiversOfBooleanMessages put: true.
	Compiler recompileAll! !


!MessageNode methodsFor: 'macro transformations' stamp: 'jmv 12/7/2022 15:07:18'!
transformAnd: encoder
	(Preferences at: #allowNonBooleanReceiversOfBooleanMessages) ifTrue: [
		(receiver == NodeTrue) | (receiver == NodeTrue) ifFalse: [
			^false ]].
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				Array 
					with: ((arguments at: 1) noteOptimizedIn: self)
					with: ((BlockNode withJust: NodeFalse) noteOptimizedIn: self).
			^true]
		ifFalse: 
			[^false]! !

!MessageNode methodsFor: 'macro transformations' stamp: 'jmv 12/7/2022 15:07:22'!
transformIfFalse: encoder
	(Preferences at: #allowNonBooleanReceiversOfBooleanMessages) ifTrue: [
		(receiver == NodeTrue) | (receiver == NodeTrue) ifFalse: [
			^false ]].
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				Array 
					with: ((BlockNode withJust: NodeNil) noteOptimizedIn: self)
					with: ((arguments at: 1) noteOptimizedIn: self).
			^true]
		ifFalse:
			[^false]! !

!MessageNode methodsFor: 'macro transformations' stamp: 'jmv 12/7/2022 15:07:25'!
transformIfFalseIfTrue: encoder
	(Preferences at: #allowNonBooleanReceiversOfBooleanMessages) ifTrue: [
		(receiver == NodeTrue) | (receiver == NodeTrue) ifFalse: [
			^false ]].
	^(self checkBlock: (arguments at: 1) as: 'False arg' from: encoder maxArgs: 0)
	   and: [(self checkBlock: (arguments at: 2) as: 'True arg' from: encoder maxArgs: 0)
	   and: [selector := SelectorNode new key: #ifTrue:ifFalse: code: #macro.
			arguments swap: 1 with: 2.
			arguments do: [:arg| arg noteOptimizedIn: self].
			true]]! !

!MessageNode methodsFor: 'macro transformations' stamp: 'jmv 12/7/2022 15:07:29'!
transformIfTrue: encoder
	(Preferences at: #allowNonBooleanReceiversOfBooleanMessages) ifTrue: [
		(receiver == NodeTrue) | (receiver == NodeTrue) ifFalse: [
			^false ]].
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				Array 
					with: ((arguments at: 1) noteOptimizedIn: self)
					with: ((BlockNode withJust: NodeNil) noteOptimizedIn: self).
			^true]
		ifFalse: 
			[^false]! !

!MessageNode methodsFor: 'macro transformations' stamp: 'jmv 12/7/2022 15:07:33'!
transformIfTrueIfFalse: encoder
	(Preferences at: #allowNonBooleanReceiversOfBooleanMessages) ifTrue: [
		(receiver == NodeTrue) | (receiver == NodeTrue) ifFalse: [
			^false ]].
	^(self checkBlock: (arguments at: 1) as: 'True arg' from: encoder maxArgs: 0)
	   and: [(self checkBlock: (arguments at: 2) as: 'False arg' from: encoder maxArgs: 0)
	   and: [arguments do: [:arg| arg noteOptimizedIn: self].
			true]]! !

!MessageNode methodsFor: 'macro transformations' stamp: 'jmv 12/7/2022 15:07:37'!
transformOr: encoder
	(Preferences at: #allowNonBooleanReceiversOfBooleanMessages) ifTrue: [
		(receiver == NodeTrue) | (receiver == NodeTrue) ifFalse: [
			^false ]].
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				Array 
					with: ((BlockNode withJust: NodeTrue) noteOptimizedIn: self)
					with: ((arguments at: 1) noteOptimizedIn: self).
			^true]
		ifFalse: 
			[^false]! !

