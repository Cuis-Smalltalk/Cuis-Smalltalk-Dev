'From Cuis 6.0 [latest update: #5947] on 25 July 2023 at 6:45:25 pm'!

!MessageNode methodsFor: 'macro transformations' stamp: 'jmv 7/25/2023 18:44:52'!
transformAnd: encoder
	(Preferences at: #allowNonBooleanReceiversOfBooleanMessages) ifTrue: [
		(receiver == NodeTrue) | (receiver == NodeFalse) ifFalse: [
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

!MessageNode methodsFor: 'macro transformations' stamp: 'jmv 7/25/2023 18:44:56'!
transformIfFalse: encoder
	(Preferences at: #allowNonBooleanReceiversOfBooleanMessages) ifTrue: [
		(receiver == NodeTrue) | (receiver == NodeFalse) ifFalse: [
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

!MessageNode methodsFor: 'macro transformations' stamp: 'jmv 7/25/2023 18:44:59'!
transformIfFalseIfTrue: encoder
	(Preferences at: #allowNonBooleanReceiversOfBooleanMessages) ifTrue: [
		(receiver == NodeTrue) | (receiver == NodeFalse) ifFalse: [
			^false ]].
	^(self checkBlock: (arguments at: 1) as: 'False arg' from: encoder maxArgs: 0)
	   and: [(self checkBlock: (arguments at: 2) as: 'True arg' from: encoder maxArgs: 0)
	   and: [selector := SelectorNode new key: #ifTrue:ifFalse: code: #macro.
			arguments swap: 1 with: 2.
			arguments do: [:arg| arg noteOptimizedIn: self].
			true]].! !

!MessageNode methodsFor: 'macro transformations' stamp: 'jmv 7/25/2023 18:45:03'!
transformIfTrue: encoder
	(Preferences at: #allowNonBooleanReceiversOfBooleanMessages) ifTrue: [
		(receiver == NodeTrue) | (receiver == NodeFalse) ifFalse: [
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

!MessageNode methodsFor: 'macro transformations' stamp: 'jmv 7/25/2023 18:45:06'!
transformIfTrueIfFalse: encoder
	(Preferences at: #allowNonBooleanReceiversOfBooleanMessages) ifTrue: [
		(receiver == NodeTrue) | (receiver == NodeFalse) ifFalse: [
			^false ]].
	^(self checkBlock: (arguments at: 1) as: 'True arg' from: encoder maxArgs: 0)
	   and: [(self checkBlock: (arguments at: 2) as: 'False arg' from: encoder maxArgs: 0)
	   and: [arguments do: [:arg| arg noteOptimizedIn: self].
			true]].! !

!MessageNode methodsFor: 'macro transformations' stamp: 'jmv 7/25/2023 18:45:09'!
transformOr: encoder
	(Preferences at: #allowNonBooleanReceiversOfBooleanMessages) ifTrue: [
		(receiver == NodeTrue) | (receiver == NodeFalse) ifFalse: [
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

