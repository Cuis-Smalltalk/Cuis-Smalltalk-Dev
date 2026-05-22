Class {
	#name : 'MethodReferenceTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Tools-Browser',
	#package : 'BaseImageTests',
	#tag : 'Tools-Browser'
}

{ #category : 'test string version' }
MethodReferenceTest >> test01DefaultStringVersionIsClassAndSelector [

	| stringVersion |
	
	stringVersion := (MethodReference method: thisContext method) stringVersion.
	
	self assert: stringVersion equals: self class name, ' ', thisContext selector.
]

{ #category : 'test string version' }
MethodReferenceTest >> test02CanPrefixStringVersion [

	| methodReference prefix |
	
	prefix := 'a prefix'.
	methodReference := MethodReference method: thisContext method.
	methodReference prefixStringVersionWith: prefix.
	
	self assert: methodReference stringVersion equals: '[',prefix,'] - ',self class name, ' ', thisContext selector.
]

{ #category : 'test actual class' }
MethodReferenceTest >> testActualClassIfAbsentEvaluatesBlockIfClassDoesNotExist [

	self 
		assert: ((MethodReference 
			classSymbol: #X_Y_Z 
			classIsMeta: false 
			methodSymbol: #xx) actualClassIfAbsent: [ 1])
		equals: 1.
]

{ #category : 'test actual class' }
MethodReferenceTest >> testActualClassIfAbsentReturnsClassIfClassExists [

	self 
		assert: ((MethodReference 
			class: self class  
			selector: thisContext selector) actualClassIfAbsent: [ self fail])
		equals: self class.
]

{ #category : 'test actual class' }
MethodReferenceTest >> testActualClassReturnNilWhenClassDoesNotExist [

	self assert: (MethodReference classSymbol: #X_Y_Z classIsMeta: false methodSymbol: #xx) actualClass isNil
]

{ #category : 'test actual class' }
MethodReferenceTest >> testActualClassReturnsClassIfExists [

	self 
		assert: (MethodReference 
			class: self class 
			selector: thisContext selector) actualClass 
		equals: self class.
]

{ #category : 'test compiled method' }
MethodReferenceTest >> testCompiledMethodIfAbsentEvaluatesAbsentBlockWhenActualClassIsNil [

	self 
		assert: ((MethodReference 
			classSymbol: #X_Y_Z 
			classIsMeta: false 
			methodSymbol: #xx) compiledMethodIfAbsent: [ 1])
		equals: 1.
]

{ #category : 'test compiled method' }
MethodReferenceTest >> testCompiledMethodIfAbsentEvaluatesAbsentBlockWhenSelectorIsNotImplemented [

	self 
		assert: ((MethodReference 
			classSymbol: self class name
			classIsMeta: false 
			methodSymbol: #xx) compiledMethodIfAbsent: [ 1])
		equals: 1.
]

{ #category : 'test compiled method' }
MethodReferenceTest >> testCompiledMethodIfAbsentReturnsImplementedMethodWhenItExists [

	self 
		assert: ((MethodReference 
			classSymbol: self class name
			classIsMeta: false 
			methodSymbol: thisContext selector) compiledMethodIfAbsent: [ self fail ])
		equals: thisContext method .
]

{ #category : 'test testing' }
MethodReferenceTest >> testReferencesParameterAtReturnsFalseWhenInvalid [

	self deny: ((MethodReference 
		classSymbol: #X_Y_Z 
		classIsMeta: false 
		methodSymbol: #xx) referencesParameterAt: 1)
]

{ #category : 'test testing' }
MethodReferenceTest >> testReferencesParameterAtReturnsTrueWhenTheParameterIsReferenced [

	self assert: ((MethodReference 
		classSymbol: #MethodReference 
		classIsMeta: false 
		methodSymbol: #referencesParameterAt:) referencesParameterAt: 1)
]

{ #category : 'test source code' }
MethodReferenceTest >> testSourceCodeIfAbsentEvaluatesBlockIfClassDoesNotExist [

	self 
		assert: ((MethodReference classSymbol: #X_Y_Z classIsMeta: false methodSymbol:#xx) sourceCodeIfAbsent: [ 1 ])
		equals: 1.
]

{ #category : 'test source code' }
MethodReferenceTest >> testSourceCodeIfAbsentEvaluatesBlockIfMethodDoesNotExist [

	self 
		assert: ((MethodReference class: self class selector:#xx) sourceCodeIfAbsent: [ 1 ])
		equals: 1.
]

{ #category : 'test source code' }
MethodReferenceTest >> testSourceCodeIfAbsentReturnsSourceCodeIfExists [

	self 
		assert: ((MethodReference class: self class selector: thisContext selector) sourceCodeIfAbsent: [ self fail ])
		equals: (self class>>thisContext selector) sourceCode.
]

{ #category : 'test source code' }
MethodReferenceTest >> testSourceCodeReturnsSourceCodeIfMethodExist [

	self 
		assert: (MethodReference 
			class: self class 
			selector: thisContext selector) sourceCode
		equals: (self class compiledMethodAt: thisContext selector) sourceCode.
]

{ #category : 'test source code' }
MethodReferenceTest >> testSourceCodeSignalExceptionIfClassDoesNotExist [

	self 
		should: [ (MethodReference classSymbol: #X_Y_Z classIsMeta: false methodSymbol:#xx) sourceCode ]
		raise: Error
		withMessageText: MethodReference classDoesNotExistErrorMessage 
]

{ #category : 'test source code' }
MethodReferenceTest >> testSourceCodeSignalExceptionIfMethodDoesNotExist [

	self 
		should: [ (MethodReference class: self class selector: #xx) sourceCode ]
		raise: Error
		withMessageText: 'key: #xx not found'
]
